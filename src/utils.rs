use std::sync::{Arc, Mutex, OnceLock};
use std::io::{self, BufWriter, Write};
use std::fs::{File, OpenOptions};
use std::path::Path as LogPath;
use std::collections::HashMap;
use chrono::Local;
use rustc_hash::FxHashSet;
use super::*;
use rayon::prelude::*;
use sysinfo::{System, Pid, ProcessesToUpdate};
use std::time::Duration;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

#[cfg(target_os = "macos")]
mod macos_mem {
    use std::mem;
    use libc::{c_int, mach_task_self, task_info, mach_msg_type_number_t, kern_return_t, task_flavor_t};

    const TASK_VM_INFO: task_flavor_t = 22;
    const KERN_SUCCESS: kern_return_t = 0;
    
    #[repr(C)]
    #[derive(Default)]
    struct task_vm_info_data_t {
         virtual_size: u64,
         integer_size: u64,
         resident_size: u64,
         resident_size_peak: u64,
         device: u64,
         device_peak: u64,
         internal: u64,
         internal_peak: u64,
         external: u64,
         external_peak: u64,
         reusable: u64,
         reusable_peak: u64,
         purgeable_volatile_pmap: u64,
         purgeable_volatile_resident: u64,
         purgeable_volatile_virtual: u64,
         compressed: u64,
         compressed_peak: u64,
         compressed_lifetime: u64,
         pub phys_footprint: u64,
         min_address: u64,
         max_address: u64,
         // Depending on SDK version, there might be more fields.
         // We rely on the kernel filling what it knows and the count being passed correctly.
         // To be safe, we can add some padding, but standard practice is often just checking count.
         // Here we assume standard layout.
         _padding: [u64; 10], // Padding for future fields just in case
    }

    pub fn get_phys_footprint_mb() -> u64 {
        unsafe {
            #[allow(deprecated)]
            let task = mach_task_self();
            let mut info: task_vm_info_data_t = mem::zeroed();
            let mut count = (mem::size_of::<task_vm_info_data_t>() / mem::size_of::<i32>()) as mach_msg_type_number_t;
            
            let ret = task_info(
                task,
                TASK_VM_INFO,
                &mut info as *mut task_vm_info_data_t as *mut i32,
                &mut count
            );
            
            if ret == KERN_SUCCESS {
                info.phys_footprint / 1024 / 1024
            } else {
                0
            }
        }
    }
}


#[derive(Clone, Debug)]
pub struct CompressedConcurrentActions {
    pub source_map: Arc<Vec<Arc<RelativizedAction>>>,
    pub valid_masks: Vec<u32>,
}

// ==================== memory management =================

pub struct MemoryGuard {
    max_usage_mb: u64,
    logger: Logger,
    pub max_rss_used: Arc<AtomicU64>,
    pub max_total_used: Arc<AtomicU64>,
    start_time: std::time::Instant,
}

impl MemoryGuard {
    pub fn new(max_usage_mb: u64, logger: Logger) -> Self {
        Self {
            max_usage_mb,
            logger,
            max_rss_used: Arc::new(AtomicU64::new(0)),
            max_total_used: Arc::new(AtomicU64::new(0)),
            start_time: std::time::Instant::now(),
        }
    }

    pub fn start_monitoring(&self) -> Arc<AtomicBool> {
        let should_terminate = Arc::new(AtomicBool::new(false));
        let clone = should_terminate.clone();
        let max_usage = self.max_usage_mb;
        let max_rss_shared = self.max_rss_used.clone();
        let max_total_shared = self.max_total_used.clone();
        let logger = self.logger.clone();
        let start_time = self.start_time;
        
        std::thread::spawn(move || {
            let mut sys = System::new_all();
            //let mut consecutive_warnings = 0;
            
            loop {
                if clone.load(Ordering::Relaxed) {
                    break;
                }
                
                sys.refresh_memory();
                sys.refresh_processes(ProcessesToUpdate::All, true);
                
                // let free_mb = sys.free_memory() / 1024 / 1024;
                // let total_mb = sys.total_memory() / 1024 / 1024;
                
                // On macOS, include swap memory in available memory calculation
                // macOS actively uses swap as part of its memory management
                // #[cfg(target_os = "macos")]
                // let free_swap_mb = sys.free_swap() / 1024 / 1024;
                // #[cfg(target_os = "macos")]
                // let available_mb = free_mb + free_swap_mb;
                
                // #[cfg(not(target_os = "macos"))]
                // let available_mb = free_mb;
                
                let current_pid = sysinfo::get_current_pid().unwrap();
                if let Some(process) = sys.process(Pid::from_u32(current_pid.as_u32())) {
                    let rss_mb = process.memory() / 1024 / 1024;
                    
                    // Definimos o 'total' de forma específica para cada plataforma
                    let total_mb = if cfg!(target_os = "windows") {
                        // No Windows, Virtual Memory é o "Commit Size" (muito informativo)
                        process.virtual_memory() / 1024 / 1024
                    } else if cfg!(target_os = "macos") {
                        // No macOS, usamos o Physical Footprint via libc
                        #[cfg(target_os = "macos")]
                        { 
                            let fp = macos_mem::get_phys_footprint_mb();
                            if fp > 0 { 
                                std::cmp::max(fp, rss_mb) 
                            } else { 
                                rss_mb 
                            }
                        }
                        #[cfg(not(target_os = "macos"))]
                        { rss_mb } // Fallback para outros Unix
                    } else {
                        rss_mb
                    };

                    if rss_mb > max_rss_shared.load(Ordering::Relaxed) {
                        max_rss_shared.store(rss_mb, Ordering::Relaxed);
                    }
                    if total_mb > max_total_shared.load(Ordering::Relaxed) {
                        max_total_shared.store(total_mb, Ordering::Relaxed);
                    }
                    
                    // CRÍTICO: uso excessivo de memória
                    if total_mb > max_usage {
                        let total_label = "Total Memory";
                        let elapsed_ms = start_time.elapsed().as_millis();
                        let msg = format!("🔴 CRITICAL: Memory usage exceeded! RAM: {}MB, {}: {}MB (Limit: {}MB) - Execution Time: {}ms", 
                            rss_mb, total_label, total_mb, max_usage, elapsed_ms);
                        //eprintln!("{}", msg);
                        logger.log(LogType::Necessary, &msg);
                        
                        let msg = "🔴 TERMINATING PROCESS TO PREVENT SYSTEM CRASH";
                        //eprintln!("{}", msg);
                        logger.log(LogType::Necessary, msg);

                        std::process::exit(137);
                    }
                }
                
                // CRÍTICO: sistema sem memória
                // let critical_threshold_mb = (total_mb as f64 * 0.05).max(256.0) as u64;
                
                // if available_mb < critical_threshold_mb {
                //     consecutive_warnings += 1;
                    
                //     #[cfg(target_os = "macos")]
                //     eprintln!("🟡 WARNING {}: Only {}MB available ({}MB RAM + {}MB swap, critical threshold: {}MB)", 
                //         consecutive_warnings, available_mb, free_mb, free_swap_mb, critical_threshold_mb);
                    
                //     #[cfg(not(target_os = "macos"))]
                //     eprintln!("🟡 WARNING {}: Only {}MB free in system (critical threshold: {}MB)", 
                //         consecutive_warnings, available_mb, critical_threshold_mb);
                    
                //     if consecutive_warnings >= 5 {
                //         eprintln!("🔴 CRITICAL: System memory critically low");
                //         eprintln!("🔴 TERMINATING PROCESS TO PREVENT SYSTEM CRASH");
                //         std::process::exit(137);
                //     }
                // } else {
                //     consecutive_warnings = 0;
                // }
                
                // Log periódico a cada ~500MB
                // if process_mb > 0 && process_mb % 500 < 50 {
                //     #[cfg(target_os = "macos")]
                //     println!("📊 Memory: Process={}MB, Available={}MB (RAM: {}MB + Swap: {}MB), Total={}MB", 
                //         process_mb, available_mb, free_mb, free_swap_mb, total_mb);
                    
                //     #[cfg(not(target_os = "macos"))]
                //     println!("📊 Memory: Process={}MB, Free={}MB, Total={}MB", 
                //         process_mb, available_mb, total_mb);
                // }
                
                std::thread::sleep(Duration::from_millis(100));
            }
        });
        
        should_terminate
    }
}

// ==================== symbol_type.rs ====================
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolType {
    Action,
    Individual,
}

impl std::fmt::Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymbolType::Action => write!(f, "action"),
            SymbolType::Individual => write!(f, "individual"),
        }
    }
}

// ==================== symbol.rs ====================
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub id: i32,
    pub value: String,
    pub symbol_type: SymbolType,
}

impl Symbol {
    pub fn new(id: i32, value: String, symbol_type: SymbolType) -> Self {
        Symbol {
            id,
            value,
            symbol_type,
        }
    }

    pub fn id(&self) -> i32 {
        self.id
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn symbol_type(&self) -> SymbolType {
        self.symbol_type
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} | {} | {}\n", self.id, self.value, self.symbol_type)
    }
}

// ==================== symbol_table.rs ====================


static INSTANCE: OnceLock<Arc<Mutex<SymbolTable>>> = OnceLock::new();

#[derive(Debug, Clone)]
pub struct SymbolTable {
    id_base: i32,
    dictionary: Vec<Symbol>,
    lookup: HashMap<(String, SymbolType), i32>,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            id_base: 1,
            dictionary: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    pub fn instance() -> Arc<Mutex<SymbolTable>> {
        INSTANCE.get_or_init(|| {
            Arc::new(Mutex::new(SymbolTable::new()))
        }).clone()
    }

    pub fn add_symbol(&mut self, value: String, symbol_type: SymbolType) -> i32 {
        if let Some(&id) = self.lookup.get(&(value.clone(), symbol_type)) {
            return id;
        }

        let id = self.id_base;
        self.id_base += 1;
        let symbol = Symbol::new(id, value.clone(), symbol_type);
        self.dictionary.push(symbol);
        self.lookup.insert((value, symbol_type), id);
        id
    }

    pub fn get_symbol_by_id(&self, id: i32) -> Option<&Symbol> {
        self.dictionary.iter().find(|s| s.id == id)
    }

    pub fn get_dictionary(&self) -> &[Symbol] {
        &self.dictionary
    }

    pub fn get_actions(&self) -> Vec<&Symbol> {
        self.dictionary.iter()
            .filter(|s| s.symbol_type == SymbolType::Action)
            .collect()
    }

    pub fn get_individuals(&self) -> Vec<&Symbol> {
        self.dictionary.iter()
            .filter(|s| s.symbol_type == SymbolType::Individual)
            .collect()
    }

    pub fn clear(&mut self) {
        self.id_base = 1;
        self.dictionary.clear();
        self.lookup.clear();
    }

    pub fn len(&self) -> usize {
        self.dictionary.len()
    }

    pub fn is_empty(&self) -> bool {
        self.dictionary.is_empty()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Table of Symbols")?;
        for symbol in &self.dictionary {
            writeln!(f, "({})  {:>10}  {}", 
                symbol.id, 
                format!("{}", symbol.symbol_type), 
                symbol.value
            )?;
        }
        Ok(())
    }
}

// ==================== log_level.rs ====================
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogLevel {
    Normal,
    Verbose,
}

// ==================== log_type.rs ====================
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogType {
    Minimal,
    Necessary,
    Additional,
}

// ==================== run_configuration.rs ====================


#[derive(Debug, Clone)]
pub struct RunConfiguration {
    contract_file_name: String,
    result_file_name: String,
    export_decompositions: bool,
    export_automaton: bool,
    continue_on_conflict: bool,
    export_min_automaton: bool,
    use_prunning: bool,
    decompositions_file_name: String,
    automaton_file_name: String,
    min_automaton_file_name: String,
    log_level: LogLevel,
    global_log_filename: String,
    test: bool,
}

impl RunConfiguration {
    pub fn new() -> Self {
        Self {
            contract_file_name: String::new(),
            result_file_name: String::new(),
            export_decompositions: false,
            export_automaton: false,
            continue_on_conflict: false,
            export_min_automaton: false,
            use_prunning: true,
            decompositions_file_name: String::new(),
            automaton_file_name: String::new(),
            min_automaton_file_name: String::new(),
            log_level: LogLevel::Normal,
            global_log_filename: String::new(),
            test: false,
        }
    }

    // Getters
    pub fn contract_file_name(&self) -> &str { &self.contract_file_name }
    pub fn result_file_name(&self) -> &str { &self.result_file_name }
    pub fn is_export_decompositions(&self) -> bool { self.export_decompositions }
    pub fn is_export_automaton(&self) -> bool { self.export_automaton }
    pub fn is_continue_on_conflict(&self) -> bool { self.continue_on_conflict }
    pub fn is_export_min_automaton(&self) -> bool { self.export_min_automaton }
    pub fn is_use_prunning(&self) -> bool { self.use_prunning }
    pub fn is_test(&self) -> bool { self.test }
    pub fn decompositions_file_name(&self) -> &str { &self.decompositions_file_name }
    pub fn automaton_file_name(&self) -> &str { &self.automaton_file_name }
    pub fn min_automaton_file_name(&self) -> &str { &self.min_automaton_file_name }
    pub fn log_level(&self) -> LogLevel { self.log_level }
    pub fn global_log_filename(&self) -> &str { &self.global_log_filename }

    // Setters
    pub fn set_contract_file_name(&mut self, name: String) { self.contract_file_name = name; }
    pub fn set_result_file_name(&mut self, name: String) { self.result_file_name = name; }
    pub fn set_export_decompositions(&mut self, value: bool) { self.export_decompositions = value; }
    pub fn set_export_automaton(&mut self, value: bool) { self.export_automaton = value; }
    pub fn set_continue_on_conflict(&mut self, value: bool) { self.continue_on_conflict = value; }
    pub fn set_export_min_automaton(&mut self, value: bool) { self.export_min_automaton = value; }
    pub fn set_use_prunning(&mut self, value: bool) { self.use_prunning = value; }
    pub fn set_log_level(&mut self, level: LogLevel) { self.log_level = level; }
    pub fn set_global_log_filename(&mut self, name: String) { self.global_log_filename = name; }
    pub fn set_automaton_file_name(&mut self, name: String) { self.automaton_file_name = name; }
    pub fn set_min_automaton_file_name(&mut self, name: String) { self.min_automaton_file_name = name; }
    pub fn set_decompositions_file_name(&mut self, name: String) { self.decompositions_file_name = name; }
    pub fn set_test(&mut self, value: bool) { self.test = value; }
}

impl Default for RunConfiguration {
    fn default() -> Self {
        Self::new()
    }
}

// ==================== file_util.rs ====================

pub struct FileUtil;

impl FileUtil {
    pub fn write_to_file(filename: &str, lines: &[&str]) -> io::Result<bool> {
        let file = File::create(filename)?;
        let mut writer = BufWriter::new(file);

        for line in lines {
            writeln!(writer, "{}", line)?;
        }

        writer.flush()?;
        Ok(true)
    }
}

// ==================== logger.rs ====================

#[derive(Clone)]
pub struct Logger {
    level: LogLevel,
    configuration: RunConfiguration,
    global_log_filename: String,
    bw_global: Arc<Mutex<Option<BufWriter<File>>>>,
    bw_local: Arc<Mutex<Option<BufWriter<File>>>>,
    contract_name: String,
}

impl Logger {
    pub fn new(configuration: RunConfiguration) -> std::io::Result<Self> {
        let global_log_filename = configuration.global_log_filename().to_string();
        let contract_name = LogPath::new(configuration.contract_file_name())
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("contract")
            .to_string();

        let global_file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&global_log_filename)?;
        
        let local_file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(configuration.result_file_name())?;

        Ok(Self {
            level: configuration.log_level(),
            configuration: configuration.clone(),
            global_log_filename,
            bw_global: Arc::new(Mutex::new(Some(BufWriter::new(global_file)))),
            bw_local: Arc::new(Mutex::new(Some(BufWriter::new(local_file)))),
            contract_name,
        })
    }

    pub fn log(&self, log_type: LogType, text: &str) {
        let date_info = self.get_date_info();
        let formatted_text = self.format(text);

        match self.level {
            LogLevel::Normal => {
                match log_type {
                    LogType::Minimal => {
                        let msg = format!("{} [{}]: {}", date_info, self.contract_name, formatted_text);
                        self.write_global(&msg);
                        println!("{}", formatted_text);
                    }
                    LogType::Necessary => {
                        let msg = format!("{} [{}]: {}", date_info, self.contract_name, formatted_text);
                        self.write_global(&msg);
                    }
                    LogType::Additional => {}
                }
                let local_msg = format!("{}: {}", date_info, formatted_text);
                self.write_local(&local_msg);
            }
            LogLevel::Verbose => {
                let local_msg = format!("{}: {}", date_info, formatted_text);
                let global_msg = format!("{} [{}]: {}", date_info, self.contract_name, formatted_text);
                self.write_local(&local_msg);
                self.write_global(&global_msg);
                println!("{}", formatted_text);
            }
        }
    }

    fn write_global(&self, line: &str) {
        if let Ok(mut lock) = self.bw_global.lock() {
            if let Some(ref mut writer) = *lock {
                if writeln!(writer, "{}", line).is_err() {
                    eprintln!("Failed to write to global log file: {}", self.global_log_filename);
                    println!("{}", line);
                }
                let _ = writer.flush();
            }
        }
    }

    fn write_local(&self, line: &str) {
        if let Ok(mut lock) = self.bw_local.lock() {
            if let Some(ref mut writer) = *lock {
                if writeln!(writer, "{}", line).is_err() {
                    eprintln!("Failed to write to local log file: {}", self.configuration.result_file_name());
                    println!("{}", line);
                }
                let _ = writer.flush();
            }
        }
    }

    fn get_date_info(&self) -> String {
        Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
    }

    fn format(&self, text: &str) -> String {
        let re = regex::Regex::new(r"\x1B\[[0-9]+m").unwrap();
        re.replace_all(text, "").to_string()
    }

    // pub fn close(&self) -> std::io::Result<()> {
    //     if let Ok(mut lock) = self.bw_global.lock() {
    //         if let Some(mut writer) = lock.take() {
    //             writer.flush()?;
    //         }
    //     }
    //     if let Ok(mut lock) = self.bw_local.lock() {
    //         if let Some(mut writer) = lock.take() {
    //             writer.flush()?;
    //         }
    //     }
    //     Ok(())
    // }
}

// ==================== contract_util.rs ====================
pub struct ContractUtil;

impl ContractUtil {
    pub fn is_valid(
        actions: &FxHashSet<Arc<RelativizedAction>>,
        conflicts: &[Conflict],
    ) -> bool {
        if actions.is_empty() {
            return false;
        }

        for conflict in conflicts {
            let count_a = actions.iter()
                .filter(|ra| ra.action.value == conflict.a.value)
                .count();

            if count_a == 0 { continue; }

            let count_b = actions.iter()
                .filter(|ra| ra.action.value == conflict.b.value)
                .count();

            if count_b == 0 { continue; }

            if conflict.conflict_type == ConflictType::Global {
                return false;
            }

            if conflict.conflict_type == ConflictType::Relativized {
                let has_relativized_conflict = actions.iter()
                    .filter(|ra| ra.action.value == conflict.a.value)
                    .any(|ra_a| {
                        actions.iter().any(|ra_b| 
                            ra_b.action.value == conflict.b.value && 
                            ra_b.sender == ra_a.sender
                        )
                    });
                
                if has_relativized_conflict {
                    return false;
                }
            }
        }
        true
    }

    /// Ordena ações por tamanho (decrescente)
    // pub fn reverse(actions: &mut Vec<FxHashSet<RelativizedAction>>) {
    //     actions.sort_by(|a, b| b.len().cmp(&a.len()));
    // }

    /// Calcula todas as ações relativizadas concorrentes considerando conflitos
    pub fn calculate_concurrent_relativized_actions(
        relativized_actions: FxHashSet<Arc<RelativizedAction>>,
        conflicts: &[Conflict],
        _config: &RunConfiguration,
        logger: &mut Logger
    ) -> CompressedConcurrentActions {
        let current_time = std::time::Instant::now();
        
        let n = relativized_actions.len();

        if n > 30 {
            let msg = format!("CRITICAL: Can't calculate the set of concurrent relativized actions. (Number of actions {}). Maximum supported is 30.", n);
            logger.log(LogType::Necessary, &msg);
            panic!("{}", msg);
        }

        // Pre-allocation check: use try_reserve on a temporary Vec to see if the OS allows this memory.
        // This triggers a catchable panic instead of a hard process abort (OOM).
        let size: u64 = 1u64 << n; // 2^n
        let mut check_vec: Vec<u32> = Vec::new();
        if let Err(_) = check_vec.try_reserve(size as usize) {
            let bytes = size * 4;
            let gb = bytes as f64 / (1024.0 * 1024.0 * 1024.0);
            let msg = format!("CRITICAL: Memory allocation of approx {:.2} GB failed for {} concurrent action combinations.", gb, size);
            logger.log(LogType::Necessary, &msg);
            panic!("{}", msg);
        }

        let src: Vec<Arc<RelativizedAction>> = relativized_actions.into_iter().collect();
        let src_arc = Arc::new(src);

        let size: u64 = 1u64 << n; // 2^n

        let mut valid_masks: Vec<u32> = (0..size)
            .into_par_iter()
            .filter_map(|mask| {
                let mut temp_set = FxHashSet::default();
                let mut temp_mask = mask as u32;
                
                while temp_mask > 0 {
                    let idx = temp_mask.trailing_zeros();
                    if let Some(act) = src_arc.get(idx as usize) {
                        temp_set.insert(act.clone());
                    }
                    temp_mask &= temp_mask - 1;
                }

                if Self::is_valid(&temp_set, conflicts) {
                    Some(mask as u32)
                } else {
                    None
                }
            })
            .collect();
        
        valid_masks.sort_by(|a, b| b.count_ones().cmp(&a.count_ones()));

        // Check memory usage
        let mut sys = System::new_all();
        sys.refresh_all();
        let pid = sysinfo::get_current_pid().unwrap();
        let process_mb = if let Some(process) = sys.process(pid) {
            process.memory() / 1024 / 1024
        } else {
            0
        };

        let elapsed = current_time.elapsed();

        logger.log(LogType::Necessary, &format!("Calculated {} concurrent actions in {:?} (Memory: {}MB)", 
                valid_masks.len(), elapsed, process_mb));

        CompressedConcurrentActions {
            source_map: src_arc,
            valid_masks,
        }
    }
}

// ==================== automaton_exporter.rs ====================

pub struct AutomatonExporter;

impl AutomatonExporter {
    pub fn dump_states(automaton: &Automaton) -> String {
        let mut output = String::from("id;clause;situation\n");
        
        let mut states: Vec<_> = automaton.states.iter().collect();
        states.sort_by_key(|s| s.id);
        
        for state in states {
            let clause_str = if let Some(ref clause) = state.clause {
                format!("{}", clause)
            } else {
                String::from("")
            };
            
            let situation_str = match state.situation {
                StateSituation::Violating => "violating",
                StateSituation::Satisfaction => "satisfaction",
                StateSituation::Conflicting => "conflicting",
                StateSituation::ConflictFree => "conflictFree",
                StateSituation::NotChecked => "notChecked",
            };
            
            output.push_str(&format!("{};{};{}\n", 
                state.id, 
                clause_str, 
                situation_str
            ));
        }
        
        output
    }

    pub fn dump_to_dot(automaton: &Automaton) -> String {
        let mut output = String::from("digraph contract {\nrankdir=LR;\n");
        
        output.push_str("node [shape = point, color=white, fontcolor=white]; start;\n");
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::NotChecked 
                || state.situation == StateSituation::ConflictFree {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=black, fontcolor=black, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""), 
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Violating {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=red, fontcolor=white, style=filled, fillcolor=red, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Satisfaction {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=green, fontcolor=white, style=filled, fillcolor=green, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Conflicting {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=orange, fontcolor=white, style=filled, fillcolor=orange, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        if let Some(ref initial) = automaton.initial {
            output.push_str(&format!("start -> S{}\n", initial.id));
        }
        
        {
            let symbol_table = SymbolTable::instance();
            let table = symbol_table.lock().unwrap();
            
            for transition in automaton.transitions.iter() {
                let actions_vec = transition.actions();
                let actions_str = Self::format_actions(&actions_vec, &table);
                output.push_str(&format!(
                    "\tS{} -> S{} [ label = \"{}\" ];\n",
                    transition.from,
                    transition.to,
                    actions_str.replace("\"", "\\\"")
                ));
            }
        }
        
        output.push_str("}\n");
        output
    }

    pub fn dump_to_text(automaton: &Automaton) -> String {
        let mut output = String::new();
        
        // Obter symbol table
        let symbol_table = SymbolTable::instance();
        let table = symbol_table.lock().unwrap();
        
        // A: Ações
        let actions: Vec<String> = table.get_actions()
            .iter()
            .map(|s| s.value.clone())
            .collect();
        output.push_str(&format!("A:{}\n", actions.join(";")));
        
        // I: Indivíduos
        let individuals: Vec<String> = table.get_individuals()
            .iter()
            .map(|s| s.value.clone())
            .collect();
        output.push_str(&format!("I:{}\n", individuals.join(";")));
        
        // Q: Estados
        let mut states_ids: Vec<usize> = automaton.states.iter().map(|s| s.id).collect();
        states_ids.sort();
        let states_str: Vec<String> = states_ids.iter().map(|id| id.to_string()).collect();
        output.push_str(&format!("Q:{}\n", states_str.join(";")));
        
        // V: Estados violating
        let mut violations: Vec<usize> = automaton.states.iter()
            .filter(|s| s.situation == StateSituation::Violating)
            .map(|s| s.id)
            .collect();
        violations.sort();
        let violations_str: Vec<String> = violations.iter().map(|id| id.to_string()).collect();
        output.push_str(&format!("V:{}\n", violations_str.join(";")));
        
        // S: Estados satisfaction
        let mut satisfactions: Vec<usize> = automaton.states.iter()
            .filter(|s| s.situation == StateSituation::Satisfaction)
            .map(|s| s.id)
            .collect();
        satisfactions.sort();
        let satisfactions_str: Vec<String> = satisfactions.iter().map(|id| id.to_string()).collect();
        output.push_str(&format!("S:{}\n", satisfactions_str.join(";")));
        
        // T: Transições
        let mut transitions_strs = Vec::new();
        for transition in automaton.transitions.iter() {
            let mut actions_parts = Vec::new();
            let transition_actions = transition.actions();
            
            for ra in transition_actions.iter() {
                let sender_name = table.get_symbol_by_id(ra.sender)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                
                let action_name = table.get_symbol_by_id(ra.action.value)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                
                let receiver_name = table.get_symbol_by_id(ra.receiver)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                
                actions_parts.push(format!("{}?{}?{}", sender_name, action_name, receiver_name));
            }
            
            let transition_str = format!(
                "{}-{}-{};",
                transition.from,
                actions_parts.join(","),
                transition.to
            );
            transitions_strs.push(transition_str);
        }
        
        output.push_str(&format!("T:{}\n", transitions_strs.join("")));
        
        output
    }

    pub fn dump_to_min_dot(automaton: &Automaton) -> String {
        let mut output = String::from("digraph contract {\nrankdir=LR;\n");
        
        output.push_str("node [shape = point, color=white, fontcolor=white]; start;\n");
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::NotChecked 
                || state.situation == StateSituation::ConflictFree {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=black, fontcolor=black, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""), 
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Violating {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=red, fontcolor=white, style=filled, fillcolor=red, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Satisfaction {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=green, fontcolor=white, style=filled, fillcolor=green, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        for state in automaton.states.iter() {
            if state.situation == StateSituation::Conflicting {
                let tooltip = if let Some(ref clause) = state.clause {
                    format!("{}", clause)
                } else {
                    String::from("")
                };
                output.push_str(&format!(
                    "node [shape = circle, color=orange, fontcolor=white, style=filled, fillcolor=orange, tooltip=\"{}\"]; S{} ;\n",
                    tooltip.replace("\"", "\\\""),
                    state.id
                ));
            }
        }
        
        if let Some(ref initial) = automaton.initial {
            output.push_str(&format!("start -> S{}\n", initial.id));
        }
        
        let mut transition_map: HashMap<(usize, usize), Vec<std::sync::Arc<RelativizedAction>>> = HashMap::new();
        
        for transition in automaton.transitions.iter() {
            let key = (transition.from, transition.to);
            
            transition_map.entry(key)
                .and_modify(|existing_actions| {
                    if transition.actions().len() > existing_actions.len() {
                        *existing_actions = transition.actions().clone();
                    }
                })
                .or_insert_with(|| transition.actions().clone());
        }
        
        let mut sorted_transitions: Vec<_> = transition_map.into_iter().collect();
        sorted_transitions.sort_by_key(|(k, _)| *k);
        
        {
            let symbol_table = SymbolTable::instance();
            let table = symbol_table.lock().unwrap();
            
            for ((from, to), actions) in sorted_transitions {
                let actions_str = Self::format_actions(&actions, &table);
                output.push_str(&format!(
                    "\tS{} -> S{} [ label = \"{}\" ];\n",
                    from,
                    to,
                    actions_str.replace("\"", "\\\"")
                ));
            }
        }
        
        output.push_str("}\n");
        output
    }
    
    // ==================== Funções auxiliares ====================
    
    fn format_actions(
        actions: &[std::sync::Arc<RelativizedAction>], 
        symbol_table: &SymbolTable
    ) -> String {
        if actions.is_empty() {
            return String::from("ε");
        }
        
        let formatted: Vec<String> = actions.iter()
            .map(|ra| ra.format_with_symbols(symbol_table))
            .collect();
        
        formatted.join(", ")
    }
}
