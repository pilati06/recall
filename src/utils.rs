use std::sync::{Arc, Mutex, OnceLock};
use std::io::{self, BufWriter, Write};
use std::fs::{File, OpenOptions};
use std::path::Path as LogPath;
use std::collections::HashMap;
use chrono::Local;
use rustc_hash::FxHashSet;
use super::*;
use rayon::prelude::*;

#[derive(Clone, Debug)]
pub struct CompressedConcurrentActions {
    pub source_map: Vec<Arc<RelativizedAction>>,
    pub valid_masks: Vec<u32>,
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
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            id_base: 1,
            dictionary: Vec::new(),
        }
    }

    pub fn instance() -> Arc<Mutex<SymbolTable>> {
        INSTANCE.get_or_init(|| {
            Arc::new(Mutex::new(SymbolTable::new()))
        }).clone()
    }

    pub fn add_symbol(&mut self, value: String, symbol_type: SymbolType) -> i32 {
        if let Some(symbol) = self.dictionary.iter()
            .find(|s| s.symbol_type == symbol_type && s.value == value) {
            return symbol.id;
        }

        let id = self.id_base;
        self.id_base += 1;
        let symbol = Symbol::new(id, value, symbol_type);
        self.dictionary.push(symbol);
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
    log_level: LogLevel,
    global_log_filename: String,
    //test: bool,
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
            log_level: LogLevel::Normal,
            global_log_filename: String::new(),
            //test: false,
        }
    }

    // Getters
    pub fn contract_file_name(&self) -> &str { &self.contract_file_name }
    pub fn result_file_name(&self) -> &str { &self.result_file_name }
    pub fn is_export_decompositions(&self) -> bool { self.export_decompositions }
    pub fn is_export_automaton(&self) -> bool { self.export_automaton }
    pub fn is_continue_on_conflict(&self) -> bool { self.continue_on_conflict }
    pub fn is_export_min_automaton(&self) -> bool { self.export_min_automaton }
    //pub fn is_use_prunning(&self) -> bool { self.use_prunning }
    pub fn decompositions_file_name(&self) -> &str { &self.decompositions_file_name }
    pub fn automaton_file_name(&self) -> &str { &self.automaton_file_name }
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
    pub fn set_decompositions_file_name(&mut self, name: String) { self.decompositions_file_name = name; }
    //pub fn set_test(&mut self, value: bool) { self.test = value; }
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

pub struct Logger {
    level: LogLevel,
    configuration: RunConfiguration,
    global_log_filename: String,
    bw_global: Option<BufWriter<File>>,
    bw_local: Option<BufWriter<File>>,
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
            bw_global: Some(BufWriter::new(global_file)),
            bw_local: Some(BufWriter::new(local_file)),
            contract_name,
        })
    }

    pub fn log(&mut self, log_type: LogType, text: &str) {
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

    fn write_global(&mut self, line: &str) {
        if let Some(ref mut writer) = self.bw_global {
            if writeln!(writer, "{}", line).is_err() {
                eprintln!("Failed to write to global log file: {}", self.global_log_filename);
                println!("{}", line);
            }
            let _ = writer.flush();
        }
    }

    fn write_local(&mut self, line: &str) {
        if let Some(ref mut writer) = self.bw_local {
            if writeln!(writer, "{}", line).is_err() {
                eprintln!("Failed to write to local log file: {}", self.configuration.result_file_name());
                println!("{}", line);
            }
            let _ = writer.flush();
        }
    }

    fn get_date_info(&self) -> String {
        Local::now().format("%Y-%m-%d %H:%M:%S").to_string()
    }

    fn format(&self, text: &str) -> String {
        let re = regex::Regex::new(r"\x1B\[[0-9]+m").unwrap();
        re.replace_all(text, "").to_string()
    }

    pub fn close(&mut self) -> std::io::Result<()> {
        if let Some(mut writer) = self.bw_global.take() {
            writer.flush()?;
        }
        if let Some(mut writer) = self.bw_local.take() {
            writer.flush()?;
        }
        Ok(())
    }
}

impl Drop for Logger {
    fn drop(&mut self) {
        let _ = self.close();
    }
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
        config: &RunConfiguration,
        logger: &mut Logger
    ) -> CompressedConcurrentActions {
        let current_time = std::time::Instant::now();
        
        let src: Vec<Arc<RelativizedAction>> = relativized_actions.into_iter().collect();
        let n = src.len();

        if n > 32 {
            panic!("Otimização atual suporta no máximo 32 ações relativizadas. Input: {}", n);
        }

        let size: u64 = 1 << n; // 2^n

        let mut valid_masks: Vec<u32> = (0..size)
            .into_par_iter()
            .filter_map(|mask| {
                let mut temp_set = FxHashSet::default();
                let mut temp_mask = mask as u32;
                
                while temp_mask > 0 {
                    let idx = temp_mask.trailing_zeros();
                    if let Some(act) = src.get(idx as usize) {
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

        
        if config.log_level() == LogLevel::Verbose {
            let elapsed = current_time.elapsed();
            logger.log(LogType::Necessary, &format!("Calculated {} concurrent actions in {:?}", 
                    valid_masks.len(), elapsed));
        }

        CompressedConcurrentActions {
            source_map: src,
            valid_masks,
        }
    }
}

// ==================== automaton_exporter.rs ====================

pub struct AutomatonExporter;

impl AutomatonExporter {
    /// Exporta estados do autômato em formato CSV
    /// Formato: id;clause;situation
    pub fn dump_states(automaton: &Automaton) -> String {
        let mut output = String::from("id;clause;situation\n");
        
        // Coleta e ordena os estados por ID
        let mut states: Vec<_> = automaton.states.iter().collect();
        states.sort_by_key(|s| s.id);
        
        // Adiciona cada estado ao output
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

    /// Exporta autômato para formato DOT (Graphviz)
    /// Inclui todas as transições sem merge
    pub fn dump_to_dot(automaton: &Automaton) -> String {
        let mut output = String::from("digraph contract {\nrankdir=LR;\n");
        
        // Nó inicial invisível
        output.push_str("node [shape = point, color=white, fontcolor=white]; start;\n");
        
        // Estados notChecked e conflictFree (círculos pretos)
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
        
        // Estados violating (círculos vermelhos preenchidos)
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
        
        // Estados satisfaction (círculos verdes preenchidos)
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
        
        // Estados conflicting (círculos laranjas preenchidos)
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
        
        // Transição do estado inicial
        if let Some(ref initial) = automaton.initial {
            output.push_str(&format!("start -> S{}\n", initial.id));
        }
        
        // Todas as transições - obter symbol table apenas para esta parte
        {
            let symbol_table = SymbolTable::instance();
            let table = symbol_table.lock().unwrap();
            
            for transition in automaton.transitions.iter() {
                let actions_str = Self::format_actions(&transition.actions, &table);
                output.push_str(&format!(
                    "\tS{} -> S{} [ label = \"{}\" ];\n",
                    transition.from,
                    transition.to,
                    actions_str.replace("\"", "\\\"")
                ));
            }
        } // Lock é liberado aqui
        
        output.push_str("}\n");
        output
    }

    /// Exporta autômato para formato texto personalizado
    /// Formato compatível com o Java original
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
            
            for ra in transition.actions.iter() {
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

    /// Exporta autômato minimizado para DOT
    /// Faz merge de transições duplicadas entre os mesmos estados, mantendo a com mais ações
    pub fn dump_to_min_dot(automaton: &Automaton) -> String {
        let mut output = String::from("digraph contract {\nrankdir=LR;\n");
        
        // Nó inicial invisível
        output.push_str("node [shape = point, color=white, fontcolor=white]; start;\n");
        
        // Estados notChecked e conflictFree (círculos pretos)
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
        
        // Estados violating (círculos vermelhos preenchidos)
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
        
        // Estados satisfaction (círculos verdes preenchidos)
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
        
        // Estados conflicting (círculos laranjas preenchidos)
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
        
        // Transição do estado inicial
        if let Some(ref initial) = automaton.initial {
            output.push_str(&format!("start -> S{}\n", initial.id));
        }
        
        // Merge de transições: manter apenas a com mais ações para cada par (from, to)
        let mut transition_map: HashMap<(usize, usize), Vec<std::sync::Arc<RelativizedAction>>> = HashMap::new();
        
        for transition in automaton.transitions.iter() {
            let key = (transition.from, transition.to);
            
            transition_map.entry(key)
                .and_modify(|existing_actions| {
                    // Se a transição atual tem mais ações, substitui
                    if transition.actions.len() > existing_actions.len() {
                        *existing_actions = transition.actions.clone();
                    }
                })
                .or_insert_with(|| transition.actions.clone());
        }
        
        // Ordenar as transições para saída consistente
        let mut sorted_transitions: Vec<_> = transition_map.into_iter().collect();
        sorted_transitions.sort_by_key(|(k, _)| *k);
        
        // Escrever transições mescladas - obter lock apenas aqui
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
        } // Lock é liberado aqui
        
        output.push_str("}\n");
        output
    }
    
    // ==================== Funções auxiliares ====================
    
    /// Formata uma lista de RelativizedActions para exibição
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
