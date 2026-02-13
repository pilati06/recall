mod model;
mod algorithms;
mod utils;
pub mod parser;

use model::contracts::*;
use model::automata::*;
use model::actions::*;
use algorithms::clause_decomposer::*;
use algorithms::conflict_searcher::*;
use algorithms::action_extractor::*;
use algorithms::automata_constructor::*;
use utils::*;
use console_util::ConsoleColors;
use pest::Parser;
//use rayon::ThreadPoolBuilder;

use parser::{
    RCLParser,
    Rule,
    build_ast
};

use std::time::Instant;
use std::path::Path;
use std::fs;
use std::sync::atomic::Ordering;

fn main() {
    // ThreadPoolBuilder::new()
    //     .num_threads(5)
    //     .build_global()
    //     .unwrap();

    let args: Vec<String> = std::env::args().collect();
    let config = parse_command_line(&args);
    let logger = match Logger::new(config.clone()) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to initialize logger: {}", e);
            std::process::exit(1);
        }
    };

    if config.is_test() {
        // Custom panic hook to silence "CRITICAL:" panics only during tests
        let default_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let payload = info.payload();
            let msg = if let Some(s) = payload.downcast_ref::<&str>() {
                Some((*s).to_string())
            } else if let Some(s) = payload.downcast_ref::<String>() {
                Some(s.clone())
            } else {
                None
            };

            if let Some(m) = msg {
                if m.starts_with("CRITICAL:") {
                    return;
                }
            }
            default_hook(info);
        }));

        if let Err(e) = run_tests() {
            println!(";;;;;;;;;{}", e);
        }
        return;
    }

    let (total_ram_mb, total_swap_mb) = get_system_memory_info();
    let max_process_mb = calculate_safe_memory_limit(total_ram_mb, total_swap_mb);
    
    let memory_guard = MemoryGuard::new(max_process_mb, logger.clone());
    let _guard_handle = memory_guard.start_monitoring();
    
    logger.log(LogType::Additional, "Memory guard active:");
    logger.log(LogType::Additional, &format!("   - System RAM: {}MB", total_ram_mb));
    logger.log(LogType::Additional, &format!("   - System Swap: {}MB", total_swap_mb));
    logger.log(LogType::Additional, &format!("   - Process limit: {}MB ({:.0}% of Total RAM + Swap)", 
                max_process_mb,
                (max_process_mb as f64 / (total_ram_mb + total_swap_mb) as f64) * 100.0));

    logger.log(LogType::Additional, &format!("Using {:?}", config));
    logger.log(
        LogType::Minimal,
        &format!("Analysing contract in {}", config.contract_file_name()),
    );

    let start = Instant::now();
    
    let analysis_logger = logger.clone();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut local_logger = analysis_logger;
        run_analysis(config, &mut local_logger, &memory_guard)
    }));

    match result {
        Ok(analysis_result) => {
            match analysis_result {
                Ok(_) => {
                    logger.log(LogType::Minimal, "Analysis completed successfully");
                }
                Err(e) => {
                    logger.log(LogType::Minimal, "A fatal error occurred");
                    logger.log(LogType::Necessary, &format!("Error: {:?}", e));
                    
                    if e.to_string().contains("parse") || e.to_string().contains("syntax") {
                        logger.log(
                            LogType::Minimal,
                            "Malformed Contract, please check the syntax.",
                        );
                    }
                }
            }
        }
        Err(payload) => {
            let msg = if let Some(s) = payload.downcast_ref::<&str>() {
                format!("{}", s)
            } else if let Some(s) = payload.downcast_ref::<String>() {
                format!("{}", s)
            } else {
                String::from("Unknown panic payload")
            };

            let panic_msg = format!("🔴 CRITICAL UNHANDLED EXCEPTION (PANIC) CAUGHT IN MAIN: {} (execution time: {}ms)", msg, start.elapsed().as_millis() as u64);
            eprintln!("\n{}", panic_msg);
            logger.log(LogType::Necessary, &panic_msg);
        }
    }
}

fn get_system_memory_info() -> (u64, u64) {
    use sysinfo::{System};
    
    let mut sys = System::new_all();
    sys.refresh_memory();
    let ram = sys.total_memory() / 1024 / 1024;
    let swap = sys.total_swap() / 1024 / 1024;
    (ram, swap)
}

fn calculate_safe_memory_limit(total_ram_mb: u64, total_swap_mb: u64) -> u64 {
    // Estratégia adaptativa: usar RAM + Swap como base
    let total_available = total_ram_mb + total_swap_mb;
    
    let percentage = if total_available >= 32 * 1024 {
        0.95 // Sistemas grandes
    } else if total_available >= 16 * 1024 {
        0.90 
    } else {
        0.85 
    };
    
    let limit = (total_available as f64 * percentage) as u64;
    
    // Garantir que sobra algo para o SO (pelo menos 2GB de buffer se possível)
    if total_available > 4096 {
        limit.min(total_available - 2048)
    } else {
        limit.max(1024) // Fallback para sistemas muito pequenos
    }
}

fn run_analysis(
    config: RunConfiguration,
    logger: &mut Logger,
    memory_guard: &MemoryGuard,
) -> Result<(), Box<dyn std::error::Error>> {

    let input_string = fs::read_to_string(config.contract_file_name())
        .map_err(|e| format!("Erro ao ler o ficheiro: {}", e))?;

    let mut pairs = RCLParser::parse(Rule::main, &input_string)
        .map_err(|e| format!("Erro de sintaxe: \n{}", e))?;

    let main_pair = pairs.next().unwrap();
    
    // Carrega o contrato
    let contract: Contract = build_ast(main_pair)
        .map_err(|e| format!("Erro ao construir AST: \n{}", e))?;

    logger.log(LogType::Necessary, &format!("Loaded Contract: {}", contract));
    
    // Log da tabela de símbolos
    let symbol_table = SymbolTable::instance();
    let table = symbol_table.lock().unwrap();

    logger.log(LogType::Additional, &format!("{}", *table));
    drop(table); 

    logger.log(LogType::Minimal, "Processing contract...");

    // Processa o contrato
    let start = Instant::now();
    let mut constructor = AutomataConstructor::new(config.clone());
    let automaton = constructor.process(contract, logger);
    let elapsed = start.elapsed();

    // Imprime resultado
    let result = print_result(
        &automaton, 
        elapsed.as_millis() as u64, 
        memory_guard.max_rss_used.load(Ordering::Relaxed),
        memory_guard.max_total_used.load(Ordering::Relaxed)
    );
    logger.log(LogType::Minimal, &result);

    // Exporta decomposições
    if config.is_export_decompositions() {
        logger.log(
            LogType::Minimal,
            &format!(
                "Exporting Decompositions to {}",
                config.decompositions_file_name()
            ),
        );
        let states_dump = AutomatonExporter::dump_states(&automaton);
        FileUtil::write_to_file(config.decompositions_file_name(), &[&states_dump])?;
    }

    // Exporta autômato
    if config.is_export_automaton() {

        // Exporta versão minimizada se configurado
        if config.is_export_min_automaton() {
            logger.log(
                LogType::Minimal,
                &format!("Exporting Automaton to {}", config.min_automaton_file_name()),
            );
            let min_dot = AutomatonExporter::dump_to_min_dot(&automaton);
            let min_filename = format!("{}", config.min_automaton_file_name());
            FileUtil::write_to_file(&min_filename, &[&min_dot])?;
        } else {
            logger.log(
                LogType::Minimal,
                &format!("Exporting Automaton to {}", config.automaton_file_name()),
            );
            // Exporta versão completa
            let dot = AutomatonExporter::dump_to_dot(&automaton);
            FileUtil::write_to_file(config.automaton_file_name(), &[&dot])?;

            // Exporta versão texto
            let text = AutomatonExporter::dump_to_text(&automaton);
            let text_filename = format!("{}.txt", config.automaton_file_name());
            FileUtil::write_to_file(&text_filename, &[&text])?;
        }
    }

    std::thread::spawn(move || {
        drop(automaton);
    });

    Ok(())
}

fn print_trace(automaton: &Automaton) -> String {
    let mut output = String::new();
    output.push_str("\n-------------------------------------------------------\n");

    let conflicts = automaton.get_conflicts();
    
    for state in conflicts {
        output.push_str(&format!("Conflict found in state (s{})\n", state.id));
        
        if let Some(ref conflict_info) = state.conflict_information {
            output.push_str(&format!("Conflict: {}\n", conflict_info));
        }
        
        output.push_str("-------------------------------------------------------\n");
        
        let mut trace_summary = String::from("Trace: ");
        let mut trace_details = String::from("Stacktrace: \n");
        
        let mut current_state = state.clone();
        
        while !current_state.trace.is_empty() {
            if let Some(&transition_id) = current_state.trace.first() {
                if let Some(transition) = automaton.get_transition_by_id(transition_id) {
                    trace_summary.push_str(&format!("(s{})", transition.to));
                    
                    trace_details.push_str(&format!(
                        "{}(s{}){}",
                        ConsoleColors::FG_YELLOW,
                        transition.to,
                        ConsoleColors::RESET
                    ));
                    
                    if let Some(to_state) = automaton.get_state_by_id(transition.to) {
                        trace_details.push_str(&format!(" - {}\n", to_state));
                    }
                    
                    trace_details.push_str(&format!(
                        "{}<T{}> - {}[{}]{}\n",
                        ConsoleColors::FG_RED,
                        transition.id,
                        ConsoleColors::FG_BLUE,
                        transition.actions().iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                        ConsoleColors::RESET
                    ));
                    
                    trace_summary.push_str(&format!("<--T{}--", transition.id));
                    
                    if let Some(from_state) = automaton.get_state_by_id(transition.from) {
                        current_state = from_state.clone();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        
        trace_summary.push_str(&format!("(s{})\n", current_state.id));
        trace_details.push_str(&format!(
            "{}(s{}){}",
            ConsoleColors::FG_YELLOW,
            current_state.id,
            ConsoleColors::RESET
        ));
        
        if let Some(ref clause) = current_state.clause {
            trace_details.push_str(&format!(" - {}\n", clause));
        }
        
        output.push_str(&trace_summary);
        output.push_str("-------------------------------------------------------\n");
        output.push_str(&trace_details);
        output.push_str("-------------------------------------------------------\n");
    }

    output
}

fn print_result(automaton: &Automaton, ms: u64, rss: u64, total: u64) -> String {
    let mut output = String::new();

    if automaton.conflict_found {
        output.push_str(&format!(
            "{}[CONFLICT] {}A conflict was found in the analyzed contract.{}\n",
            ConsoleColors::FG_RED,
            ConsoleColors::FG_WHITE,
            ConsoleColors::RESET
        ));
        output.push_str(&print_trace(automaton));
    } else {
        output.push_str(&format!(
            "{}[CONFLICT-FREE] {}The analyzed contract is conflict-free.{}\n",
            ConsoleColors::FG_GREEN,
            ConsoleColors::FG_WHITE,
            ConsoleColors::RESET
        ));
    }


    output.push_str("\n-------------------------------------------------------\n");
    
    output.push_str(&format!("Completed in {}ms\n", ms));
    output.push_str(&format!("Max RAM: {}MB\n", rss));
    output.push_str(&format!("Max Total Memory: {}MB\n", total));
    
    output.push_str("-------------------------------------------------------\n");
    
    output
}

fn parse_command_line(args: &[String]) -> RunConfiguration {
    if args.len() < 2 || args[1].starts_with('-') {
        print_usage();
        std::process::exit(0);
    }

    let mut config = RunConfiguration::new();
    
    config.set_contract_file_name(args[1].clone());
    let file_stem = Path::new(&args[1])
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("contract");
    
    config.set_result_file_name(format!("{}.result", file_stem));
    config.set_global_log_filename(format!("{}.log", file_stem));
    config.set_automaton_file_name(format!("{}.dot", file_stem));
    config.set_min_automaton_file_name(format!("{}_min.dot", file_stem));
    config.set_decompositions_file_name(format!("{}.csv", file_stem));
    
    let mut i = 2;
    while i < args.len() {
        let arg = &args[i];

        if arg.starts_with('-') && !arg.starts_with("--") && arg.len() > 2 {
            for ch in arg.chars().skip(1) {
                match ch {
                    'h' => {
                        print_usage();
                        std::process::exit(0);
                    }
                    'v' => {
                        config.set_log_level(LogLevel::Verbose);
                    }
                    'g' => {
                        config.set_export_automaton(true);
                        config.set_export_decompositions(true);
                    }
                    'n' => {
                        config.set_use_prunning(false);
                    }
                    'c' => {
                        config.set_continue_on_conflict(true);
                    }
                    'm' => {
                        config.set_export_automaton(true);
                        config.set_export_min_automaton(true);
                    }
                    't' => {
                        config.set_test(true);
                    }
                    _ => {
                        eprintln!("Unknown option: -{}", ch);
                        print_usage();
                        std::process::exit(1);
                    }
                }
            }

            i += 1;
            continue;
        }

        match arg.as_str() {
            "-h" | "--help" => {
                print_usage();
                std::process::exit(0);
            }
            "-v" | "--verbose" => {
                config.set_log_level(LogLevel::Verbose);
            }
            "-g" => {
                config.set_export_automaton(true);
                config.set_export_decompositions(true);
            }
            "-n" | "--no-prunning" => {
                config.set_use_prunning(false);
            }
            "-c" | "--continue" => {
                config.set_continue_on_conflict(true);
            }
            "-m" => {
                config.set_export_automaton(true);
                config.set_export_min_automaton(true);
            }
            "-t" => {
                config.set_test(true);
            }
            _ => {
                eprintln!("Unknown option: {}", arg);
                print_usage();
                std::process::exit(1);
            }
        }

        i += 1;
    }
    
    config
}

fn get_automaton_data(time: u64, memory: u64, automaton: &Automaton, contract: &Contract) -> String {
    let automaton_size_mb = estimate_automaton_size(automaton) as f64 / (1024.0 * 1024.0);
    
    format!(
        "{};{};{};{};{};{};{};{:.2};{:.2};success",
        time,
        automaton.states.len(),
        automaton.transitions.len(),
        contract.individuals.len(),
        contract.actions.len(),
        if automaton.conflict_found { 1 } else { 0 },
        automaton.get_conflicts().len(),
        automaton_size_mb,
        memory as f64
    )
}

/// Estimativa aproximada do tamanho do autômato em bytes (deep size)
fn estimate_automaton_size(automaton: &Automaton) -> usize {
    let mut total = std::mem::size_of_val(automaton);
    
    // Estados (Heap)
    // FxHashSet overhead é aprox 1.1 * capacity * size_of
    let states_capacity = automaton.states.capacity();
    total += states_capacity * (std::mem::size_of::<State>() + 16);
    
    for state in &automaton.states {
        // Trace Vec
        total += state.trace.capacity() * std::mem::size_of::<usize>();
    }
    
    // Transições (Heap)
    let transitions_capacity = automaton.transitions.capacity();
    total += transitions_capacity * (std::mem::size_of::<Transition>() + 16);
    
    // Note: source_map dentro da Transição é um Arc, 
    // então o conteúdo real já está sendo monitorado pelo MemoryGuard global
    // ou contado uma vez se formos rigorosos, mas aqui estimamos o tamanho do grafo.
    
    // Mapa de Cláusula -> ID
    let map_capacity = automaton.state_map.capacity();
    total += map_capacity * (std::mem::size_of::<Clause>() + std::mem::size_of::<usize>() + 16);
    
    total
}

fn run_tests() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let config = parse_command_line(&args);
    let logger = match Logger::new(config.clone()) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to initialize logger: {}", e);
            std::process::exit(1);
        }
    };

    let (total_ram_mb, total_swap_mb) = get_system_memory_info();
    let max_process_mb = calculate_safe_memory_limit(total_ram_mb, total_swap_mb);
    
    let memory_guard = MemoryGuard::new(max_process_mb, logger.clone());
    let _guard_handle = memory_guard.start_monitoring();

    logger.log(LogType::Necessary, "Memory guard active:");
    logger.log(LogType::Necessary, &format!("   - System RAM: {}MB", total_ram_mb));
    logger.log(LogType::Necessary, &format!("   - System Swap: {}MB", total_swap_mb));
    logger.log(LogType::Necessary, &format!("   - Process limit: {}MB ({:.0}% of Total RAM + Swap)", 
                max_process_mb,
                (max_process_mb as f64 / (total_ram_mb + total_swap_mb) as f64) * 100.0));

    logger.log(LogType::Necessary, &format!("Using {:?}", config));
    logger.log(
        LogType::Necessary,
        &format!("Analysing contract in {}", config.contract_file_name()),
    );
    
    let analysis_logger = logger.clone();
    let input_string = fs::read_to_string(config.contract_file_name())
        .map_err(|e| format!("Erro ao ler o ficheiro: {}", e))?;

    let mut pairs = RCLParser::parse(Rule::main, &input_string)
        .map_err(|e| format!("Erro de sintaxe: \n{}", e))?;

    let main_pair = pairs.next().unwrap();
    
    // Carrega o contrato
    let contract: Contract = build_ast(main_pair)
        .map_err(|e| format!("Erro ao construir AST: \n{}", e))?;

    logger.log(LogType::Necessary, &format!("Loaded Contract: {}", contract));

    let symbol_table = SymbolTable::instance();
    let table = symbol_table.lock().unwrap();

    logger.log(LogType::Necessary, &format!("{}", *table));
    drop(table);

    logger.log(LogType::Necessary, "Processing contract...");

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut local_logger = analysis_logger;
        let start = Instant::now();
        let mut constructor = AutomataConstructor::new(config.clone());
        let automaton = constructor.process(contract.clone(), &mut local_logger);
        let elapsed = start.elapsed();

        let rss = memory_guard.max_rss_used.load(Ordering::Relaxed);
        let total = memory_guard.max_total_used.load(Ordering::Relaxed);

        let result = print_result(&automaton, elapsed.as_millis() as u64, rss, total);
        logger.log(LogType::Necessary, &result);
        
        let data = get_automaton_data(elapsed.as_millis() as u64, total, &automaton, &contract);
        println!("{}", data);
    }));

    match result {
        Ok(_) => {
            logger.log(LogType::Necessary, "Analysis completed successfully");
        }
        Err(payload) => {
            let msg = if let Some(s) = payload.downcast_ref::<&str>() {
                format!("{}", s)
            } else if let Some(s) = payload.downcast_ref::<String>() {
                format!("{}", s)
            } else {
                String::from("Unknown panic payload")
            };
            // Aligned with 12 columns total (1 from script + 11 from tool)
            println!(";;;;;;;;;{}", msg);
        }
    }

    Ok(())
}

/// Imprime informações de uso
fn print_usage() {
    println!("recall - RelativizEd ContrAct Language anaLyser (v1.0)\n");
    println!("USAGE:");
    println!("    recall <CONTRACT_FILE> [OPTIONS]\n");
    println!("OPTIONS:");
    println!("    -h, --help          Print this message and exit");
    println!("    -v, --verbose       Turn on the verbose mode");
    println!("    -g                  Exports the automaton into a graphviz file");
    println!("                        Default filename is <CONTRACT_FILE>.dot");
    println!("    -n, --no-prunning   Don't use the prunning method");
    println!("    -c, --continue      Continues the analysis if a conflict is found");
    println!("    -m                  Export minimized automaton");
    println!("    -t                  Test mode (outputs CSV metrics)\n");
    println!("EXAMPLES:");
    println!("    recall contract.rcl");
    println!("        Analyzes a contract in the file 'contract.rcl'");
    println!("    recall contract.rcl -g");
    println!("        Analyzes the contract and writes automaton in a file\n");
    println!("Please report issues to: edson.luiz.pilati@uel.br / bonifacio@uel.br");
    println!("More information: https://recall-site.github.io/");
}

// ==================== Módulo de Utilidades de Console ====================

mod console_util {
    pub struct ConsoleColors;

    impl ConsoleColors {
        pub const RESET: &'static str = "\u{001B}[0m";

        pub const FG_RED: &'static str = "\u{001B}[31m";
        pub const FG_GREEN: &'static str = "\u{001B}[32m";
        pub const FG_YELLOW: &'static str = "\u{001B}[33m";
        pub const FG_BLUE: &'static str = "\u{001B}[34m";
        pub const FG_WHITE: &'static str = "\u{001B}[37m";
    }
}
