use rustc_hash::FxHashSet;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use std::sync::Arc;
use crate::{
    Automaton, State, StateSituation, Transition, Clause, Contract,
    RelativizedAction, ActionExtractor, ClauseDecomposer, ConflictSearcher,
    RunConfiguration, SymbolTable, Logger, LogLevel, LogType, CompressedConcurrentActions
};

/// Construtor de autômatos para contratos
pub struct AutomataConstructor {
    automaton: Option<Automaton>,
    concurrent_actions: Option<CompressedConcurrentActions>,
    relativized_actions: Option<FxHashSet<Arc<RelativizedAction>>>,
    extractor: Option<ActionExtractor>,
    // decomposer: Option<ClauseDecomposer>,
    // searcher: Option<ConflictSearcher>,
    config: RunConfiguration,
    current_contract: Option<Contract>,
}

impl AutomataConstructor {
    /// Cria um novo construtor de autômatos
    /// 
    /// # Argumentos
    /// * `config` - Configuração de execução
    pub fn new(config: RunConfiguration) -> Self {
        AutomataConstructor {
            automaton: None,
            concurrent_actions: None,
            relativized_actions: None,
            extractor: None,
            // decomposer: None,
            // searcher: None,
            config,
            current_contract: None,
        }
    }

    /// Processa um contrato e constrói seu autômato
    /// 
    /// # Argumentos
    /// * `contract` - O contrato a ser processado
    /// 
    /// # Retorna
    /// O autômato construído
    pub fn process(&mut self, contract: Contract, logger: &mut Logger) -> Automaton {
        // Inicializa estado
        self.current_contract = Some(contract.clone());
        self.relativized_actions = None;
        self.concurrent_actions = None;

        if self.extractor.is_none() {
            if let Some(ref contract) = self.current_contract {
                self.extractor = Some(ActionExtractor::new(
                    contract.get_all_conflicts(),
                ));
            }
        }

        // Log de informações do contrato
        if self.config.log_level() == LogLevel::Verbose {
            self.log_contract_info(&contract, logger);
        }

        // Cria autômato e componentes auxiliares
        let mut automaton = Automaton::new(contract.clone());

        // Constrói o autômato a partir do estado inicial
        if let Some(initial_state) = automaton.initial.clone() {
            self.automaton = Some(automaton.clone());
            self.construct_automaton(initial_state.id, logger);
            automaton = self.automaton.take().unwrap();
        }

        // Limpa referências
        self.current_contract = None;
        
        automaton
    }

    /// Registra informações do contrato no log
    fn log_contract_info(&self, contract: &Contract, logger: &mut Logger) {
        let mut info = String::from("Contract Info: ");
        info.push_str(&format!("{}\n", contract));
        
        info.push_str("\nActions:\n");
        for action in &contract.actions {
            info.push_str(&format!("{} ", action));
        }
        
        info.push_str("\nIndividuals:\n");
        let symbol_table = SymbolTable::instance();
        let table = symbol_table.lock().unwrap();
        for &individual in &contract.individuals {
            if let Some(symbol) = table.get_symbol_by_id(individual) {
                info.push_str(&format!("{} ", symbol.value()));
            }
        }
        
        logger.log(LogType::Necessary, &format!("{}", info));
    }

    /// Algoritmo 1 - Construção do Autômato
    /// 
    /// # Argumentos
    /// * `state_id` - ID do estado a ser analisado
    fn construct_automaton(&mut self, state_id: usize, logger: &mut Logger) {
        let clause = if let Some(ref automaton) = self.automaton {
            automaton.get_state_by_id(state_id)
                .and_then(|s| s.clause.clone())
        } else {
            return;
        };

        let Some(clause) = clause else {
            self.mark_boolean_state(state_id, true);
            return;
        };

        let individuals: FxHashSet<i32> = Self::get_individuals(&self, &clause);

        if let Clause::Boolean { value, .. } = clause {
            self.mark_boolean_state(state_id, value);
            return;
        }

        let has_conflict = self.check_conflict_without_clone(state_id, &individuals);

        if has_conflict {
            if let Some(ref mut automaton) = self.automaton {
                automaton.conflict_found = true;
            }
        }

        // Verifica condição de parada
        let should_stop = if let Some(ref automaton) = self.automaton {
            automaton.conflict_found && !self.config.is_continue_on_conflict()
        } else {
            false
        };

        if should_stop {
            return;
        }


        let compressed_actions = self.generate_actions(&clause, &individuals, logger);

        let source_map = &compressed_actions.source_map;
        let masks = &compressed_actions.valid_masks;

        const BATCH_SIZE: usize = 2000; 

        for chunk in masks.chunks(BATCH_SIZE) {
            
            let batch_results: Vec<_> = {
        
                let decomposer = Some(ClauseDecomposer::new(
                    individuals.clone(),
                    true,
                ));

                chunk.par_iter()
                    .map(|&mask| {
                        let mut action_list = Vec::with_capacity(mask.count_ones() as usize);
                        let mut temp_mask = mask;
                        
                        let mut temp_set_for_logic = FxHashSet::default();

                        while temp_mask > 0 {
                            let idx = temp_mask.trailing_zeros();
                            if let Some(act) = source_map.get(idx as usize) {
                                let action_ref = act.clone();
                                action_list.push(action_ref.clone());
                                temp_set_for_logic.insert(action_ref);
                            }
                            temp_mask &= temp_mask - 1;
                        }
                        
                        // Calcula próxima cláusula usando o Set (lógica booleana)
                        let next_clause = decomposer.as_ref().unwrap().decompose(&clause, &temp_set_for_logic);

                        // Retorna o VEC para ser armazenado na transição
                        (action_list, next_clause)
                    })
                    .collect() 
            };

            for (action_set, next_clause) in batch_results {
                if let Some(ref mut automaton) = self.automaton {
                    
                    if let Some(existing_state) = automaton.get_state_by_clause(&next_clause) {
                        let transition = Transition::new(state_id, existing_state.id, action_set);
                        automaton.add_transition(transition);
                    } else {
                        let new_state = State::with_auto_id(Some(next_clause.clone()));
                        let new_state_id = new_state.id;

                        logger.log(LogType::Necessary, &format!("New State: {}", new_state));
                        
                        automaton.add_state(new_state);
                        
                        let transition = Transition::new(state_id, new_state_id, action_set);
                        let transition_id = transition.id;
                        automaton.add_transition(transition);
                        
                        automaton.update_state(new_state_id, |s| {
                            s.push_trace(transition_id);
                        });
                        
                        self.construct_automaton(new_state_id, logger);
                    }
                }
            }
        }
    }

    fn check_conflict_without_clone(&mut self, state_id: usize, indiv: &FxHashSet<i32>) -> bool {
        if let Some(ref mut automaton) = self.automaton {
            // Remove temporariamente, modifica, reinsere
            let searcher = Some(ConflictSearcher::new(
                indiv.clone(),
                self.current_contract.clone().unwrap().get_all_conflicts(),
            ));

            if let Some(mut state) = automaton.get_state_by_id_mut(state_id) {
                let has_conflict = if let Some(ref searcher) = searcher {
                    searcher.has_conflict(&mut state)
                } else {
                    false
                };
                automaton.replace_state(state);
                return has_conflict;
            }
        }
        false
    }

    /// Marca um estado booleano como satisfação ou violação
    fn mark_boolean_state(&mut self, state_id: usize, value: bool) {
        if let Some(ref mut automaton) = self.automaton {
            automaton.update_state(state_id, |s| {
                s.situation = if value {
                    StateSituation::Satisfaction
                } else {
                    StateSituation::Violating
                };
            });
        }
    }

    /// Gera ações para uma cláusula
    /// 
    /// # Argumentos
    /// * `clause` - A cláusula para gerar ações
    /// 
    /// # Retorna
    /// Lista de conjuntos de ações relativizadas concorrentes
    fn generate_actions(&mut self, clause: &Clause, indiv: &FxHashSet<i32>, logger: &mut Logger) -> CompressedConcurrentActions {
        if let Some(ref mut extractor) = self.extractor {
            return extractor.calculate_concurrent_relativized_actions(clause, indiv, &self.config, logger);
        }

        CompressedConcurrentActions {
            source_map: Vec::new(),
            valid_masks: Vec::new(),
        }
    }

    fn get_individuals(&self, clause: &Clause) -> FxHashSet<i32> {
        if self.config.is_use_prunning() {
            ActionExtractor::calculate_individuals(&clause, self.current_contract.clone().unwrap().individuals)
        } else {
            self.current_contract.clone().unwrap().individuals
        }
    }
}
