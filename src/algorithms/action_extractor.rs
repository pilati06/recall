use rustc_hash::FxHashSet;
use std::sync::Arc;
use crate::{
    Clause, RelativizedAction, Conflict, RelativizationType, 
    ClauseDecomposer, ContractUtil, CompressedConcurrentActions, 
    RunConfiguration, Logger, LogLevel, LogType
};

/// Extrator de ações que calcula ações relativizadas e concorrentes
pub struct ActionExtractor {
    individuals: FxHashSet<i32>,
    conflicts: Vec<Conflict>,
}

impl ActionExtractor {
    /// Cria um novo extrator de ações
    /// 
    /// # Argumentos
    /// * `individuals` - Conjunto de indivíduos do contrato
    /// * `conflicts` - Lista de conflitos predefinidos
    pub fn new(individuals: FxHashSet<i32>, conflicts: Vec<Conflict>) -> Self {
        ActionExtractor {
            individuals,
            conflicts,
        }
    }

    /// Calcula ações relativizadas concorrentes para uma cláusula
    /// 
    /// # Argumentos
    /// * `clause` - A cláusula para extrair ações
    /// 
    /// # Retorna
    /// Lista de conjuntos de ações relativizadas que podem ocorrer concorrentemente
    pub fn calculate_concurrent_relativized_actions(
        &mut self,
        clause: &Clause,
        config: &RunConfiguration,
        logger: &mut Logger
    ) -> CompressedConcurrentActions {
        let processed = ClauseDecomposer::process_composed_actions(clause);
        
        let actions = self.calculate_relativized_actions(&processed);
        
        if config.log_level() == LogLevel::Verbose {
            logger.log(LogType::Necessary, &format!(
                "Concurrent Relativized Actions for {} is [{}]",
                processed, actions.iter()
                            .map(|a| a.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
            ));
        }
        
        let mut compressed_result = ContractUtil::calculate_concurrent_relativized_actions(
            actions.clone(),
            &self.conflicts,
            &config,
            logger
        );
        
        if compressed_result.source_map.len() == 1 {
            let action = &compressed_result.source_map[0];
            
            let negation = Arc::new(RelativizedAction::negation(action));
            compressed_result.source_map.push(negation);
            let new_index = compressed_result.source_map.len() - 1;
            let new_mask: u32 = 1 << new_index;
            compressed_result.valid_masks.push(new_mask);
        }
        
        compressed_result
    }

    /// Calcula ações relativizadas para uma cláusula
    /// 
    /// # Argumentos
    /// * `clause` - A cláusula para extrair ações
    /// 
    /// # Retorna
    /// Conjunto de ações relativizadas extraídas da cláusula
    pub fn calculate_relativized_actions(&self, clause: &Clause) -> FxHashSet<Arc<RelativizedAction>> {
        let mut actions = FxHashSet::default();
        
        if let Clause::Boolean { .. } = clause {
            return actions;
        }

        match clause {
            Clause::Deontic { sender, receiver, relativization_type, action, .. } |
            Clause::Dynamic { sender, receiver, relativization_type, action, .. } => {
                let basic_actions = action.get_basic_actions();
                
                match relativization_type {
                    RelativizationType::Directed => {
                        for ba in basic_actions {
                            actions.insert(Arc::new(RelativizedAction::new(*sender, ba, *receiver)));
                        }
                    }
                    
                    RelativizationType::Relativized => {
                        let ignore_self = self.individuals.len() > 1;
                        
                        for &j in &self.individuals {
                            if !(ignore_self && sender == &j) {
                                for ba in &basic_actions {
                                    actions.insert(Arc::new(RelativizedAction::new(*sender, ba.clone(), j)));
                                }
                            }
                        }
                    }
                    
                    RelativizationType::Global => {
                        let ignore_self = self.individuals.len() > 1;
                        
                        for &i in &self.individuals {
                            for &j in &self.individuals {
                                if !(ignore_self && i == j) {
                                    for ba in &basic_actions {
                                        actions.insert(Arc::new(RelativizedAction::new(i, ba.clone(), j)));
                                    }
                                }
                            }
                        }
                    }
                }
            }

             Clause::Boolean { .. } => {
                // Já tratado acima
            }
        }
        
        if let Some(composition) = clause.get_composition() {
            let other_actions = self.calculate_relativized_actions(&composition.other);
            actions.extend(other_actions);
        }
        
        actions
    }
}