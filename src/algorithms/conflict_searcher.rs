use rustc_hash::FxHashSet;
use crate::{
    State, StateSituation, Clause, ClauseCompositionType,
    DeonticTag, ConflictInformation, Conflict, ConflictType,
    DeonticClauseType, RelativizationType, BasicAction, ClauseDecomposer,
};

pub struct ConflictSearcher {
    individuals: FxHashSet<i32>,
    conflicts: Vec<Conflict>,
}

impl ConflictSearcher {
    pub fn new(individuals: FxHashSet<i32>, conflicts: Vec<Conflict>) -> Self {
        ConflictSearcher {
            individuals,
            conflicts,
        }
    }

    /// Verifica se um estado possui conflitos
    /// 
    /// # Argumentos
    /// * `state` - O estado a ser verificado
    /// 
    /// # Retorna
    /// `true` se conflitos foram encontrados, `false` caso contrário
    pub fn has_conflict(&self, state: &mut State) -> bool {
        if let Some(clause) = &state.clause {
            let processed_clause = ClauseDecomposer::process_composed_actions(clause);
            let delta = self.extract_tags(&processed_clause);
            
            for (i, d1) in delta.iter().enumerate() {
                for (j, d2) in delta.iter().enumerate() {
                    if i == j {
                        continue;
                    }
                    
                    for tag in d1 {
                        let conflict_set = self.generate_conflict_set(tag);
                        let intersection: FxHashSet<_> = conflict_set
                            .intersection(d2)
                            .cloned()
                            .collect();
                        
                        if !intersection.is_empty() {
                            state.situation = StateSituation::Conflicting;
                            state.conflict_information = Some(ConflictInformation::new(
                                tag.clone(),
                                intersection,
                                d2.clone(),
                            ));
                            return true;
                        }
                    }
                }
            }
            
            state.situation = StateSituation::ConflictFree;
            false
        } else {
            state.situation = StateSituation::ConflictFree;
            false
        }
    }

    /// Função Delta que retorna todas as tags deônticas de uma cláusula
    /// 
    /// # Argumentos
    /// * `clause` - A cláusula da qual extrair as tags
    /// 
    /// # Retorna
    /// Um vetor de conjuntos de tags deônticas
    fn extract_tags(&self, clause: &Clause) -> Vec<FxHashSet<DeonticTag>> {
        let mut result = Vec::new();
        
        if let Clause::Deontic { 
            sender, 
            receiver, 
            relativization_type, 
            action, 
            deontic_type, 
            .. 
        } = clause {
            let mut dt = FxHashSet::default();
            let basic_actions = action.get_basic_actions();
            
            match relativization_type {
                RelativizationType::Global => {
                    for ba in basic_actions {
                        dt.insert(DeonticTag::global(*deontic_type, ba));
                    }
                }
                
                RelativizationType::Relativized => {
                    for ba in basic_actions {
                        dt.insert(DeonticTag::relativized(*deontic_type, ba, *sender));
                    }
                }
                
                RelativizationType::Directed => {
                    for ba in basic_actions {
                        dt.insert(DeonticTag::directed(*deontic_type, ba, *sender, *receiver));
                    }
                }
            }
            
            result.push(dt);
        }
        
        if let Some(composition) = clause.get_composition() {
            let other_tags = self.extract_tags(&composition.other);
            
            match composition.composition_type {
                ClauseCompositionType::And => {
                    result.extend(other_tags);
                }
                _ => {
                    result.extend(other_tags);
                }
            }
        }
        
        result
    }

    /// Função F# que retorna todas as tags deônticas conflitantes para uma tag dada
    /// 
    /// # Argumentos
    /// * `tag` - A tag para a qual gerar conflitos
    /// 
    /// # Retorna
    /// Conjunto de todas as tags que conflitam com a tag dada
    fn generate_conflict_set(&self, tag: &DeonticTag) -> FxHashSet<DeonticTag> {
        let mut conflict_set = FxHashSet::default();
        
        match tag.deontic_type {
            DeonticClauseType::Obligation => {
                conflict_set.extend(self.generate_tags_by_type(
                    DeonticClauseType::Prohibition,
                    tag,
                ));
                conflict_set.extend(self.get_predefined_conflicts(
                    tag,
                    &[DeonticClauseType::Obligation, DeonticClauseType::Permission],
                ));
            }
            
            DeonticClauseType::Permission => {
                conflict_set.extend(self.generate_tags_by_type(
                    DeonticClauseType::Prohibition,
                    tag,
                ));
                conflict_set.extend(self.get_predefined_conflicts(
                    tag,
                    &[DeonticClauseType::Obligation],
                ));
            }
            
            DeonticClauseType::Prohibition => {
                conflict_set.extend(self.generate_tags_by_type(
                    DeonticClauseType::Obligation,
                    tag,
                ));
                conflict_set.extend(self.generate_tags_by_type(
                    DeonticClauseType::Permission,
                    tag,
                ));
            }
        }
        
        conflict_set
    }

    /// Retorna todos os conflitos predefinidos na lista de conflitos
    /// para uma determinada ação/tag
    /// 
    /// # Argumentos
    /// * `tag` - Tag contendo a ação para buscar conflitos predefinidos
    /// * `types` - Tipos de conflitos a gerar se existir um conflito determinado
    /// 
    /// # Retorna
    /// Todos os conflitos predefinidos
    fn get_predefined_conflicts(
        &self,
        tag: &DeonticTag,
        types: &[DeonticClauseType],
    ) -> FxHashSet<DeonticTag> {
        let mut result = FxHashSet::default();
        
        for conflict in &self.conflicts {
            if conflict.a == tag.action {
                match conflict.conflict_type {
                    ConflictType::Global => {
                        for &deontic_type in types {
                            result.extend(self.generate_tags_by_type(
                                deontic_type,
                                &DeonticTag::global(deontic_type, conflict.b.clone()),
                            ));
                        }
                    }
                    
                    ConflictType::Relativized => {
                        for &deontic_type in types {
                            if tag.relativization == RelativizationType::Global {
                                result.extend(self.generate_tags_by_type(
                                    deontic_type,
                                    &DeonticTag::global(deontic_type, conflict.b.clone()),
                                ));
                            } else {
                                result.extend(self.generate_relativized_tags(
                                    deontic_type,
                                    &conflict.b,
                                    tag.sender,
                                ));
                            }
                        }
                    }
                }
            }
        }
        
        result
    }

    /// Gera tags para um tipo dado
    /// 
    /// # Argumentos
    /// * `deontic_type` - Tipo das tags
    /// * `action` - Ação incluída nas tags
    /// * `sender` - Remetente de todas as tags
    /// 
    /// # Retorna
    /// Conjunto de tags
    fn generate_relativized_tags(
        &self,
        deontic_type: DeonticClauseType,
        action: &BasicAction,
        sender: i32,
    ) -> FxHashSet<DeonticTag> {
        let mut tags = FxHashSet::default();
        
        // GLOBAL
        tags.insert(DeonticTag::global(deontic_type, action.clone()));
        
        // RELATIVIZED
        tags.insert(DeonticTag::relativized(deontic_type, action.clone(), sender));
        
        // DIRECTED (para todos os indivíduos)
        for &receiver in &self.individuals {
            tags.insert(DeonticTag::directed(
                deontic_type,
                action.clone(),
                sender,
                receiver,
            ));
        }
        
        tags
    }

    /// Gera todos os tipos de tags por um tipo dado
    /// 
    /// # Argumentos
    /// * `deontic_type` - O tipo das tags
    /// * `tag` - Informação adicional sobre as tags geradas (ação, remetente, receptor)
    /// 
    /// # Retorna
    /// Conjunto de tags com todos os tipos possíveis
    fn generate_tags_by_type(
        &self,
        deontic_type: DeonticClauseType,
        tag: &DeonticTag,
    ) -> FxHashSet<DeonticTag> {
        let mut tags = FxHashSet::default();
        
        match tag.relativization {
            RelativizationType::Global => {
                // GLOBAL
                tags.insert(DeonticTag::global(deontic_type, tag.action.clone()));
                
                // Para cada indivíduo
                for &i in &self.individuals {
                    // RELATIVIZED
                    tags.insert(DeonticTag::relativized(
                        deontic_type,
                        tag.action.clone(),
                        i,
                    ));
                    
                    // DIRECTED (para todos os pares de indivíduos)
                    for &j in &self.individuals {
                        tags.insert(DeonticTag::directed(
                            deontic_type,
                            tag.action.clone(),
                            i,
                            j,
                        ));
                    }
                }
            }
            
            RelativizationType::Relativized => {
                let i = tag.sender;
                
                // GLOBAL
                tags.insert(DeonticTag::global(deontic_type, tag.action.clone()));
                
                // RELATIVIZED
                tags.insert(DeonticTag::relativized(
                    deontic_type,
                    tag.action.clone(),
                    i,
                ));
                
                // DIRECTED (para todos os receptores)
                for &j in &self.individuals {
                    tags.insert(DeonticTag::directed(
                        deontic_type,
                        tag.action.clone(),
                        i,
                        j,
                    ));
                }
            }
            
            RelativizationType::Directed => {
                let i = tag.sender;
                let j = tag.receiver;
                
                // GLOBAL
                tags.insert(DeonticTag::global(deontic_type, tag.action.clone()));

                // RELATIVIZED
                tags.insert(DeonticTag::relativized(
                    deontic_type,
                    tag.action.clone(),
                    i,
                ));
                
                // DIRECTED (mantém sender e receiver)
                tags.insert(DeonticTag::directed(
                    deontic_type,
                    tag.action.clone(),
                    i,
                    j,
                ));
            }
        }
        
        tags
    }
}
