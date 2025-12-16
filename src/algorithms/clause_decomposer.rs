use rustc_hash::FxHashSet;
use std::sync::Arc;

use crate::{
    Clause, ClauseComposition, ClauseCompositionType,
    Action, ActionOperator, RelativizedAction, 
    DeonticClauseType, RelativizationType,
};

pub struct ClauseDecomposer {
    individuals: FxHashSet<i32>,
    ignore_self_actions: bool,
}

impl ClauseDecomposer {
    pub fn new(individuals: FxHashSet<i32>, ignore_self_actions: bool) -> Self {
        ClauseDecomposer {
            individuals,
            ignore_self_actions,
        }
    }

    pub fn decompose(&self, clause: &Clause, actions: &FxHashSet<Arc<RelativizedAction>>) -> Clause {
        if let Some(comp) = clause.get_composition() {
            let mut clause_copy = clause.clone();
            clause_copy.set_composition_to_none();
            
            let c1 = self.decompose_single(&clause_copy, actions);
            let c2 = self.decompose(&comp.other, actions);
            
            self.combine(c1, c2, comp.composition_type)
        } else {
            self.decompose_single(clause, actions)
        }
    }

    fn decompose_single(&self, clause: &Clause, actions: &FxHashSet<Arc<RelativizedAction>>) -> Clause {
        match clause {
            Clause::Boolean { .. } => clause.clone(),
            
            Clause::Deontic { action, .. } => {
                if Self::is_composed_action(action) {
                    let processed = Self::process_composed_actions(clause);
                    self.decompose(&processed, actions)
                } else {
                    self.decompose_deontic(clause, actions)
                }
            }
            
            Clause::Dynamic { action, .. } => {
                if Self::is_composed_action(action) {
                    let processed = Self::process_composed_actions(clause);
                    self.decompose(&processed, actions)
                } else {
                    self.decompose_dynamic(clause, actions)
                }
            }
        }
    }

    fn decompose_dynamic(&self, clause: &Clause, actions: &FxHashSet<Arc<RelativizedAction>>) -> Clause {
        if let Clause::Dynamic { relativization_type, action, clause: inner_clause, .. } = clause {
            if let Action::Basic(basic_action) = action {
                if basic_action.skip {
                    return (**inner_clause).clone();
                }

                let clause_actions = self.generate_relativized_actions(clause);
                let has_intersection = actions.iter().any(|arc| clause_actions.contains(&**arc));

                let mut sat = match relativization_type {
                    RelativizationType::Relativized => has_intersection,
                    RelativizationType::Global | RelativizationType::Directed => {
                        clause_actions.iter().all(|ca| actions.contains(ca))
                    }
                };

                if basic_action.negation {
                    sat = !sat;
                }

                if sat {
                    (**inner_clause).clone()
                } else {
                    Clause::boolean_true()
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn decompose_deontic(&self, clause: &Clause, actions: &FxHashSet<Arc<RelativizedAction>>) -> Clause {
        if let Clause::Deontic { action, deontic_type, penalty, relativization_type, .. } = clause {
            if let Action::Basic(basic_action) = action {
                if basic_action.violation || basic_action.skip {
                    return self.decompose_deontic_special(clause, actions);
                }

                let clause_actions = self.generate_relativized_actions(clause);
                let intersection: FxHashSet<Arc<RelativizedAction>> = actions
                    .iter()
                    .filter(|arc| clause_actions.contains(&**arc))
                    .cloned()
                    .collect();

                match deontic_type {
                    DeonticClauseType::Obligation => {
                        let satisfied = match relativization_type {
                            RelativizationType::Relativized => !intersection.is_empty(),
                            RelativizationType::Global | RelativizationType::Directed => {
                                clause_actions.iter().all(|ca| actions.contains(ca))
                            }
                        };

                        if satisfied {
                            Clause::boolean_true()
                        } else {
                            penalty.as_ref()
                                .map(|p| (**p).clone())
                                .unwrap_or_else(|| Clause::boolean_false())
                        }
                    }
                    
                    DeonticClauseType::Prohibition => {
                        if intersection.is_empty() {
                            Clause::boolean_true()
                        } else {
                            penalty.as_ref()
                                .map(|p| (**p).clone())
                                .unwrap_or_else(|| Clause::boolean_false())
                        }
                    }
                    
                    DeonticClauseType::Permission => Clause::boolean_true(),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn decompose_deontic_special(&self, clause: &Clause, _actions: &FxHashSet<Arc<RelativizedAction>>) -> Clause {
        if let Clause::Deontic { action, deontic_type, penalty, .. } = clause {
            if let Action::Basic(basic_action) = action {
                match deontic_type {
                    DeonticClauseType::Obligation => {
                        if basic_action.skip {
                            Clause::boolean_true()
                        } else {
                            penalty.as_ref()
                                .map(|p| (**p).clone())
                                .unwrap_or_else(|| Clause::boolean_false())
                        }
                    }
                    
                    DeonticClauseType::Prohibition => {
                        if basic_action.violation {
                            Clause::boolean_true()
                        } else {
                            penalty.as_ref()
                                .map(|p| (**p).clone())
                                .unwrap_or_else(|| Clause::boolean_false())
                        }
                    }
                    
                    DeonticClauseType::Permission => Clause::boolean_true(),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn combine(&self, c1: Clause, c2: Clause, comp_type: ClauseCompositionType) -> Clause {
        match (&c1, &c2) {
            (Clause::Boolean { value: v1, .. }, Clause::Boolean { value: v2, .. }) => {
                self.evaluate_boolean(*v1, *v2, comp_type)
            }
            
            (Clause::Boolean { .. }, _) => self.combine_clause(c2, c1, comp_type),
            
            (_, Clause::Boolean { .. }) => self.combine_clause(c1, c2, comp_type),
            
            _ => {
                if c2.contains(comp_type, &c1) {
                    c2
                } else {
                    let mut result = c1.clone();
                    result.set_composition(ClauseComposition::new(comp_type, c2));
                    result
                }
            }
        }
    }

    fn evaluate_boolean(&self, v1: bool, v2: bool, comp_type: ClauseCompositionType) -> Clause {
        let result = match comp_type {
            ClauseCompositionType::And => v1 && v2,
            ClauseCompositionType::Or => v1 || v2,
            ClauseCompositionType::Xor => (v1 && !v2) || (!v1 && v2),
            ClauseCompositionType::None => false,
        };
        
        if result {
            Clause::boolean_true()
        } else {
            Clause::boolean_false()
        }
    }

    fn combine_clause(&self, c1: Clause, c2: Clause, comp_type: ClauseCompositionType) -> Clause {
        if let Clause::Boolean { value, .. } = c2 {
            match comp_type {
                ClauseCompositionType::And => {
                    if value {
                        c1
                    } else {
                        c2
                    }
                }
                
                ClauseCompositionType::Or | ClauseCompositionType::Xor => {
                    let mut result = c1.clone();
                    result.set_composition(ClauseComposition::new(comp_type, c2));
                    result
                }
                
                ClauseCompositionType::None => c1,
            }
        } else {
            c1
        }
    }

    fn generate_relativized_actions(&self, clause: &Clause) -> FxHashSet<RelativizedAction> {
        let ignore = if self.individuals.len() > 1 {
            self.ignore_self_actions
        } else {
            false
        };

        let mut set = FxHashSet::default();

        match clause {
            Clause::Deontic { sender, receiver, relativization_type, action, .. } |
            Clause::Dynamic { sender, receiver, relativization_type, action, .. } => {
                let basic_actions = action.get_basic_actions();
                match relativization_type {
                    RelativizationType::Directed => {
                        for action in &basic_actions {
                            set.insert(RelativizedAction::new(*sender, action.clone(), *receiver));
                        }
                    }
                    
                    RelativizationType::Relativized => {
                        for j in &self.individuals {
                            if !(ignore && sender == j) {
                                for action in &basic_actions {
                                    set.insert(RelativizedAction::new(*sender, action.clone(), *j));
                                }
                            }
                        }
                    }
                    
                    RelativizationType::Global => {
                        for i in &self.individuals {
                            for j in &self.individuals {
                                if !(ignore && i == j) {
                                    for action in &basic_actions {
                                        set.insert(RelativizedAction::new(*i, action.clone(), *j));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            
            Clause::Boolean { .. } => {}
        }

        set
    }

    pub fn process_composed_actions(clause: &Clause) -> Clause {
        if let Some(comp) = clause.get_composition() {
            let mut clause_copy = clause.clone();
            clause_copy.set_composition_to_none();
            
            let c1 = Self::process_single_composed_actions(&clause_copy);
            let c2 = Self::process_composed_actions(&comp.other);
            
            Self::append_composition(c1, c2, comp.composition_type)
        } else {
            Self::process_single_composed_actions(clause)
        }
    }
    
    fn append_composition(mut clause: Clause, new_clause: Clause, comp_type: ClauseCompositionType) -> Clause {
        if let Some(comp) = clause.get_composition() {
            let other = comp.other.as_ref().clone();
            let updated_other = Self::append_composition(other, new_clause, comp_type);
            clause.set_composition(ClauseComposition::new(comp.composition_type, updated_other));
            clause
        } else {
            clause.set_composition(ClauseComposition::new(comp_type, new_clause));
            clause
        }
    }

    pub fn process_single_composed_actions(clause: &Clause) -> Clause {
        match clause {
            Clause::Boolean { .. } => clause.clone(),
            
            Clause::Deontic { action, .. } => {
                if let Action::Basic(_) = action {
                    clause.clone()
                } else {
                    let processed = Self::process_deontic_composed_actions(clause);
                    processed
                }
            }
            
            Clause::Dynamic { action, .. } => {
                if let Action::Basic(_) = action {
                    clause.clone()
                } else {
                    let processed = Self::process_dynamic_composed_actions(clause);
                    processed
                }
            }
        }
    }

    pub fn process_deontic_composed_actions(clause: &Clause) -> Clause {
        if let Clause::Deontic { action, deontic_type, .. } = clause {
            if let Action::Composed(_composed) = action {
                match deontic_type {
                    DeonticClauseType::Obligation => Self::process_composed_obligation(clause),
                    DeonticClauseType::Prohibition => Self::process_composed_prohibition(clause),
                    DeonticClauseType::Permission => Self::process_composed_permission(clause),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn process_composed_obligation(clause: &Clause) -> Clause {
        if let Clause::Deontic { sender, receiver, relativization_type, action, deontic_type, penalty, composition } = clause {
            if let Action::Composed(composed) = action {
                match composed.operator {
                    ActionOperator::Concurrency => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: composition.clone(),
                            };
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::And, c2));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Sequence => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let cd = Clause::dynamic_directed(*sender, *receiver, (**left).clone(), c2);
                            
                            let mut cd_with_comp = cd;
                            if let Some(comp) = composition {
                                cd_with_comp.set_composition(comp.clone());
                            }
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::And, cd_with_comp));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Choice => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: composition.clone(),
                            };
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::Or, c2));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    _ => clause.clone(),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn process_composed_prohibition(clause: &Clause) -> Clause {
        if let Clause::Deontic { sender, receiver, relativization_type, action, deontic_type, penalty, composition } = clause {
            if let Action::Composed(composed) = action {
                match composed.operator {
                    ActionOperator::Choice | ActionOperator::Concurrency => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: composition.clone(),
                            };
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::And, c2));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Sequence => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Deontic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                deontic_type: *deontic_type,
                                penalty: penalty.clone(),
                                composition: None,
                            };
                            
                            let cd = Clause::dynamic_directed(*sender, *receiver, (**left).clone(), c2);
                            
                            let mut cd_with_comp = cd;
                            if let Some(comp) = composition {
                                cd_with_comp.set_composition(comp.clone());
                            }
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::Or, cd_with_comp));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    _ => clause.clone(),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn process_composed_permission(clause: &Clause) -> Clause {
        Self::process_composed_obligation(clause)
    }

    pub fn process_dynamic_composed_actions(clause: &Clause) -> Clause {
        if let Clause::Dynamic { sender, receiver, relativization_type, action, clause: inner_clause, composition } = clause {
            if let Action::Composed(composed) = action {
                match composed.operator {
                    ActionOperator::Star => {
                        if let Some(left) = &composed.left {
                            let clause1 = clause.clone();
                            let mut clause2 = (**inner_clause).clone();
                            
                            let dc = Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                clause: Arc::new(clause1),
                                composition: composition.clone(),
                            };
                            
                            clause2.set_composition(ClauseComposition::new(ClauseCompositionType::And, dc));
                            clause2
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Sequence => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                clause: inner_clause.clone(),
                                composition: None,
                            };
                            
                            Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                clause: Arc::new(c1),
                                composition: composition.clone(),
                            }
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Choice => {
                        if let (Some(left), Some(right)) = (&composed.left, &composed.right) {
                            let c1 = Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**left).clone(),
                                clause: inner_clause.clone(),
                                composition: None,
                            };
                            
                            let c2 = Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: (**right).clone(),
                                clause: inner_clause.clone(),
                                composition: composition.clone(),
                            };
                            
                            let mut result = c1;
                            result.set_composition(ClauseComposition::new(ClauseCompositionType::And, c2));
                            result
                        } else {
                            clause.clone()
                        }
                    }
                    
                    ActionOperator::Negation => Self::process_negation_composed_actions(clause),
                    
                    _ => clause.clone(),
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn process_negation_composed_actions(clause: &Clause) -> Clause {
        if let Clause::Dynamic { sender, receiver, relativization_type, action, clause: inner_clause, composition } = clause {
            if let Action::Composed(composed) = action {
                if let Some(left) = &composed.left {
                    match left.as_ref() {
                        Action::Basic(ba) => {
                            let negated = ba.negate();
                            Clause::Dynamic {
                                sender: *sender,
                                receiver: *receiver,
                                relativization_type: *relativization_type,
                                action: Action::Basic(negated),
                                clause: inner_clause.clone(),
                                composition: composition.clone(),
                            }
                        }
                        
                        Action::Composed(inner_composed) => {
                            match inner_composed.operator {
                                ActionOperator::Sequence => {
                                    if let (Some(seq_left), Some(seq_right)) = (&inner_composed.left, &inner_composed.right) {
                                        let c1 = Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**seq_right).clone()),
                                            clause: inner_clause.clone(),
                                            composition: None,
                                        };
                                        
                                        Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**seq_left).clone()),
                                            clause: Arc::new(c1),
                                            composition: composition.clone(),
                                        }
                                    } else {
                                        clause.clone()
                                    }
                                }
                                
                                ActionOperator::Concurrency => {
                                    if let (Some(conc_left), Some(conc_right)) = (&inner_composed.left, &inner_composed.right) {
                                        let c1 = Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**conc_left).clone()),
                                            clause: inner_clause.clone(),
                                            composition: None,
                                        };
                                        
                                        let c2 = Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**conc_right).clone()),
                                            clause: inner_clause.clone(),
                                            composition: composition.clone(),
                                        };
                                        
                                        let mut result = c1;
                                        result.set_composition(ClauseComposition::new(ClauseCompositionType::And, c2));
                                        result
                                    } else {
                                        clause.clone()
                                    }
                                }
                                
                                ActionOperator::Choice => {
                                    if let (Some(choice_left), Some(choice_right)) = (&inner_composed.left, &inner_composed.right) {
                                        let c1 = Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**choice_left).clone()),
                                            clause: inner_clause.clone(),
                                            composition: None,
                                        };
                                        
                                        let c2 = Clause::Dynamic {
                                            sender: *sender,
                                            receiver: *receiver,
                                            relativization_type: *relativization_type,
                                            action: Action::negation((**choice_right).clone()),
                                            clause: inner_clause.clone(),
                                            composition: composition.clone(),
                                        };
                                        
                                        let mut result = c1;
                                        result.set_composition(ClauseComposition::new(ClauseCompositionType::Or, c2));
                                        result
                                    } else {
                                        clause.clone()
                                    }
                                }
                                
                                _ => clause.clone(),
                            }
                        }
                    }
                } else {
                    Clause::boolean_false()
                }
            } else {
                clause.clone()
            }
        } else {
            clause.clone()
        }
    }

    fn is_composed_action(action: &Action) -> bool {
        matches!(action, Action::Composed(_))
    }
}