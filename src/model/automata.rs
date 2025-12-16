use std::sync::Arc;
use rustc_hash::FxHashSet;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::fmt;

use crate::{Clause, Contract, BasicAction, DeonticClauseType, RelativizationType, SymbolTable, RelativizedAction};

// ==================== StateSituation ====================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StateSituation {
    Violating,
    Satisfaction,
    Conflicting,
    ConflictFree,
    NotChecked,
}

// ==================== DeonticTag ====================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeonticTag {
    pub deontic_type: DeonticClauseType,
    pub action: BasicAction,
    pub relativization: RelativizationType,
    pub sender: i32,
    pub receiver: i32,
}

impl DeonticTag {
    pub fn new(
        deontic_type: DeonticClauseType,
        action: BasicAction,
        relativization: RelativizationType,
        sender: i32,
        receiver: i32,
    ) -> Self {
        DeonticTag {
            deontic_type,
            action,
            relativization,
            sender,
            receiver,
        }
    }

    pub fn directed(deontic_type: DeonticClauseType, action: BasicAction, sender: i32, receiver: i32) -> Self {
        Self::new(deontic_type, action, RelativizationType::Directed, sender, receiver)
    }

    pub fn relativized(deontic_type: DeonticClauseType, action: BasicAction, sender: i32) -> Self {
        Self::new(deontic_type, action, RelativizationType::Relativized, sender, -1)
    }

    pub fn global(deontic_type: DeonticClauseType, action: BasicAction) -> Self {
        Self::new(deontic_type, action, RelativizationType::Global, -1, -1)
    }

    pub fn format_with_symbols(&self, symbol_table: &SymbolTable) -> String {
        let symbol = self.deontic_type.symbol();
        
        match self.relativization {
            RelativizationType::Directed => {
                let sender_name = symbol_table.get_symbol_by_id(self.sender)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                let receiver_name = symbol_table.get_symbol_by_id(self.receiver)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                format!("{}({},{},{})", symbol, sender_name, self.action, receiver_name)
            }
            RelativizationType::Relativized => {
                let sender_name = symbol_table.get_symbol_by_id(self.sender)
                    .map(|s| s.value.as_str())
                    .unwrap_or("?");
                format!("{}({},{})", symbol, sender_name, self.action)
            }
            RelativizationType::Global => {
                format!("{}({})", symbol, self.action)
            }
        }
    }
}

impl fmt::Display for DeonticTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Usa uma symbol table padrão para display
        let symbol_table = SymbolTable::default();
        write!(f, "{}", self.format_with_symbols(&symbol_table))
    }
}

// ==================== ConflictInformation ====================

#[derive(Debug, Clone, PartialEq)]
pub struct ConflictInformation {
    pub tag: DeonticTag,
    pub conflicting_tags: FxHashSet<DeonticTag>,
    pub other_set: FxHashSet<DeonticTag>,
}

impl ConflictInformation {
    pub fn new(
        tag: DeonticTag,
        conflicting_tags: FxHashSet<DeonticTag>,
        other_set: FxHashSet<DeonticTag>,
    ) -> Self {
        ConflictInformation {
            tag,
            conflicting_tags,
            other_set,
        }
    }
}

impl fmt::Display for ConflictInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} conflicts with {:?}", self.tag, self.conflicting_tags)
    }
}

// ==================== Transition ====================

static TRANSITION_COUNTER: AtomicUsize = AtomicUsize::new(1);

#[derive(Debug, Clone)]
pub struct Transition {
    pub id: usize,
    pub from: usize,
    pub to: usize,
    pub actions: Vec<Arc<RelativizedAction>>,
}

impl Transition {
    pub fn new(from: usize, to: usize, actions: Vec<Arc<RelativizedAction>>) -> Self {
        Transition {
            id: TRANSITION_COUNTER.fetch_add(1, Ordering::SeqCst),
            from,
            to,
            actions,
        }
    }
}

impl PartialEq for Transition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Transition {}

impl std::hash::Hash for Transition {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "T{}: {} -> {} [{:?}]", 
            self.id, self.from, self.to, self.actions)
    }
}

// ==================== State ====================

static STATE_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone)]
pub struct State {
    pub id: usize,
    pub clause: Option<Clause>,
    pub situation: StateSituation,
    pub conflict_information: Option<ConflictInformation>,
    pub trace: Vec<usize>,
}

impl State {
    pub fn with_auto_id(clause: Option<Clause>) -> Self {
        let id = STATE_COUNTER.fetch_add(1, Ordering::SeqCst);
        State {
            id,
            clause,
            situation: StateSituation::NotChecked,
            conflict_information: None,
            trace: Vec::new(),
        }
    }

    pub fn push_trace(&mut self, transition_id: usize) {
        self.trace.push(transition_id);
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for State {}

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(clause) = &self.clause {
            write!(f, "{}", clause)
        } else {
            write!(f, "<empty>")
        }
    }
}

// ==================== Automaton ====================

#[derive(Debug, Clone)]
pub struct Automaton {
    pub states: FxHashSet<State>,
    pub initial: Option<State>,
    pub transitions: FxHashSet<Transition>,
    pub conflict_found: bool,
}

impl Automaton {
    pub fn new(contract: Contract) -> Self {
        let full_contract = contract.get_full_contract();
        let initial = full_contract.map(|clause| State::with_auto_id(Some(clause)));
        
        let mut states = FxHashSet::default();
        if let Some(ref initial_state) = initial {
            states.insert(initial_state.clone());
        }

        Automaton {
            states,
            initial,
            transitions: FxHashSet::default(),
            conflict_found: false,
        }
    }

    pub fn add_state(&mut self, state: State) -> bool {
        self.states.insert(state)
    }

    pub fn add_transition(&mut self, transition: Transition) -> bool {
        self.transitions.insert(transition)
    }

    pub fn get_state_by_clause(&self, clause: &Clause) -> Option<&State> {
        self.states.iter().find(|s| {
            if let Some(ref state_clause) = s.clause {
                state_clause == clause
            } else {
                false
            }
        })
    }

    pub fn get_state_by_id(&self, id: usize) -> Option<&State> {
        self.states.iter().find(|s| s.id == id)
    }

    /// Atualiza um estado existente usando uma closure
    /// 
    /// # Exemplo
    /// ```
    /// automaton.update_state(state_id, |state| {
    ///     state.situation = StateSituation::Conflicting;
    ///     state.push_trace(transition_id);
    /// });
    /// ```
    /// 
    /// Retorna `true` se o estado foi encontrado e atualizado, `false` caso contrário
    pub fn update_state<F>(&mut self, id: usize, f: F) -> bool
    where
        F: FnOnce(&mut State),
    {
        if let Some(state) = self.states.take(&State {
            id,
            clause: None,
            situation: StateSituation::NotChecked,
            conflict_information: None,
            trace: Vec::new(),
        }) {
            let mut state = state;
            f(&mut state);
            self.states.insert(state);
            true
        } else {
            false
        }
    }

    pub fn get_state_by_id_mut(&mut self, id: usize) -> Option<State> {
        self.states.take(&State {
            id,
            clause: None,
            situation: StateSituation::NotChecked,
            conflict_information: None,
            trace: Vec::new(),
        })
    }

    pub fn replace_state(&mut self, state: State) -> bool {
        self.states.replace(state).is_some()
    }

    pub fn get_conflicts(&self) -> Vec<&State> {
        self.states
            .iter()
            .filter(|s| s.situation == StateSituation::Conflicting)
            .collect()
    }

    pub fn get_transition_by_id(&self, id: usize) -> Option<&Transition> {
        self.transitions.iter().find(|t| t.id == id)
    }
}

impl fmt::Display for Automaton {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "States: {}", self.states.len())?;
        writeln!(f, "Transitions: {}", self.transitions.len())?;
        writeln!(f, "Conflicting: {}", self.conflict_found)?;
        Ok(())
    }
}