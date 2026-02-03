use rustc_hash::FxHashSet;
use std::fmt;
use std::sync::Arc;
use crate::model::actions::*;
use crate::utils::*;

// ==================== Enums ====================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ClauseCompositionType {
    Xor,
    Or,
    And,
    None,
}

impl fmt::Display for ClauseCompositionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClauseCompositionType::Xor => write!(f, "XOR"),
            ClauseCompositionType::Or => write!(f, "OR"),
            ClauseCompositionType::And => write!(f, "AND"),
            ClauseCompositionType::None => write!(f, "NONE"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConflictType {
    Global,
    Relativized,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeonticClauseType {
    Obligation,
    Permission,
    Prohibition,
}

impl DeonticClauseType {
    pub fn symbol(&self) -> &str {
        match self {
            DeonticClauseType::Obligation => "OBLIGATION",
            DeonticClauseType::Permission => "PERMISSION",
            DeonticClauseType::Prohibition => "PROHIBITION",
        }
    }

    pub fn short_symbol(&self) -> &str {
        match self {
            DeonticClauseType::Obligation => "O",
            DeonticClauseType::Permission => "P",
            DeonticClauseType::Prohibition => "F",
        }
    }
}

impl fmt::Display for DeonticClauseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelativizationType {
    Global,
    Relativized,
    Directed,
}

// ==================== ClauseComposition ====================

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ClauseComposition {
    pub composition_type: ClauseCompositionType,
    pub other: Arc<Clause>,
}

impl ClauseComposition {
    pub fn new(composition_type: ClauseCompositionType, other: Clause) -> Self {
        ClauseComposition {
            composition_type,
            other: Arc::new(other),
        }
    }
}
impl fmt::Display for ClauseComposition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " {} {}", self.composition_type, self.other)
    }
}

// ==================== Clause ====================

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Clause {
    Boolean {
        value: bool,
        composition: Option<ClauseComposition>,
    },
    Deontic {
        sender: i32,
        receiver: i32,
        relativization_type: RelativizationType,
        action: Action,
        deontic_type: DeonticClauseType,
        penalty: Option<Arc<Clause>>,
        composition: Option<ClauseComposition>,
    },
    Dynamic {
        sender: i32,
        receiver: i32,
        relativization_type: RelativizationType,
        action: Action,
        clause: Arc<Clause>,
        composition: Option<ClauseComposition>,
    },
}

impl Clause {
    pub fn boolean_true() -> Self {
        Clause::Boolean {
            value: true,
            composition: None,
        }
    }

    pub fn boolean_false() -> Self {
        Clause::Boolean {
            value: false,
            composition: None,
        }
    }

    pub fn deontic_global(
        action: Action,
        deontic_type: DeonticClauseType,
        penalty: Option<Clause>,
    ) -> Self {
        Clause::Deontic {
            sender: -1,
            receiver: -1,
            relativization_type: RelativizationType::Global,
            action,
            deontic_type,
            penalty: penalty.map(Arc::new),
            composition: None,
        }
    }

    pub fn deontic_relativized(
        sender: i32,
        action: Action,
        deontic_type: DeonticClauseType,
        penalty: Option<Clause>,
    ) -> Self {
        Clause::Deontic {
            sender,
            receiver: -1,
            relativization_type: RelativizationType::Relativized,
            action,
            deontic_type,
            penalty: penalty.map(Arc::new),
            composition: None,
        }
    }

    pub fn deontic_directed(
        sender: i32,
        receiver: i32,
        action: Action,
        deontic_type: DeonticClauseType,
        penalty: Option<Clause>,
    ) -> Self {
        let relativization_type = if sender < 0 {
            RelativizationType::Global
        } else if receiver < 0 {
            RelativizationType::Relativized
        } else {
            RelativizationType::Directed
        };

        Clause::Deontic {
            sender,
            receiver,
            relativization_type,
            action,
            deontic_type,
            penalty: penalty.map(Arc::new),
            composition: None,
        }
    }

    pub fn dynamic_global(action: Action, clause: Clause) -> Self {
        Clause::Dynamic {
            sender: -1,
            receiver: -1,
            relativization_type: RelativizationType::Global,
            action,
            clause: Arc::new(clause),
            composition: None,
        }
    }

    pub fn dynamic_relativized(sender: i32, action: Action, clause: Clause) -> Self {
        Clause::Dynamic {
            sender,
            receiver: -1,
            relativization_type: RelativizationType::Relativized,
            action,
            clause: Arc::new(clause),
            composition: None,
        }
    }

    pub fn dynamic_directed(sender: i32, receiver: i32, action: Action, clause: Clause) -> Self {
        let relativization_type = if sender < 0 {
            RelativizationType::Global
        } else if receiver < 0 {
            RelativizationType::Relativized
        } else {
            RelativizationType::Directed
        };

        Clause::Dynamic {
            sender,
            receiver,
            relativization_type,
            action,
            clause: Arc::new(clause),
            composition: None,
        }
    }

    // Métodos auxiliares
    pub fn get_receiver(&self) -> &i32 {
        match self {
            Clause::Deontic { receiver, .. } => receiver,
            Clause::Dynamic { receiver, .. } => receiver,
            Clause::Boolean { .. } => &0,
        }
    }

    pub fn get_sender(&self) -> &i32 {
        match self {
            Clause::Deontic { sender, .. } => sender,
            Clause::Dynamic { sender, .. } => sender,
            Clause::Boolean { .. } => &0,
        }
    }

    pub fn set_composition(&mut self, composition: ClauseComposition) {
        match self {
            Clause::Boolean { composition: c, .. } => *c = Some(composition),
            Clause::Deontic { composition: c, .. } => *c = Some(composition),
            Clause::Dynamic { composition: c, .. } => *c = Some(composition),
        }
    }

    pub fn get_composition(&self) -> Option<&ClauseComposition> {
        match self {
            Clause::Boolean { composition, .. } => composition.as_ref(),
            Clause::Deontic { composition, .. } => composition.as_ref(),
            Clause::Dynamic { composition, .. } => composition.as_ref(),
        }
    }

    pub fn get_composition_mut(&mut self) -> Option<&mut ClauseComposition> {
        match self {
            Clause::Boolean { composition, .. } => composition.as_mut(),
            Clause::Deontic { composition, .. } => composition.as_mut(),
            Clause::Dynamic { composition, .. } => composition.as_mut(),
        }
    }

    pub fn get_tail(&self) -> &Clause {
        if let Some(comp) = self.get_composition() {
            let mut tail = comp.other.as_ref();
            while let Some(next_comp) = tail.get_composition() {
                tail = next_comp.other.as_ref();
            }
            tail
        } else {
            self
        }
    }

    pub fn contains(&self, comp_type: ClauseCompositionType, target: &Clause) -> bool {
        let mut current = self.clone(); 
        current.set_composition_to_none();
        
        if &current == target {
            return true;
        }

        if let Some(comp) = self.get_composition() {
            if comp.composition_type != comp_type {
                return false;
            }
            return comp.other.contains(comp_type, target);
        }

        false
    }

    pub fn set_composition_to_none(&mut self) {
        match self {
            Clause::Boolean { composition, .. } => *composition = None,
            Clause::Deontic { composition, .. } => *composition = None,
            Clause::Dynamic { composition, .. } => *composition = None,
        }
    }

    fn format_individual(id: i32, symbol_table: &SymbolTable) -> String {
        if id < 0 {
            "GLOBAL".to_string()
        } else {
            symbol_table.get_symbol_by_id(id)
                .map(|s| s.value.clone())
                .unwrap_or_else(|| "GLOBAL".to_string())
        }
    }

    fn format_relativization(&self, symbol_table: &SymbolTable) -> String {
        match self {
            Clause::Boolean { .. } => String::new(),
            Clause::Deontic { sender, receiver, relativization_type, .. } |
            Clause::Dynamic { sender, receiver, relativization_type, .. } => {
                match relativization_type {
                    RelativizationType::Global => String::new(),
                    RelativizationType::Relativized => {
                        format!("{{{}}}", Self::format_individual(*sender, symbol_table))
                    }
                    RelativizationType::Directed => {
                        format!("{{{},{}}}", 
                            Self::format_individual(*sender, symbol_table),
                            Self::format_individual(*receiver, symbol_table))
                    }
                }
            }
        }
    }
}

impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Clause::Boolean { value, composition } => {
                write!(f, "{}", if *value { "T" } else { "F" })?;
                if let Some(comp) = composition {
                    write!(f, "{}", comp)?;
                }
                Ok(())
            }
            Clause::Deontic { deontic_type, action, penalty, composition, .. } => {
                let symbol_table = SymbolTable::instance();
                let table = symbol_table.lock().unwrap();
                let relativization = self.format_relativization(&table);
                drop(table); 
                
                write!(f, "{}{}", relativization, deontic_type)?;
                write!(f, "({})", action)?; 
                if let Some(pen) = penalty {
                    write!(f, "_/{}/_", pen)?;
                }
                if let Some(comp) = composition {
                    write!(f, "{}", comp)?;
                }
                Ok(())
            }
            Clause::Dynamic { action, clause, composition, .. } => {
                let symbol_table = SymbolTable::instance();
                let table = symbol_table.lock().unwrap();
                let relativization = self.format_relativization(&table);
                drop(table); 
                
                write!(f, "{}[{}]({})", relativization, action, clause)?;
                if let Some(comp) = composition {
                    write!(f, "{}", comp)?;
                }
                Ok(())
            }
        }
    }
}


// ==================== Conflict ====================
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Conflict {
    pub a: BasicAction,
    pub b: BasicAction,
    pub conflict_type: ConflictType,
}

impl Conflict {
    pub fn new(a: BasicAction, b: BasicAction, conflict_type: ConflictType) -> Self {
        Conflict { a, b, conflict_type }
    }
}

impl fmt::Display for Conflict {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let conflict_type_str = match self.conflict_type {
            ConflictType::Global => "GLOBAL",
            ConflictType::Relativized => "RELATIVIZED",
        };
        write!(f, "({},{}: {})", self.a, self.b, conflict_type_str)
    }
}

impl std::hash::Hash for Conflict {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.conflict_type.hash(state);
        let (first, second) = if self.a.value <= self.b.value {
            (&self.a, &self.b)
        } else {
            (&self.b, &self.a)
        };
        first.hash(state);
        second.hash(state);
    }
}

// ==================== Contract ====================

#[derive(Debug, Clone)]
pub struct Contract {
    pub clauses: FxHashSet<Clause>,
    pub global_conflicts: Vec<Conflict>,
    pub relativized_conflicts: Vec<Conflict>,
    pub individuals: FxHashSet<i32>,
    pub actions: FxHashSet<BasicAction>, 
}

impl Contract {
    pub fn new() -> Self {
        Contract {
            clauses: FxHashSet::default(),
            global_conflicts: Vec::new(),
            relativized_conflicts: Vec::new(),
            individuals: FxHashSet::default(),
            actions: FxHashSet::default(),
        }
    }

    pub fn with_clauses(clauses: Vec<Clause>) -> Self {
        let mut contract = Self::new();
        for clause in clauses {
            contract.add_clause(clause);
        }
        contract
    }

    pub fn add_clause(&mut self, clause: Clause) -> bool {
        self.extract_from_clause(&clause);
        self.clauses.insert(clause)
    }

    fn extract_from_clause(&mut self, clause: &Clause) {
        match clause {
            Clause::Boolean { composition, .. } => {
                if let Some(comp) = composition {
                    self.extract_from_clause(&comp.other);
                }
            }
            Clause::Deontic { sender, receiver, action, penalty, composition, .. } => {
                if *sender >= 0 {
                    self.individuals.insert(*sender);
                }
                if *receiver >= 0 {
                    self.individuals.insert(*receiver);
                }
                
                for basic_action in action.get_basic_actions() {
                    if !basic_action.skip && !basic_action.violation {
                        self.actions.insert(basic_action);
                    }
                }
                
                if let Some(pen) = penalty {
                    self.extract_from_clause(pen);
                }
                
                if let Some(comp) = composition {
                    self.extract_from_clause(&comp.other);
                }
            }
            Clause::Dynamic { sender, receiver, action, clause: inner_clause, composition, .. } => {
                if *sender >= 0 {
                    self.individuals.insert(*sender);
                }
                if *receiver >= 0 {
                    self.individuals.insert(*receiver);
                }
                
                for basic_action in action.get_basic_actions() {
                    if !basic_action.skip && !basic_action.violation {
                        self.actions.insert(basic_action);
                    }
                }
                
                self.extract_from_clause(inner_clause);
                
                if let Some(comp) = composition {
                    self.extract_from_clause(&comp.other);
                }
            }
        }
    }

    pub fn get_all_conflicts(&self) -> Vec<Conflict> {
        let mut conflicts = self.global_conflicts.clone();
        conflicts.extend(self.relativized_conflicts.clone());
        conflicts
    }

    fn java_string_hashcode(s: &str) -> i32 {
        let mut hash: i32 = 0;
        for c in s.chars() {
            hash = hash.wrapping_mul(31).wrapping_add(c as i32);
        }
        hash
    }
    
    /// Determina categoria da cláusula para agrupamento
    fn clause_category(clause_str: &str) -> u8 {
        if clause_str.contains("[!") {
            0  // Negações primeiro
        } else if clause_str.contains("PROHIBITION") {
            1  // PROHIBITIONs
        } else if clause_str.contains("OBLIGATION") {
            2  // OBLIGATIONs
        } else {
            3  // Outros
        }
    }

    pub fn get_full_contract(&self) -> Option<Clause> {
        if self.clauses.is_empty() {
            return None;
        }
        
        let mut clauses: Vec<_> = self.clauses.iter().collect();
        
        // Ordena primeiro por categoria, depois por hash (como Java)
        clauses.sort_by_cached_key(|c| {
            let s = format!("{}", c);
            let category = Self::clause_category(&s);
            let hash = Self::java_string_hashcode(&s);
            (category, hash)
        });
        
        let mut iter = clauses.into_iter();
        let first = iter.next().unwrap().clone();
        
        let mut head = first;
        for clause in iter {
            Self::append_clause_recursive(&mut head, clause.clone());
        }
        
        Some(head)
    }

    fn append_clause_recursive(target: &mut Clause, other: Clause) {
        if let Some(comp) = target.get_composition_mut() {
            Self::append_clause_recursive(Arc::make_mut(&mut comp.other), other);
        } else {
            target.set_composition(ClauseComposition::new(
                ClauseCompositionType::And,
                other
            ));
        }
    }
}

impl Default for Contract {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for Contract {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.clauses.is_empty() {
            write!(f, "Empty contract")
        } else {
            writeln!(f)?;
            for clause in &self.clauses {
                writeln!(f, "  {}", clause)?;
            }
            writeln!(f, "Conflicts:")?;
            
            write!(f, "\tGlobal: [")?;
            for (i, conflict) in self.global_conflicts.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", conflict)?;
            }
            writeln!(f, "]")?;
            
            write!(f, "\tRelativized: [")?;
            for (i, conflict) in self.relativized_conflicts.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", conflict)?;
            }
            writeln!(f, "]")?;
            
            Ok(())
        }
    }
}
