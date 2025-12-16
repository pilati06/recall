use std::fmt;
use crate::SymbolTable;

// ==================== ActionOperator ====================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ActionOperator {
    Choice,
    Concurrency,
    Sequence,
    Negation,
    Star,
    None,
}

impl fmt::Display for ActionOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ActionOperator::Choice => write!(f, "+"),
            ActionOperator::Concurrency => write!(f, "&"),
            ActionOperator::Negation => write!(f, "!"),
            ActionOperator::Sequence => write!(f, "."),
            ActionOperator::Star => write!(f, "*"),
            ActionOperator::None => write!(f, "NONE"),
        }
    }
}

// ==================== Action Trait ====================

pub trait ActionTrait: fmt::Display + fmt::Debug {
    fn get_basic_actions(&self) -> Vec<BasicAction>;
}

// ==================== BasicAction ====================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicAction {
    pub value: i32,
    pub violation: bool,
    pub skip: bool,
    pub negation: bool,
    pub concurrent_actions: Vec<BasicAction>,
}

impl BasicAction {
    pub const fn skip() -> Self {
        BasicAction {
            value: 0,
            violation: false,
            skip: true,
            negation: false,
            concurrent_actions: Vec::new(),
        }
    }

    pub const fn violation() -> Self {
        BasicAction {
            value: -1,
            violation: true,
            skip: false,
            negation: false,
            concurrent_actions: Vec::new(),
        }
    }

    pub fn new(value: i32, violation: bool, skip: bool, negation: bool) -> Self {
        BasicAction {
            value,
            violation,
            skip,
            negation,
            concurrent_actions: Vec::new(),
        }
    }

    pub fn with_value(value: i32) -> Self {
        BasicAction::new(value, false, false, false)
    }

    pub fn format_with_symbols(&self, symbol_table: &SymbolTable) -> String {
        if self.violation {
            return "VIOLATION".to_string();
        }
        if self.skip || self.value == 0 {
            return "SKIP".to_string();
        }
        
        let symbol_str = symbol_table.get_symbol_by_id(self.value)
            .map(|s| s.value.as_str())
            .unwrap_or("UNDEF");
        
        if self.negation {
            format!("!{}", symbol_str)
        } else {
            symbol_str.to_string()
        }
    }

    pub fn negate(&self) -> Self {
        BasicAction {
            value: self.value,
            violation: self.violation,
            skip: self.skip,
            negation: !self.negation,
            concurrent_actions: self.concurrent_actions.clone(),
        }
    }
}

// ==================== Display para BasicAction ====================
impl fmt::Display for BasicAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.skip {
            return write!(f, "1");
        }
        if self.violation {
            return write!(f, "0");
        }
        
        let symbol_table = SymbolTable::instance();
        let table = symbol_table.lock().unwrap();
        
        let prefix = if self.negation { "!" } else { "" };
        
        if let Some(symbol) = table.get_symbol_by_id(self.value) {
            write!(f, "{}{}", prefix, symbol.value())
        } else {
            write!(f, "{}UNDEF", prefix)
        }
    }
}

impl ActionTrait for BasicAction {
    fn get_basic_actions(&self) -> Vec<BasicAction> {
        let mut actions = Vec::new();
        if !self.skip {
            actions.push(BasicAction::with_value(self.value));
        }
        actions.extend(self.concurrent_actions.clone());
        actions
    }
}

// ==================== ComposedAction ====================

#[derive(Debug, Clone, PartialEq)]
pub struct ComposedAction {
    pub left: Option<Box<Action>>,
    pub right: Option<Box<Action>>,
    pub operator: ActionOperator,
}

impl ComposedAction {
    pub fn new(left: Option<Action>, right: Option<Action>, operator: ActionOperator) -> Self {
        ComposedAction {
            left: left.map(Box::new),
            right: right.map(Box::new),
            operator,
        }
    }

    pub fn binary(left: Action, right: Action, operator: ActionOperator) -> Self {
        ComposedAction::new(Some(left), Some(right), operator)
    }

    pub fn unary(action: Action, operator: ActionOperator) -> Self {
        ComposedAction::new(Some(action), None, operator)
    }

    pub fn empty() -> Self {
        ComposedAction::new(None, None, ActionOperator::None)
    }
}

impl fmt::Display for ComposedAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (&self.left, &self.right, self.operator) {
            (None, Some(right), _) => write!(f, "{}", right),
            
            (Some(left), None, ActionOperator::Star) => {
                write!(f, "{}*", left)
            }

            (Some(left), None, ActionOperator::Negation) => {
                write!(f, "{}{}", self.operator, left)
            }
            
            (Some(left), Some(right), _) => {
                write!(f, "({} {} {})", left, self.operator, right)
            }
            
            (Some(left), None, _) => write!(f, "{}", left),
            
            (None, None, _) => write!(f, ""),
        }
    }
}

impl ActionTrait for ComposedAction {
    fn get_basic_actions(&self) -> Vec<BasicAction> {
        let mut actions = Vec::new();
        
        if let Some(ref left) = self.left {
            actions.extend(left.get_basic_actions());
        }
        
        if let Some(ref right) = self.right {
            actions.extend(right.get_basic_actions());
        }
        
        actions
    }
}

// ==================== Action Enum ====================

#[derive(Debug, Clone, PartialEq)]
pub enum Action {
    Basic(BasicAction),
    Composed(ComposedAction),
}

impl Action {
    pub fn get_basic_actions(&self) -> Vec<BasicAction> {
        match self {
            Action::Basic(ba) => ba.get_basic_actions(),
            Action::Composed(ca) => ca.get_basic_actions(),
        }
    }

    pub fn choice(left: Action, right: Action) -> Self {
        Action::Composed(ComposedAction::binary(left, right, ActionOperator::Choice))
    }

    pub fn concurrency(left: Action, right: Action) -> Self {
        Action::Composed(ComposedAction::binary(left, right, ActionOperator::Concurrency))
    }

    pub fn sequence(left: Action, right: Action) -> Self {
        Action::Composed(ComposedAction::binary(left, right, ActionOperator::Sequence))
    }

    pub fn negation(action: Action) -> Self {
        Action::Composed(ComposedAction::unary(action, ActionOperator::Negation))
    }

    pub fn star(action: Action) -> Self {
        Action::Composed(ComposedAction::unary(action, ActionOperator::Star))
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Basic(ba) => write!(f, "{}", ba),
            Action::Composed(ca) => {
                if ca.operator == ActionOperator::Star {
                    if let Some(ref left) = ca.left {
                        if let Action::Composed(inner) = left.as_ref() {
                            if inner.operator == ActionOperator::Negation {
                                return write!(f, "!{}*", inner.left.as_ref().unwrap());
                            }
                        }
                    }
                }
                write!(f, "{}", ca)
            }
        }
    }
}

impl std::hash::Hash for Action {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Action::Basic(ba) => {
                "basic".hash(state);
                ba.hash(state);
            }
            Action::Composed(ca) => {
                "composed".hash(state);
                ca.operator.hash(state);
                if let Some(ref left) = ca.left {
                    left.as_ref().hash(state);
                }
                if let Some(ref right) = ca.right {
                    right.as_ref().hash(state);
                }
            }
        }
    }
}

impl Eq for Action {}

// ==================== RelativizedAction ====================

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelativizedAction {
    pub negation: bool,
    pub sender: i32,
    pub action: BasicAction,
    pub receiver: i32,
}

impl RelativizedAction {
    pub fn new(sender: i32, action: BasicAction, receiver: i32) -> Self {
        RelativizedAction {
            negation: false,
            sender,
            action,
            receiver,
        }
    }

    pub fn negation(action: &RelativizedAction) -> Self {
        RelativizedAction {
            negation: true,
            sender: action.sender,
            action: action.action.clone(),
            receiver: action.receiver,
        }
    }

    pub fn format_with_symbols(&self, symbol_table: &SymbolTable) -> String {
        let action_str = if self.negation {
            format!("!{}", self.action.format_with_symbols(symbol_table))
        } else {
            self.action.format_with_symbols(symbol_table)
        };

        let sender_str = symbol_table.get_symbol_by_id(self.sender)
            .map(|s| s.value.as_str())
            .unwrap_or("?");
        
        let receiver_str = symbol_table.get_symbol_by_id(self.receiver)
            .map(|s| s.value.as_str())
            .unwrap_or("?");

        format!("({}, {}, {})", sender_str, action_str, receiver_str)
    }
}

impl fmt::Display for RelativizedAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbol_table = SymbolTable::instance();
        let table = symbol_table.lock().unwrap();
        write!(f, "{}", self.format_with_symbols(&table))
    }
}