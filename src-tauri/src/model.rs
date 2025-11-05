#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct Contract {
    pub clauses: Vec<Clause>,
    pub global_conflicts: Vec<Conflict>,
    pub relativized_conflicts: Vec<Conflict>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct Conflict {
    pub act1: Action,
    pub act2: Action,
    pub ctype: ConflictType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum ConflictType {
    Global,
    Relativized,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Clause {
    True,
    False,
    And(Box<Clause>, Box<Clause>),
    Or(Box<Clause>, Box<Clause>),
    Xor(Box<Clause>, Box<Clause>),
    Deontic(DeonticClause),
    Dynamic(DynamicClause),
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct DeonticClause {
    pub relation: Relation,
    pub operator: DeonticOperator,
    pub action: Action,
    pub penalty: Box<Clause>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct DynamicClause {
    pub relation: Relation,
    pub action: Action,
    pub compensation: Option<Box<Clause>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum DeonticOperator {
    Obligation,
    Permission,
    Prohibition,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Action {
    Basic(BasicAction),
    Composed(ComposedAction),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum BasicAction {
    Skip,
    Violation,
    Id(String),
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub struct ComposedAction {
    pub left: Box<Action>,
    pub right: Option<Box<Action>>,
    pub op: ActionOperator,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum ActionOperator {
    Choice,
    Sequence,
    Concurrency,
    Iteration,
    Negation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Relation {
    Empty,
    Single(String),
    Pair(String, String),
}
// --- FIM DAS DEFINIÇÕES DA AST ---


// --- 2. Erros do Construtor do AST ---
#[derive(Error, Debug)]
pub enum AstError {
    #[error("Erro de construção do AST: {0}")]
    BuildError(String),
    
    #[error("Regra inesperada: esperava {expected:?}, encontrou {found:?}")]
    UnexpectedRule {
        expected: Rule,
        found: Rule,
    },
}

type Result<T> = std::result::Result<T, AstError>;