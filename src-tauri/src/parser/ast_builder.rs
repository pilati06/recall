use pest::iterators::{Pair, Pairs};
use thiserror::Error;
use crate::parser::Rule;

// --- 1. Definições da AST (COPIE AS SUAS STRUCTS E ENUMS AQUI) ---
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

// --- 3. O Construtor do AST (Funções de "Visitor") ---

pub fn build_ast(pair: Pair<Rule>) -> Result<Contract> {
    if pair.as_rule() != Rule::main {
        return Err(AstError::UnexpectedRule {
            expected: Rule::main,
            found: pair.as_rule(),
        });
    }
    let inner_pair = pair.into_inner().next().ok_or_else(|| AstError::BuildError("Ficheiro de contrato vazio.".to_string()))?;
    build_contract(inner_pair)
}

fn build_contract(pair: Pair<Rule>) -> Result<Contract> {
    let mut global_conflicts = Vec::new();
    let mut relativized_conflicts = Vec::new();
    let mut clauses = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::conflict => {
                for conflict_part in inner_pair.into_inner() {
                    match conflict_part.as_rule() {
                        Rule::conflict_body => {
                            for body_part in conflict_part.into_inner() {
                                match body_part.as_rule() {
                                    Rule::cfGlobal_block => {
                                        for p in body_part.into_inner() {
                                            if p.as_rule() == Rule::cfPair {
                                                let (act1, act2) = build_cf_pair(p)?;
                                                global_conflicts.push(Conflict {
                                                    act1,
                                                    act2,
                                                    ctype: ConflictType::Global,
                                                });
                                            }
                                        }
                                    }
                                    Rule::cfRel_block => {
                                        for p in body_part.into_inner() {
                                            if p.as_rule() == Rule::cfPair {
                                                let (act1, act2) = build_cf_pair(p)?;
                                                relativized_conflicts.push(Conflict {
                                                    act1,
                                                    act2,
                                                    ctype: ConflictType::Relativized,
                                                });
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            Rule::clause => {
                clauses.push(build_clause(inner_pair)?);
            }
            Rule::EOI | Rule::END => {}
            _ => return Err(AstError::BuildError(format!("Regra inesperada em contract: {:?}", inner_pair.as_rule()))),
        }
    }

    Ok(Contract {
        clauses,
        global_conflicts,
        relativized_conflicts,
    })
}

fn build_cf_pair(pair: Pair<Rule>) -> Result<(Action, Action)> {
    let mut pairs = pair.into_inner();
    let id1 = pairs.next().ok_or_else(|| AstError::BuildError("Falta ID1 em cfPair".to_string()))?.as_str().to_string();
    let id2 = pairs.next().ok_or_else(|| AstError::BuildError("Falta ID2 em cfPair".to_string()))?.as_str().to_string();
    
    let act1 = Action::Basic(BasicAction::Id(id1));
    let act2 = Action::Basic(BasicAction::Id(id2));
    
    Ok((act1, act2))
}

// --- Funções de árvore infixa (SEGURAS) ---

fn build_infix_tree<F, G>(
    mut pairs: Pairs<Rule>,
    build_term: F,
    op_map: G,
) -> Result<Clause>
where
    F: Fn(Pair<Rule>) -> Result<Clause>,
    G: Fn(Rule) -> Box<dyn Fn(Box<Clause>, Box<Clause>) -> Clause>,
{
    let left_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        "Estrutura de árvore infixa incompleta: esperava um termo à esquerda.".to_string()
    ))?;
    let mut left = build_term(left_pair)?;

    while let Some(op_pair) = pairs.next() {
        let op_rule = op_pair.as_rule();
        
        let right_pair = pairs.next().ok_or_else(|| AstError::BuildError(
            format!("Estrutura de árvore infixa incompleta: esperava um termo à direita após o operador {:?}.", op_rule)
        ))?;
        
        let right = build_term(right_pair)?;
        let op_fn = op_map(op_rule);
        left = op_fn(Box::new(left), Box::new(right));
    }
    Ok(left)
}

fn build_clause(pair: Pair<Rule>) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        build_clause_term,
        |rule| match rule {
            Rule::AND => Box::new(Clause::And),
            _ => unreachable!("Operador de cláusula desconhecido: {:?}", rule),
        },
    )
}

fn build_co(pair: Pair<Rule>) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        build_co_atom,
        |rule| match rule {
            Rule::XOR => Box::new(Clause::Xor),
            _ => unreachable!("Operador CO desconhecido: {:?}", rule),
        },
    )
}

fn build_cp(pair: Pair<Rule>) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        build_cp_atom,
        |rule| match rule {
            Rule::XOR => Box::new(Clause::Xor),
            _ => unreachable!("Operador CP desconhecido: {:?}", rule),
        },
    )
}

fn build_cf(pair: Pair<Rule>) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        build_cf_term,
        |rule| match rule {
            Rule::OR => Box::new(Clause::Or),
            _ => unreachable!("Operador CF desconhecido: {:?}", rule),
        },
    )
}

// --- Construtores de Termos (Folhas das expressões) ---

fn build_clause_term(pair: Pair<Rule>) -> Result<Clause> {
    let inner = pair.into_inner().next().ok_or_else(|| AstError::BuildError("Termo de cláusula vazio.".to_string()))?;
    match inner.as_rule() {
        Rule::co => build_co(inner),
        Rule::cp => build_cp(inner),
        Rule::cf => build_cf(inner),
        Rule::cd => build_cd(inner),
        Rule::T => Ok(Clause::True),
        Rule::F => Ok(Clause::False),
        // A gramática tem 'OPEN_EXP ~ clause ~ CLOSE_EXP'
        Rule::OPEN_EXP => {
            let mut pairs = inner.into_inner();
            pairs.next(); // Salta OPEN_EXP
            let clause_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'clause' dentro de '(...)'".to_string()))?;
            build_clause(clause_pair)
        }
        _ => Err(AstError::BuildError(format!("Termo de cláusula inesperado: {:?}", inner.as_rule()))),
    }
}

fn build_cf_term(pair: Pair<Rule>) -> Result<Clause> {
    let inner = pair.into_inner().next().ok_or_else(|| AstError::BuildError("Termo CF vazio.".to_string()))?;
    match inner.as_rule() {
        Rule::cd => build_cd(inner),
        Rule::cf_atom => build_cf_atom(inner),
        _ => Err(AstError::BuildError(format!("Termo CF inesperado: {:?}", inner.as_rule()))),
    }
}

// --- Construtores de Átomos (Regras base) ---

// ** VERSÃO CORRIGIDA DE build_cd **
fn build_cd(pair: Pair<Rule>) -> Result<Clause> {
    let mut pairs = pair.into_inner();
    let rule_name = "Dynamic Clause (cd)";

    // 1. Pega 'rel'
    let relation_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        format!("Erro em '{}': esperava uma Relação (rel).", rule_name)
    ))?;
    let relation = build_rel(relation_pair)?;

    // 2. Consome OPEN_DYN
    let open_dyn_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        format!("Erro em '{}': falta '[' (OPEN_DYN) após a relação.", rule_name)
    ))?;
    if open_dyn_pair.as_rule() != Rule::OPEN_DYN {
         return Err(AstError::BuildError(format!(
            "Erro em '{}': regra inesperada {:?}, esperava '['.", rule_name, open_dyn_pair.as_rule()
        )));
    }
    
    // 3. Pega 'beta'
    let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        format!("Erro em '{}': esperava uma Ação (beta) após '['.", rule_name)
    ))?;
    let action = build_beta(beta_pair)?;

    // 4. Consome CLOSE_DYN
    let close_dyn_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        format!("Erro em '{}': falta ']' (CLOSE_DYN) após a ação.", rule_name)
    ))?;
    if close_dyn_pair.as_rule() != Rule::CLOSE_DYN {
         return Err(AstError::BuildError(format!(
            "Erro em '{}': regra inesperada {:?}, esperava ']'.", rule_name, close_dyn_pair.as_rule()
        )));
    }

    // 5. Pega a compensação opcional ("achatada")
    let compensation = if let Some(open_exp_pair) = pairs.next() {
        if open_exp_pair.as_rule() != Rule::OPEN_EXP {
            return Err(AstError::BuildError(format!(
                "Erro em '{}': token inesperado {:?}, esperava '(' (OPEN_EXP) para a compensação.", rule_name, open_exp_pair.as_rule()
            )));
        }

        let clause_pair = pairs.next().ok_or_else(|| AstError::BuildError(
            format!("Erro em '{}': falta 'clause' dentro da compensação '(...)'", rule_name)
        ))?;
        let clause = build_clause(clause_pair)?;

        let close_exp_pair = pairs.next().ok_or_else(|| AstError::BuildError(
            format!("Erro em '{}': falta ')' (CLOSE_EXP) para fechar a compensação.", rule_name)
        ))?;
        if close_exp_pair.as_rule() != Rule::CLOSE_EXP {
                return Err(AstError::BuildError(format!(
                "Erro em '{}': regra inesperada {:?}, esperava ')'.", rule_name, close_exp_pair.as_rule()
            )));
        }
        Some(Box::new(clause))
    } else {
        None
    };

    Ok(Clause::Dynamic(DynamicClause {
        relation,
        action,
        compensation,
    }))
}

fn build_co_atom(pair: Pair<Rule>) -> Result<Clause> {
    let mut pairs = pair.into_inner();
    let relation = build_rel(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'rel' em 'co_atom'".to_string()))?)?;
    pairs.next(); // Ignora DEO_O
    pairs.next(); // Ignora OPEN_EXP
    let action = build_alpha(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'alpha' em 'co_atom'".to_string()))?)?;
    pairs.next(); // Ignora CLOSE_EXP
    let penalty = build_penalty(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'penalty' em 'co_atom'".to_string()))?)?;

    Ok(Clause::Deontic(DeonticClause {
        relation,
        operator: DeonticOperator::Obligation,
        action,
        penalty: Box::new(penalty),
    }))
}

fn build_cp_atom(pair: Pair<Rule>) -> Result<Clause> {
    let mut pairs = pair.into_inner();
    let relation = build_rel(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'rel' em 'cp_atom'".to_string()))?)?;
    pairs.next(); // Ignora DEO_P
    pairs.next(); // Ignora OPEN_EXP
    let action = build_alpha(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'alpha' em 'cp_atom'".to_string()))?)?;
    pairs.next(); // Ignora CLOSE_EXP
    
    Ok(Clause::Deontic(DeonticClause {
        relation,
        operator: DeonticOperator::Permission,
        action,
        penalty: Box::new(Clause::False), 
    }))
}

fn build_cf_atom(pair: Pair<Rule>) -> Result<Clause> {
    let mut pairs = pair.into_inner();
    let relation = build_rel(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'rel' em 'cf_atom'".to_string()))?)?;
    pairs.next(); // Ignora DEO_F
    pairs.next(); // Ignora OPEN_EXP
    let action = build_alpha(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'alpha' em 'cf_atom'".to_string()))?)?;
    pairs.next(); // Ignora CLOSE_EXP
    let penalty = build_penalty(pairs.next().ok_or_else(|| AstError::BuildError("Falta 'penalty' em 'cf_atom'".to_string()))?)?;

    Ok(Clause::Deontic(DeonticClause {
        relation,
        operator: DeonticOperator::Prohibition,
        action,
        penalty: Box::new(penalty),
    }))
}

// ** VERSÃO CORRIGIDA DE build_penalty **
fn build_penalty(pair: Pair<Rule>) -> Result<Clause> {
    // 'pair' é a regra 'penalty'. Vamos ver os seus filhos.
    let mut inner = pair.into_inner();
    
    // Tenta apanhar o primeiro filho.
    if let Some(first_child) = inner.next() {
        // CASO 2: O bloco _/.../_ EXISTE.
        // O first_child TEM de ser OPEN_PTY.
        
        if first_child.as_rule() != Rule::OPEN_PTY {
            return Err(AstError::BuildError(format!(
                "Erro de Gramática: 'penalty' esperava 'OPEN_PTY' (_/), mas encontrou {:?}", first_child.as_rule()
            )));
        }
        
        // O próximo filho TEM de ser 'clause'.
        let clause_pair = inner.next().ok_or_else(|| AstError::BuildError(
            "Erro de Gramática: 'penalty' continha '_/' mas faltava a 'clause'.".to_string()
        ))?;
        if clause_pair.as_rule() != Rule::clause {
             return Err(AstError::BuildError(format!("Erro de Gramática: 'penalty' esperava 'clause', mas encontrou {:?}", clause_pair.as_rule())));
        }
        let clause = build_clause(clause_pair)?;

        // O próximo filho TEM de ser 'CLOSE_PTY'.
        let close_pty_pair = inner.next().ok_or_else(|| AstError::BuildError(
            "Erro de Gramática: 'penalty' continha 'clause' mas faltava o '/_' final.".to_string()
        ))?;
        if close_pty_pair.as_rule() != Rule::CLOSE_PTY {
             return Err(AstError::BuildError(format!("Erro de Gramática: 'penalty' esperava 'CLOSE_PTY' (/_), mas encontrou {:?}", close_pty_pair.as_rule())));
        }

        Ok(clause)
        
    } else {
        // CASO 1: O bloco _/.../_ NÃO EXISTE.
        // inner.next() retornou None (penalty: ""), o que é permitido.
        Ok(Clause::False)
    }
}

fn build_rel(pair: Pair<Rule>) -> Result<Relation> {
    let mut pairs = pair.into_inner();
    match pairs.next() {
        None => Ok(Relation::Empty), // Alternativa vazia
        Some(first_pair) => {
            match first_pair.as_rule() {
                Rule::OPEN_REL => {
                    let id1_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava ID após '{' em 'rel'".to_string()))?;
                    let id1 = id1_pair.as_str().to_string();
                    
                    if let Some(sep_or_close) = pairs.next() {
                        match sep_or_close.as_rule() {
                            Rule::SEP_REL => {
                                let id2_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava ID após ',' em 'rel'".to_string()))?;
                                let id2 = id2_pair.as_str().to_string();
                                pairs.next().ok_or_else(|| AstError::BuildError("Falta '}' em 'rel' de par".to_string()))?;
                                Ok(Relation::Pair(id1, id2))
                            }
                            Rule::CLOSE_REL => {
                                Ok(Relation::Single(id1))
                            }
                            _ => Err(AstError::BuildError("Esperava ',' ou '}' em 'rel'".to_string()))
                        }
                    } else {
                        Err(AstError::BuildError("Falta '}' em 'rel'".to_string()))
                    }
                }
                _ => Err(AstError::BuildError(format!("Regra 'rel' inesperada: {:?}", first_pair.as_rule())))
            }
        }
    }
}


// --- Construtores de Ação (alpha, beta) ---

fn build_action_infix_tree<F>(
    mut pairs: Pairs<Rule>,
    build_term: F,
) -> Result<Action>
where
    F: Fn(Pair<Rule>) -> Result<Action>,
{
    let left_pair = pairs.next().ok_or_else(|| AstError::BuildError(
        "Estrutura de ação infixa incompleta: esperava um termo de ação à esquerda.".to_string()
    ))?;
    let mut left = build_term(left_pair)?;

    while let Some(op_pair) = pairs.next() {
        // 'op_pair' é a regra 'op'.
        
        // --- INÍCIO DA CORREÇÃO ---
        // Nós queremos o *filho* de 'op' (ex: OP_CHOICE, OP_SEQ, OP_CONC).
        let op_rule = op_pair.into_inner().next().ok_or_else(|| AstError::BuildError(
            "Regra 'op' de ação estava vazia.".to_string()
        ))?.as_rule();
        // --- FIM DA CORREÇÃO ---

        let right_pair = pairs.next().ok_or_else(|| AstError::BuildError(
            format!("Estrutura de ação infixa incompleta: esperava um termo de ação à direita após o operador {:?}.", op_rule)
        ))?;
        let right = build_term(right_pair)?;
        
        let op = match op_rule {
            Rule::OP_CHOICE => ActionOperator::Choice,
            Rule::OP_SEQ => ActionOperator::Sequence,
            Rule::OP_CONC => ActionOperator::Concurrency,
            // Agora este 'unreachable' está correto.
            _ => unreachable!("Operador de ação desconhecido (dentro de 'op'): {:?}", op_rule),
        };
        
        left = Action::Composed(ComposedAction {
            left: Box::new(left),
            right: Some(Box::new(right)),
            op,
        });
    }
    Ok(left)
}

fn build_alpha(pair: Pair<Rule>) -> Result<Action> {
    build_action_infix_tree(pair.into_inner(), build_alpha_atom)
}

fn build_alpha_atom(pair: Pair<Rule>) -> Result<Action> {
    let inner = pair.into_inner().next().ok_or_else(|| AstError::BuildError("Átomo alfa vazio.".to_string()))?;
    match inner.as_rule() {
        Rule::SKIP => Ok(Action::Basic(BasicAction::Skip)),
        Rule::VIOLATION => Ok(Action::Basic(BasicAction::Violation)),
        Rule::ID => Ok(Action::Basic(BasicAction::Id(inner.as_str().to_string()))),
        Rule::OPEN_EXP => {
            let mut pairs = inner.into_inner();
            pairs.next(); // Salta OPEN_EXP
            let alpha_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'alpha' dentro de '(...)'".to_string()))?;
            build_alpha(alpha_pair)
        }
        _ => Err(AstError::BuildError(format!("Átomo alfa inesperado: {:?}", inner.as_rule()))),
    }
}

fn build_beta(pair: Pair<Rule>) -> Result<Action> {
    build_action_infix_tree(pair.into_inner(), build_beta_term)
}

// ** VERSÃO CORRIGIDA DE build_beta_term **
fn build_beta_term(pair: Pair<Rule>) -> Result<Action> {
    // Corrigido para E0382: Salva a string de debug ANTES de mover 'pair'
    let pair_str_for_error = pair.as_str().to_string();
    
    let mut pairs = pair.into_inner();
    
    let first = pairs.next().ok_or_else(|| AstError::BuildError(
        format!("Termo beta inesperado: a regra '({:?})' estava vazia.", pair_str_for_error)
    ))?;

    // Esta lógica de 'match' deve corresponder à sua gramática .pest
    match first.as_rule() {

        Rule::beta => {
            build_beta(first)
        }
        
        // Caso: "(" ~ beta ~ ")" ~ "*"
        Rule::OPEN_EXP if pairs.clone().last().map_or(false, |p| p.as_rule() == Rule::UN_OP_IT) => {
            let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'beta' em '(...)*'".to_string()))?;
            let beta_action = build_beta(beta_pair)?;
            // Ignora o CLOSE_EXP e o UN_OP_IT
            Ok(Action::Composed(ComposedAction {
                left: Box::new(beta_action),
                right: None,
                op: ActionOperator::Iteration,
            }))
        }

        // Caso: "!" ~ "(" ~ beta ~ ")" ~ "*"
        Rule::UN_OP_NEG if pairs.clone().last().map_or(false, |p| p.as_rule() == Rule::UN_OP_IT) => {
            let _open_exp = pairs.next().ok_or_else(|| AstError::BuildError("Esperava '(' em '!(...)*'".to_string()))?;
            let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'beta' em '!(...)*'".to_string()))?;
            let action_to_negate = build_beta(beta_pair)?;
            // Ignora CLOSE_EXP e UN_OP_IT
            
            let negated_action = Action::Composed(ComposedAction {
                left: Box::new(action_to_negate),
                right: None,
                op: ActionOperator::Negation,
            });
            
            Ok(Action::Composed(ComposedAction {
                left: Box::new(negated_action),
                right: None,
                op: ActionOperator::Iteration,
            }))
        }

        // Caso: ID ~ "*"
        Rule::ID if pairs.clone().last().map_or(false, |p| p.as_rule() == Rule::UN_OP_IT) => {
            let id_action = Action::Basic(BasicAction::Id(first.as_str().to_string()));
            Ok(Action::Composed(ComposedAction {
                left: Box::new(id_action),
                right: None,
                op: ActionOperator::Iteration,
            }))
        }

        // Caso: SKIP ~ "*"
        Rule::SKIP if pairs.clone().last().map_or(false, |p| p.as_rule() == Rule::UN_OP_IT) => {
            let skip_action = Action::Basic(BasicAction::Skip);
            Ok(Action::Composed(ComposedAction {
                left: Box::new(skip_action),
                right: None,
                op: ActionOperator::Iteration,
            }))
        }

        // Caso: VIOLATION ~ "*"
        Rule::VIOLATION if pairs.clone().last().map_or(false, |p| p.as_rule() == Rule::UN_OP_IT) => {
            let viol_action = Action::Basic(BasicAction::Violation);
            Ok(Action::Composed(ComposedAction {
                left: Box::new(viol_action),
                right: None,
                op: ActionOperator::Iteration,
            }))
        }
        
        // --- Casos sem '*' (devem vir DEPOIS dos casos com '*') ---

        // Caso: ID | ID.beta
        Rule::ID => {
            let id_action = Action::Basic(BasicAction::Id(first.as_str().to_string()));
            match pairs.next() {
                None => Ok(id_action), // Regra: ID
                Some(op) => {
                    if op.as_rule() != Rule::OP_SEQ {
                        return Err(AstError::BuildError(format!("Token inesperado após ID em beta_term: {:?}", op.as_rule())));
                    }
                    // Regra: ID . beta
                    let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'beta' após 'ID.'".to_string()))?;
                    let beta_action = build_beta(beta_pair)?;
                    Ok(Action::Composed(ComposedAction {
                        left: Box::new(id_action),
                        right: Some(Box::new(beta_action)),
                        op: ActionOperator::Sequence,
                    }))
                }
            }
        }
        
        // Caso: SKIP
        Rule::SKIP => Ok(Action::Basic(BasicAction::Skip)),
        
        // Caso: VIOLATION
        Rule::VIOLATION => Ok(Action::Basic(BasicAction::Violation)),

        // Caso: !ID ou !(beta)
        Rule::UN_OP_NEG => { // 'first' é '!'
            let target = pairs.next().ok_or_else(|| AstError::BuildError("Esperava ID ou '(' após '!'".to_string()))?;
            
            let action_to_negate = match target.as_rule() {
                Rule::ID => { // Regra: !ID
                    Action::Basic(BasicAction::Id(target.as_str().to_string()))
                }
                Rule::OPEN_EXP => { // Regra: !(beta)
                    // Pares: [!, (, beta, )]
                    let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'beta' dentro de '!(...)'".to_string()))?;
                    let action = build_beta(beta_pair)?;
                    pairs.next().ok_or_else(|| AstError::BuildError("Faltando ')' para fechar '!(...)'".to_string()))?; // Consome ')'
                    action
                }
                rule => return Err(AstError::BuildError(format!("Token inesperado depois de ! em beta_term: {:?}", rule)))
            };
            
            Ok(Action::Composed(ComposedAction {
                left: Box::new(action_to_negate),
                right: None,
                op: ActionOperator::Negation,
            }))
        }

        // Caso: (beta)
        Rule::OPEN_EXP => { // 'first' é '('
            // Pares: [(, beta, )]
            let beta_pair = pairs.next().ok_or_else(|| AstError::BuildError("Esperava 'beta' dentro de '(...)'".to_string()))?;
            let beta_action = build_beta(beta_pair)?;
            pairs.next().ok_or_else(|| AstError::BuildError("Faltando ')' para fechar '(...)'".to_string()))?; // Salta o CLOSE_EXP ')'
            Ok(beta_action)
        }
        
        rule => Err(AstError::BuildError(format!("Termo beta inesperado (não tratado): {:?}", rule))),
    }
}