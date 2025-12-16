use pest::iterators::{Pair, Pairs};
use crate::parser::Rule;
use crate::model::contracts::*;
use crate::model::actions::*;
use crate::utils::*;
use std::fmt;

// ==================== Error Types ====================

#[derive(Debug)]
pub enum AstError {
    UnexpectedRule {
        expected: Rule,
        found: Rule,
    },
    BuildError(String),
    ParseError(String),
}

impl fmt::Display for AstError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstError::UnexpectedRule { expected, found } => {
                write!(f, "Unexpected rule. Expected {:?}, found {:?}", expected, found)
            }
            AstError::BuildError(msg) => write!(f, "Build error: {}", msg),
            AstError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for AstError {}

type Result<T> = std::result::Result<T, AstError>;

// ==================== AST Builder ====================

pub fn build_ast(pair: Pair<Rule>) -> Result<Contract> {
    if pair.as_rule() != Rule::main {
        return Err(AstError::UnexpectedRule {
            expected: Rule::main,
            found: pair.as_rule(),
        });
    }
    let inner_pair = pair
        .into_inner()
        .next()
        .ok_or_else(|| AstError::BuildError("Empty contract file.".to_string()))?;
    build_contract(inner_pair)
}

fn build_contract(pair: Pair<Rule>) -> Result<Contract> {
    let mut contract = Contract::new();
    let symbol_table = SymbolTable::instance();
    let mut table = symbol_table.lock().unwrap();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::conflict => {
                for conflict_part in inner_pair.into_inner() {
                    if conflict_part.as_rule() == Rule::conflict_body {
                        for body_part in conflict_part.into_inner() {
                            match body_part.as_rule() {
                                Rule::cfGlobal_block => {
                                    for p in body_part.into_inner() {
                                        if p.as_rule() == Rule::cfPair {
                                            let (act1, act2) = build_cf_pair(p, &mut table)?;
                                            contract.global_conflicts.push(Conflict::new(
                                                act1,
                                                act2,
                                                ConflictType::Global,
                                            ));
                                        }
                                    }
                                }
                                Rule::cfRel_block => {
                                    for p in body_part.into_inner() {
                                        if p.as_rule() == Rule::cfPair {
                                            let (act1, act2) = build_cf_pair(p, &mut table)?;
                                            contract.relativized_conflicts.push(Conflict::new(
                                                act1,
                                                act2,
                                                ConflictType::Relativized,
                                            ));
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            Rule::clause => {
                let clause = build_clause(inner_pair, &mut table)?;
                contract.add_clause(clause);
            }
            Rule::EOI | Rule::END => {}
            _ => {
                return Err(AstError::BuildError(format!(
                    "Unexpected rule in contract: {:?}",
                    inner_pair.as_rule()
                )))
            }
        }
    }

    Ok(contract)
}

fn build_cf_pair(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<(BasicAction, BasicAction)> {
    let mut pairs = pair.into_inner();
    let id1 = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing ID1 in cfPair".to_string()))?
        .as_str()
        .to_string();
    let id2 = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing ID2 in cfPair".to_string()))?
        .as_str()
        .to_string();

    let act1_id = table.add_symbol(id1, SymbolType::Action);
    let act2_id = table.add_symbol(id2, SymbolType::Action);

    Ok((BasicAction::with_value(act1_id), BasicAction::with_value(act2_id)))
}

// ==================== Infix Tree Builder ====================

fn build_infix_tree<F, G>(
    mut pairs: Pairs<Rule>,
    build_term: &F,
    op_map: &G,
    table: &mut SymbolTable,
) -> Result<Clause>
where
    F: Fn(Pair<Rule>, &mut SymbolTable) -> Result<Clause>,
    G: Fn(ClauseCompositionType) -> ClauseCompositionType,
{
    let first_pair = pairs.next().ok_or_else(|| {
        AstError::BuildError("Incomplete infix tree: expected left term.".to_string())
    })?;
    let first_term = build_term(first_pair, table)?;

    let mut terms = vec![first_term];
    let mut ops = vec![];

    while let Some(op_pair) = pairs.next() {
        let op_rule = op_pair.as_rule();
        
        let right_pair = pairs.next().ok_or_else(|| {
            AstError::BuildError(format!(
                "Incomplete infix tree: expected right term after operator {:?}.",
                op_rule
            ))
        })?;
        
        let right_term = build_term(right_pair, table)?;
        
        let comp_type = match op_rule {
            Rule::AND => ClauseCompositionType::And,
            Rule::OR => ClauseCompositionType::Or,
            Rule::XOR => ClauseCompositionType::Xor,
            _ => {
                return Err(AstError::BuildError(format!(
                    "Unknown clause operator: {:?}",
                    op_rule
                )))
            }
        };

        ops.push(op_map(comp_type));
        terms.push(right_term);
    }

    let mut current_clause = terms.pop().unwrap(); 

    while let Some(op_type) = ops.pop() {
        if let Some(mut prev_clause) = terms.pop() {
            let composition = ClauseComposition::new(op_type, current_clause);
            prev_clause.set_composition(composition);
            current_clause = prev_clause;
        }
    }

    Ok(current_clause)
}

// ==================== Clause Builders ====================

fn build_clause(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        &build_clause_term,
        &|op| op,
        table,
    )
}

fn build_clause_term(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let inner = pair
        .into_inner()
        .next()
        .ok_or_else(|| AstError::BuildError("Empty clause term.".to_string()))?;
    
    match inner.as_rule() {
        Rule::co => build_co(inner, table),
        Rule::cp => build_cp(inner, table),
        Rule::cf => build_cf(inner, table),
        Rule::cd => build_cd(inner, table),
        Rule::T => Ok(Clause::boolean_true()),
        Rule::F => Ok(Clause::boolean_false()),
        Rule::OPEN_EXP => {
            let mut pairs = inner.into_inner();
            pairs.next();
            let clause_pair = pairs
                .next()
                .ok_or_else(|| AstError::BuildError("Expected 'clause' inside '(...)'".to_string()))?;
            build_clause(clause_pair, table)
        }
        _ => Err(AstError::BuildError(format!(
            "Unexpected clause term: {:?}",
            inner.as_rule()
        ))),
    }
}

fn build_co(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        &build_co_atom,
        &|op| op,
        table,
    )
}

fn build_cp(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        &build_cp_atom,
        &|op| op,
        table,
    )
}

fn build_cf(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    build_infix_tree(
        pair.into_inner(),
        &build_cf_term,
        &|op| op,
        table,
    )
}

fn build_cf_term(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let inner = pair
        .into_inner()
        .next()
        .ok_or_else(|| AstError::BuildError("Empty CF term.".to_string()))?;
    
    match inner.as_rule() {
        Rule::cd => build_cd(inner, table),
        Rule::cf_atom => build_cf_atom(inner, table),
        _ => Err(AstError::BuildError(format!(
            "Unexpected CF term: {:?}",
            inner.as_rule()
        ))),
    }
}

// ==================== Deontic Clause Builders ====================

fn build_cd(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let mut pairs = pair.into_inner();

    let relation_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Expected relation in cd".to_string()))?;
    let (sender, receiver, rel_type) = build_rel(relation_pair, table)?;

    let open_dyn = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Expected '[' after relation in cd".to_string()))?;
    if open_dyn.as_rule() != Rule::OPEN_DYN {
        return Err(AstError::BuildError(format!(
            "Expected '[', found {:?}",
            open_dyn.as_rule()
        )));
    }

    let beta_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Expected action (beta) in cd".to_string()))?;
    
    let action = build_beta(beta_pair, table)?;

    let close_dyn = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Expected ']' after action in cd".to_string()))?;
    if close_dyn.as_rule() != Rule::CLOSE_DYN {
        return Err(AstError::BuildError(format!(
            "Expected ']', found {:?}",
            close_dyn.as_rule()
        )));
    }

    let inner_clause = if let Some(open_exp) = pairs.next() {
        if open_exp.as_rule() != Rule::OPEN_EXP {
            return Err(AstError::BuildError(format!(
                "Expected '(' for compensation, found {:?}",
                open_exp.as_rule()
            )));
        }

        let clause_pair = pairs
            .next()
            .ok_or_else(|| AstError::BuildError("Expected clause in compensation".to_string()))?;
        let clause = build_clause(clause_pair, table)?;

        let close_exp = pairs
            .next()
            .ok_or_else(|| AstError::BuildError("Expected ')' after compensation".to_string()))?;
        if close_exp.as_rule() != Rule::CLOSE_EXP {
            return Err(AstError::BuildError(format!(
                "Expected ')', found {:?}",
                close_exp.as_rule()
            )));
        }

        clause
    } else {
        Clause::boolean_true()
    };

    Ok(match rel_type {
        RelativizationType::Global => Clause::dynamic_global(action, inner_clause),
        RelativizationType::Relativized => Clause::dynamic_relativized(sender, action, inner_clause),
        RelativizationType::Directed => Clause::dynamic_directed(sender, receiver, action, inner_clause),
    })
}

fn build_co_atom(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let mut pairs = pair.into_inner();

    let relation_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'rel' in 'co_atom'".to_string()))?;
    let (sender, receiver, rel_type) = build_rel(relation_pair, table)?;

    pairs.next();
    pairs.next();

    let action_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'alpha' in 'co_atom'".to_string()))?;
    let action = build_alpha(action_pair, table)?;

    pairs.next();

    let penalty_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'penalty' in 'co_atom'".to_string()))?;
    let penalty = build_penalty(penalty_pair, table)?;

    Ok(match rel_type {
        RelativizationType::Global => {
            Clause::deontic_global(action, DeonticClauseType::Obligation, Some(penalty))
        }
        RelativizationType::Relativized => {
            Clause::deontic_relativized(sender, action, DeonticClauseType::Obligation, Some(penalty))
        }
        RelativizationType::Directed => {
            Clause::deontic_directed(sender, receiver, action, DeonticClauseType::Obligation, Some(penalty))
        }
    })
}

fn build_cp_atom(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let mut pairs = pair.into_inner();

    let relation_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'rel' in 'cp_atom'".to_string()))?;
    let (sender, receiver, rel_type) = build_rel(relation_pair, table)?;

    pairs.next();
    pairs.next();

    let action_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'alpha' in 'cp_atom'".to_string()))?;
    let action = build_alpha(action_pair, table)?;

    pairs.next();

    Ok(match rel_type {
        RelativizationType::Global => {
            Clause::deontic_global(action, DeonticClauseType::Permission, None)
        }
        RelativizationType::Relativized => {
            Clause::deontic_relativized(sender, action, DeonticClauseType::Permission, None)
        }
        RelativizationType::Directed => {
            Clause::deontic_directed(sender, receiver, action, DeonticClauseType::Permission, None)
        }
    })
}

fn build_cf_atom(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let mut pairs = pair.into_inner();

    let relation_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'rel' in 'cf_atom'".to_string()))?;
    let (sender, receiver, rel_type) = build_rel(relation_pair, table)?;

    pairs.next();
    pairs.next();

    let action_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'alpha' in 'cf_atom'".to_string()))?;
    let action = build_alpha(action_pair, table)?;

    pairs.next();

    let penalty_pair = pairs
        .next()
        .ok_or_else(|| AstError::BuildError("Missing 'penalty' in 'cf_atom'".to_string()))?;
    let penalty = build_penalty(penalty_pair, table)?;

    Ok(match rel_type {
        RelativizationType::Global => {
            Clause::deontic_global(action, DeonticClauseType::Prohibition, Some(penalty))
        }
        RelativizationType::Relativized => {
            Clause::deontic_relativized(sender, action, DeonticClauseType::Prohibition, Some(penalty))
        }
        RelativizationType::Directed => {
            Clause::deontic_directed(sender, receiver, action, DeonticClauseType::Prohibition, Some(penalty))
        }
    })
}

fn build_penalty(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Clause> {
    let mut inner = pair.into_inner();

    if let Some(first_child) = inner.next() {
        if first_child.as_rule() != Rule::OPEN_PTY {
            return Err(AstError::BuildError(format!(
                "Expected OPEN_PTY (_/), found {:?}",
                first_child.as_rule()
            )));
        }

        let clause_pair = inner
            .next()
            .ok_or_else(|| AstError::BuildError("Expected clause in penalty".to_string()))?;
        if clause_pair.as_rule() != Rule::clause {
            return Err(AstError::BuildError(format!(
                "Expected 'clause', found {:?}",
                clause_pair.as_rule()
            )));
        }
        let clause = build_clause(clause_pair, table)?;

        let close_pty = inner
            .next()
            .ok_or_else(|| AstError::BuildError("Expected CLOSE_PTY (/_)".to_string()))?;
        if close_pty.as_rule() != Rule::CLOSE_PTY {
            return Err(AstError::BuildError(format!(
                "Expected CLOSE_PTY (/_), found {:?}",
                close_pty.as_rule()
            )));
        }

        Ok(clause)
    } else {
        Ok(Clause::boolean_false())
    }
}

fn build_rel(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<(i32, i32, RelativizationType)> {
    let mut pairs = pair.into_inner();

    match pairs.next() {
        None => Ok((-1, -1, RelativizationType::Global)),
        Some(first_pair) => match first_pair.as_rule() {
            Rule::OPEN_REL => {
                let id1_pair = pairs
                    .next()
                    .ok_or_else(|| AstError::BuildError("Expected ID after '{' in 'rel'".to_string()))?;
                let id1_str = id1_pair.as_str().to_string();
                let sender = table.add_symbol(id1_str, SymbolType::Individual);

                if let Some(sep_or_close) = pairs.next() {
                    match sep_or_close.as_rule() {
                        Rule::SEP_REL => {
                            let id2_pair = pairs.next().ok_or_else(|| {
                                AstError::BuildError("Expected ID after ',' in 'rel'".to_string())
                            })?;
                            let id2_str = id2_pair.as_str().to_string();
                            let receiver = table.add_symbol(id2_str, SymbolType::Individual);

                            pairs.next().ok_or_else(|| {
                                AstError::BuildError("Missing '}' in pair relation".to_string())
                            })?;

                            Ok((sender, receiver, RelativizationType::Directed))
                        }
                        Rule::CLOSE_REL => {
                            Ok((sender, -1, RelativizationType::Relativized))
                        }
                        _ => Err(AstError::BuildError(
                            "Expected ',' or '}' in 'rel'".to_string(),
                        )),
                    }
                } else {
                    Err(AstError::BuildError("Missing '}' in 'rel'".to_string()))
                }
            }
            _ => Err(AstError::BuildError(format!(
                "Unexpected rule in 'rel': {:?}",
                first_pair.as_rule()
            ))),
        },
    }
}

// ==================== Action Builders ====================

fn build_action_infix_tree<F>(
    mut pairs: Pairs<Rule>,
    build_term: &F,
    table: &mut SymbolTable,
) -> Result<Action>
where
    F: Fn(Pair<Rule>, &mut SymbolTable) -> Result<Action>,
{
    let left_pair = pairs.next().ok_or_else(|| {
        AstError::BuildError("Incomplete action infix tree: expected left term.".to_string())
    })?;
    let mut left = build_term(left_pair, table)?;

    while let Some(op_pair) = pairs.next() {
        let op_rule = op_pair
            .into_inner()
            .next()
            .ok_or_else(|| AstError::BuildError("Empty 'op' rule".to_string()))?
            .as_rule();

        let right_pair = pairs.next().ok_or_else(|| {
            AstError::BuildError(format!(
                "Incomplete action infix tree: expected right term after operator {:?}.",
                op_rule
            ))
        })?;
        let right = build_term(right_pair, table)?;

        let operator = match op_rule {
            Rule::OP_CHOICE => ActionOperator::Choice,
            Rule::OP_SEQ => ActionOperator::Sequence,
            Rule::OP_CONC => ActionOperator::Concurrency,
            _ => {
                return Err(AstError::BuildError(format!(
                    "Unknown action operator: {:?}",
                    op_rule
                )))
            }
        };

        left = Action::Composed(ComposedAction::binary(left, right, operator));
    }
    Ok(left)
}

fn build_alpha(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Action> {
    build_action_infix_tree(pair.into_inner(), &build_alpha_atom, table)
}

fn build_alpha_atom(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Action> {
    let inner = pair
        .into_inner()
        .next()
        .ok_or_else(|| AstError::BuildError("Empty alpha atom.".to_string()))?;

    match inner.as_rule() {
        Rule::SKIP => Ok(Action::Basic(BasicAction::skip())),
        Rule::VIOLATION => Ok(Action::Basic(BasicAction::violation())),
        Rule::ID => {
            let id = table.add_symbol(inner.as_str().to_string(), SymbolType::Action);
            Ok(Action::Basic(BasicAction::with_value(id)))
        }
        Rule::OPEN_EXP => {
            let mut pairs = inner.into_inner();
            pairs.next();
            let alpha_pair = pairs
                .next()
                .ok_or_else(|| AstError::BuildError("Expected 'alpha' inside '(...)'".to_string()))?;
            build_alpha(alpha_pair, table)
        }
        _ => Err(AstError::BuildError(format!(
            "Unexpected alpha atom: {:?}",
            inner.as_rule()
        ))),
    }
}

fn build_beta(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Action> {
    build_action_infix_tree(pair.into_inner(), &build_beta_term, table)
}

fn build_beta_term(pair: Pair<Rule>, table: &mut SymbolTable) -> Result<Action> {
    let mut action: Option<Action> = None;
    let mut negation = false;
    let mut iteration = false;

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::UN_OP_NEG => {
                negation = true;
            }
            Rule::UN_OP_IT => {
                iteration = true;
            }
            Rule::ID => {
                let id = table.add_symbol(inner_pair.as_str().to_string(), SymbolType::Action);
                let basic_action = Action::Basic(BasicAction::with_value(id));
                
                if let Some(existing_action) = action {
                     action = Some(Action::Composed(ComposedAction::binary(
                        existing_action, 
                        basic_action, 
                        ActionOperator::Sequence
                    )));
                } else {
                    action = Some(basic_action);
                }
            }
            Rule::SKIP => {
                action = Some(Action::Basic(BasicAction::skip()));
            }
            Rule::VIOLATION => {
                action = Some(Action::Basic(BasicAction::violation()));
            }
            Rule::beta => {
                let inner_action = build_beta(inner_pair, table)?;
                
                if let Some(existing_action) = action {
                    action = Some(Action::Composed(ComposedAction::binary(
                        existing_action, 
                        inner_action, 
                        ActionOperator::Sequence
                    )));
                } else {
                    action = Some(inner_action);
                }
            }

            Rule::OPEN_EXP | Rule::CLOSE_EXP | Rule::OP_SEQ => {}
            
            _ => {
                // Opcional
            }
        }
    }

    let mut final_action = action.ok_or_else(|| 
        AstError::BuildError("Could not build action body for beta_term".to_string())
    )?;
    
    if iteration {
        final_action = Action::star(final_action);
    }

    if negation {
        final_action = Action::negation(final_action);
    }

    Ok(final_action)
}