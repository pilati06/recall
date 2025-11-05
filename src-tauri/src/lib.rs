use std::fs;
use std::collections::HashSet;
use pest::Parser;

pub mod parser;

use parser::{
    RCLParser,
    Rule,
    build_ast,
    Contract,
    Clause,
    DeonticOperator,
    Action,
    BasicAction,
    Relation,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DeonticTag {
    relation: Relation,
    operator: DeonticOperator,
    action_id: String,
}

fn extract_action_id(action: &Action) -> Option<String> {
    match action {
        Action::Basic(BasicAction::Id(id)) => Some(id.clone()),
        Action::Basic(_) => None,
        Action::Composed(composed) => extract_action_id(&composed.left),
    }
}

// Extrai deltas com informação de contexto dinâmico
fn extract_delta_from_clause(clause: &Clause, in_dynamic_context: bool) -> Vec<(HashSet<DeonticTag>, bool)> {
    let mut deltas = Vec::new();
    extract_delta_recursive(clause, &mut HashSet::new(), &mut deltas, in_dynamic_context);
    deltas
}

fn extract_delta_recursive(
    clause: &Clause,
    current_set: &mut HashSet<DeonticTag>,
    deltas: &mut Vec<(HashSet<DeonticTag>, bool)>,
    in_dynamic_context: bool
) {
    match clause {
        Clause::Deontic(d) => {
            if let Some(action_id) = extract_action_id(&d.action) {
                let mut new_set = current_set.clone();
                new_set.insert(DeonticTag {
                    relation: d.relation.clone(),
                    operator: d.operator.clone(),
                    action_id,
                });
                
                if matches!(&*d.penalty, Clause::False) {
                    // Penalidade vazia = nó terminal
                    deltas.push((new_set, in_dynamic_context));
                } else {
                    extract_delta_recursive(&d.penalty, &mut new_set, deltas, in_dynamic_context);
                }
            }
        }
        Clause::Dynamic(dyn_clause) => {
            if let Some(comp) = &dyn_clause.compensation {
                // Entrando em contexto dinâmico
                extract_delta_recursive(comp, current_set, deltas, true);
            } else {
                // Sem compensação = terminal
                if !current_set.is_empty() {
                    deltas.push((current_set.clone(), in_dynamic_context));
                }
            }
        }
        Clause::And(l, r) => {
            // Para AND, precisa processar ambos no mesmo conjunto
            let mut temp_set = current_set.clone();
            extract_delta_recursive(l, &mut temp_set, deltas, in_dynamic_context);
            extract_delta_recursive(r, current_set, deltas, in_dynamic_context);
        }
        Clause::Or(l, r) | Clause::Xor(l, r) => {
            // OR/XOR cria caminhos separados
            extract_delta_recursive(l, &mut current_set.clone(), deltas, in_dynamic_context);
            extract_delta_recursive(r, &mut current_set.clone(), deltas, in_dynamic_context);
        }
        Clause::True => {
            if !current_set.is_empty() {
                deltas.push((current_set.clone(), in_dynamic_context));
            }
        }
        Clause::False => {
            // False não gera delta
        }
    }
}

fn generate_conflict_set(tag: &DeonticTag, contract: &Contract, in_dynamic_context: bool) -> HashSet<DeonticTag> {
    let mut conflict_set = HashSet::new();
    
    // Conflitos globais - sempre verificados
    for conflict in &contract.global_conflicts {
        if let (Some(id1), Some(id2)) = (
            extract_action_id(&conflict.act1),
            extract_action_id(&conflict.act2)
        ) {
            if tag.action_id == id1 {
                for op in [DeonticOperator::Obligation, DeonticOperator::Prohibition, DeonticOperator::Permission] {
                    conflict_set.insert(DeonticTag {
                        relation: tag.relation.clone(),
                        operator: op,
                        action_id: id2.clone(),
                    });
                }
            }
            if tag.action_id == id2 {
                for op in [DeonticOperator::Obligation, DeonticOperator::Prohibition, DeonticOperator::Permission] {
                    conflict_set.insert(DeonticTag {
                        relation: tag.relation.clone(),
                        operator: op,
                        action_id: id1.clone(),
                    });
                }
            }
        }
    }
    
    // Conflitos relativizados - só verificados dentro de contexto dinâmico
    if in_dynamic_context {
        for conflict in &contract.relativized_conflicts {
            if let (Some(id1), Some(id2)) = (
                extract_action_id(&conflict.act1),
                extract_action_id(&conflict.act2)
            ) {
                if tag.action_id == id1 {
                    for op in [DeonticOperator::Obligation, DeonticOperator::Prohibition, DeonticOperator::Permission] {
                        conflict_set.insert(DeonticTag {
                            relation: tag.relation.clone(),
                            operator: op,
                            action_id: id2.clone(),
                        });
                    }
                }
                if tag.action_id == id2 {
                    for op in [DeonticOperator::Obligation, DeonticOperator::Prohibition, DeonticOperator::Permission] {
                        conflict_set.insert(DeonticTag {
                            relation: tag.relation.clone(),
                            operator: op,
                            action_id: id1.clone(),
                        });
                    }
                }
            }
        }
    }
    
    conflict_set
}

fn check_conflicts(contract: &Contract) -> Vec<String> {
    let mut conflicts = Vec::new();
    
    for (idx, clause) in contract.clauses.iter().enumerate() {
        let deltas = extract_delta_from_clause(clause, false);
        
        for (i, (d1, in_ctx1)) in deltas.iter().enumerate() {
            for (j, (d2, in_ctx2)) in deltas.iter().enumerate() {
                if i >= j {
                    continue;
                }
                
                // Usa o contexto mais restritivo (se qualquer um está em contexto dinâmico)
                let in_dynamic = *in_ctx1 || *in_ctx2;
                
                for tag1 in d1 {
                    let conflict_set = generate_conflict_set(tag1, contract, in_dynamic);
                    
                    for tag2 in d2 {
                        if conflict_set.contains(tag2) {
                            let conflict_type = if in_dynamic && 
                                contract.relativized_conflicts.iter().any(|c| {
                                    let id1 = extract_action_id(&c.act1);
                                    let id2 = extract_action_id(&c.act2);
                                    (id1.as_ref() == Some(&tag1.action_id) && id2.as_ref() == Some(&tag2.action_id)) ||
                                    (id2.as_ref() == Some(&tag1.action_id) && id1.as_ref() == Some(&tag2.action_id))
                                }) {
                                "RELATIVIZADO"
                            } else {
                                "GLOBAL"
                            };
                            
                            conflicts.push(format!(
                                "CONFLITO {} na cláusula {}: {:?}({}) ∈ δ{} conflita com {:?}({}) ∈ δ{}",
                                conflict_type, idx + 1, tag1.operator, tag1.action_id, i + 1,
                                tag2.operator, tag2.action_id, j + 1
                            ));
                        }
                    }
                }
            }
        }
    }
    
    conflicts
}

#[tauri::command]
fn process_file(path: String) -> Result<String, String> {
    let input_string = fs::read_to_string(&path)
        .map_err(|e| format!("Erro ao ler o ficheiro: {}", e))?;

    let mut pairs = RCLParser::parse(Rule::main, &input_string)
        .map_err(|e| format!("Erro de sintaxe: \n{}", e))?;

    let main_pair = pairs.next().unwrap();
    let ast: Contract = build_ast(main_pair)
        .map_err(|e| format!("Erro ao construir AST: \n{}", e))?;

    let conflicts = check_conflicts(&ast);
    
    let mut output = format!("=== AST ===\n{:#?}\n\n", ast);
    
    if conflicts.is_empty() {
        output.push_str("=== VERIFICAÇÃO DE CONFLITOS ===\nNenhum conflito encontrado.\n");
    } else {
        output.push_str(&format!("=== CONFLITOS ENCONTRADOS ({}) ===\n", conflicts.len()));
        for conflict in conflicts {
            output.push_str(&format!("{}\n", conflict));
        }
    }
    
    Ok(output)
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_opener::init())
        .invoke_handler(tauri::generate_handler![process_file])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}