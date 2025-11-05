// 1. Declara os ficheiros .rs internos deste módulo
mod parser;
pub mod ast_builder;

// 2. RE-EXPORTA publicamente os tipos principais.
//    Isto permite que 'lib.rs' faça 'use crate::parser::Rule'
//    em vez do complicado 'use crate::parser::parser::Rule'
pub use parser::{RCLParser, Rule};

// 3. (Opcional) Re-exporte os seus tipos de AST para fácil acesso
pub use ast_builder::{
    build_ast,  // <-- Adicione esta linha
    Contract,   // <-- Adicione os seus structs de AST
    Clause,
    Action,
    DeonticClause,
    DeonticOperator,
    BasicAction,
    Relation,
    AstError    // <-- Adicione o seu tipo de erro do builder
}; // Adicione aqui os seus structs/enums de AST