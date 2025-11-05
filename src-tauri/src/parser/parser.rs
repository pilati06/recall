use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/parser/RelativizedCL.pest"]
pub struct RCLParser;