use pom_lexer::Tokens;
use pom_utils::arena::{Arena, Id};

use crate::{error::Error, expr::Expr, parser::Parser, stmt::Stmt};

pub mod error;
pub mod expr;
pub mod stmt;

mod parser;
#[cfg(test)]
mod tests;

pub fn parse(src: &str, tokens: Tokens) -> (Ast, Errors) {
    Parser::new(src, tokens).parse()
}

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
}

pub type Errors = Vec<Error>;
