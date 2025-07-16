use pom_lexer::token::Tokens;

use crate::{ast::Ast, error::Errors, parser::Parser};

pub mod ast;
pub mod error;

mod parser;
#[cfg(test)]
mod tests;

pub fn parse(src: &str, tokens: Tokens) -> (Ast, Errors) {
    Parser::new(src, tokens).parse()
}
