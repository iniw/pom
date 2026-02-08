use pom_lexer::token::Tokens;

use crate::{ast::Ast, error::Errors, parser::Parser};

pub mod ast;
pub mod error;

mod parser;
#[cfg(test)]
mod tests;

/// Parses tokens into an AST and any parser errors.
///
/// Malformed constructs become [`ExprKind::Invalid`](ast::expr::ExprKind::Invalid)
/// nodes so parsing can continue past errors.
pub fn parse(src: &str, tokens: Tokens) -> (Ast, Errors) {
    Parser::new(src, tokens).parse()
}
