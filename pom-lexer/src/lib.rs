use crate::{error::Errors, lexer::Lexer, token::Tokens};

pub mod error;
pub mod token;

mod lexer;
#[cfg(test)]
mod tests;

pub fn lex(src: &str) -> (Tokens, Errors) {
    Lexer::new(src).lex()
}
