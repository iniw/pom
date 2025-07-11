use crate::{error::Error, lexer::Lexer, token::Token};

pub mod error;
pub mod token;

mod lexer;
#[cfg(test)]
mod tests;

pub fn lex(src: &str) -> (Tokens, Errors) {
    Lexer::new(src).lex()
}

pub type Tokens = Vec<Token>;
pub type Errors = Vec<Error>;
