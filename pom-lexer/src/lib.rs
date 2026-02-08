use crate::{error::Errors, lexer::Lexer, token::Tokens};

pub mod error;
pub mod token;

mod lexer;
#[cfg(test)]
mod tests;

/// Tokenizes `src` into a list of tokens and any lexer errors.
///
/// Unrecognized characters become [`TokenKind::Invalid`](token::TokenKind::Invalid)
/// tokens rather than halting, so the output always covers the full input.
pub fn lex(src: &str) -> (Tokens, Errors) {
    Lexer::new(src).lex()
}
