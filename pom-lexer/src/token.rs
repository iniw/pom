use logos::Logos;
use pom_utils::span::Span;

use crate::error::ErrorKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn eof() -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span::eof(),
        }
    }
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(error = ErrorKind)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum TokenKind {
    #[token(r"false", |_| false)]
    #[token(r"true", |_| true)]
    Bool(bool),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex(r"[-+]?[0-9]+")]
    Int,

    #[regex(r"[-+]?[0-9]+\.[0-9]+")]
    Float,

    #[token(r"+")]
    Plus,

    #[token(r"-")]
    Minus,

    #[token(r"*")]
    Star,

    #[token(r"/")]
    Slash,

    Invalid,

    Eof,
}
