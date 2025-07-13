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
    #[token(r"true")]
    #[token(r"false")]
    Bool,

    #[token(r":")]
    Colon,

    #[token(r"=")]
    Equal,

    #[regex(r"[-+]?[0-9]+\.[0-9]+")]
    Float,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex(r"[-+]?[0-9]+")]
    Int,

    #[token(r"{")]
    LBrace,

    #[token(r"(")]
    LParen,

    #[token(r"-")]
    Minus,

    #[token(r"+")]
    Plus,

    #[token(r";")]
    Semicolon,

    #[token(r"/")]
    Slash,

    #[token(r"*")]
    Star,

    #[token(r"}")]
    RBrace,

    #[token(r")")]
    RParen,

    Invalid,

    Eof,
}
