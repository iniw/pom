use logos::Logos;
use pom_utils::span::Span;
use static_assertions::const_assert_eq;

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

    #[token(r",")]
    Comma,

    #[token(r"=")]
    Equal,

    #[regex(r"[-+]?[0-9]+\.[0-9]+")]
    Float,

    #[token(r"fn")]
    Fn,

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

    #[token(r"|")]
    Pipe,

    #[token(r"+")]
    Plus,

    #[token(r"}")]
    RBrace,

    #[token(r")")]
    RParen,

    #[token(r";")]
    Semicolon,

    #[token(r"/")]
    Slash,

    #[token(r"*")]
    Star,

    #[token(r"type")]
    Type,

    Invalid,

    Eof,
}

pub type Tokens = Vec<Token>;

// This should be as tiny as possible.
// Increasing the size of this enum increases the size of several things throughout the parser.
const_assert_eq!(std::mem::size_of::<TokenKind>(), 1);

pub trait TokenExt {
    type Output;

    fn kind(self) -> Self::Output;
}

impl<E> TokenExt for Result<Token, E> {
    type Output = Result<TokenKind, E>;

    fn kind(self) -> Self::Output {
        self.map(|token| token.kind)
    }
}

impl TokenExt for Option<Token> {
    type Output = Option<TokenKind>;

    fn kind(self) -> Self::Output {
        self.map(|token| token.kind)
    }
}
