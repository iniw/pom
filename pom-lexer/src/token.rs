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
#[logos(skip r"[ \t\r\n\f]+", error = ErrorKind)]
pub enum TokenKind {
    #[token(r"->")]
    Arrow,

    #[token(r"true")]
    #[token(r"false")]
    Bool,

    #[token(r"@")]
    At,

    #[token(r":")]
    Colon,

    #[token(r",")]
    Comma,

    #[token(r"$")]
    Dollar,

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

    Invalid(ErrorKind),

    /// Synthetic end-of-input marker (not produced by logos).
    Eof,
}

/// Alias for a sequence of tokens produced by the lexer.
pub type Tokens = Vec<Token>;

/// Maps a `Result<Token, E>` or `Option<Token>` to its `.kind`.
pub trait TokenExt {
    type Output;

    fn kind(self) -> Self::Output;
}

impl<E> TokenExt for Result<Token, E> {
    type Output = Result<TokenKind, E>;

    #[inline]
    fn kind(self) -> Self::Output {
        self.map(|token| token.kind)
    }
}

impl TokenExt for Option<Token> {
    type Output = Option<TokenKind>;

    #[inline]
    fn kind(self) -> Self::Output {
        self.map(|token| token.kind)
    }
}
