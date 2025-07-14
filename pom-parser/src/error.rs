use pom_lexer::token::{Token, TokenKind};
use pom_utils::span::Span;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidIntLiteral(ParseIntError),
    InvalidFloatLiteral(ParseFloatError),

    UnbalancedBlock,

    UnexpectedToken {
        wanted: &'static [TokenKind],
        got: Token,
    },
}
