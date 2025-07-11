use std::num::{ParseFloatError, ParseIntError};

use pom_lexer::token::TokenKind;
use pom_utils::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidIntLiteral(ParseIntError),
    InvalidFloatLiteral(ParseFloatError),

    UnexpectedToken(TokenKind),
}
