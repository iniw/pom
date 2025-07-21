use pom_lexer::token::{Token, TokenKind};
use pom_utils::{arena::Arena, span::Span};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidIntLiteral(ParseIntError),
    InvalidFloatLiteral(ParseFloatError),

    UnbalancedBlock,

    UnexpectedToken { wanted: Vec<TokenKind>, got: Token },
}

pub type Errors = Arena<Error>;
