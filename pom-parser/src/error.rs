use pom_lexer::token::{Token, TokenKind};
use pom_utils::{arena::Arena, span::Span};
use smallvec::SmallVec;
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
        wanted: SmallVec<[TokenKind; 8]>,
        got: Token,
    },
}

pub type Errors = Arena<Error>;
