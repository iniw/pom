use pom_utils::{
    arena::{Arena, Id},
    span::Span,
};

use pom_parser::error::Error as ParserError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    UnknownSymbol,
    ParserError(Id<ParserError>),
}

pub type Errors = Arena<Error>;
