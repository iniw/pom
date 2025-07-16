use pom_utils::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    UnknownSymbol,
}

pub type Errors = Vec<Error>;

pub type ErrorOr<T> = Result<T, Error>;
