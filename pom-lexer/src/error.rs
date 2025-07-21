use pom_utils::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    #[default]
    UnknownToken,
}

pub type Errors = Vec<Error>;
