use pom_utils::{arena::Arena, span::Span};

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

pub type Errors = Arena<Error>;
