use pom_utils::span::Span;

/// A lexer error with its [`ErrorKind`] and source [`Span`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

/// The kind of lexer error.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    /// Character not recognized by any lexer rule.
    #[default]
    UnknownToken,
}

/// Alias for a collection of lexer errors.
pub type Errors = Vec<Error>;
