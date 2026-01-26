use pom_utils::{
    arena::{Arena, Id},
    span::Span,
};

use pom_parser::error::Error as ParserError;

/// A lowering error with its [`ErrorKind`] and source [`Span`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

/// The kind of lowering error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    /// Identifier not found in any enclosing scope.
    UnknownSymbol,
    /// Propagated from a parser [`Invalid`](pom_parser::ast::expr::ExprKind::Invalid) node.
    ParserError(Id<ParserError>),
}

/// Arena of lowering errors (arena so that `ExprKind::Invalid` can hold `Id<Error>`).
pub type Errors = Arena<Error>;
