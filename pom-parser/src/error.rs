use pom_lexer::token::{Token, TokenKind};
use pom_utils::{arena::Arena, span::Span};
use std::num::{ParseFloatError, ParseIntError};

/// A parser error with its [`ErrorKind`] and source [`Span`].
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

/// The kind of parser error.
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// Literal matched the lexer regex but failed `str::parse` for integers.
    InvalidIntLiteral(ParseIntError),
    /// Literal matched the lexer regex but failed `str::parse` for floats.
    InvalidFloatLiteral(ParseFloatError),

    /// `{` without a matching `}`.
    UnbalancedBlock,

    /// Expected one of `wanted`, found `got`.
    UnexpectedToken { wanted: Vec<TokenKind>, got: Token },
}

/// Arena of parser errors (arena, not `Vec`, because [`ExprKind::Invalid`](crate::ast::expr::ExprKind::Invalid) holds `Id<Error>`).
pub type Errors = Arena<Error>;
