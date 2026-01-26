use pom_utils::{arena::Id, span::Span};

use crate::{
    ast::stmt::{Bind, Stmt},
    error::Error,
};

/// An expression node: [`ExprKind`] paired with its source [`Span`].
#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// The kind of an expression node.
#[derive(Debug, PartialEq)]
pub enum ExprKind {
    /// Function type, right-associative.
    ///
    /// Examples:
    /// - `(i32) -> i32`
    FnType { lhs: Id<Expr>, rhs: Id<Expr> },

    /// Binary arithmetic operation.
    ///
    /// Examples:
    /// - `1 + 2`
    /// - `a * b`
    Binary {
        lhs: Id<Expr>,
        op: BinaryOp,
        rhs: Id<Expr>,
    },

    /// Brace-delimited block of statements.
    ///
    /// The last expression in a block does not require a trailing semicolon,
    /// which is what allows blocks to double as precedence grouping
    /// (`{1 + 2} * 3`). An empty block `{}` represents the unit value; this
    /// is disambiguated in sema.
    ///
    /// Examples:
    /// - `{ a := 5; a + 1 }`
    /// - `{1 + 2} * 3`
    /// - `{}`
    Block(Vec<Id<Stmt>>),

    /// Boolean literal.
    ///
    /// Examples:
    /// - `true`
    /// - `false`
    Bool(bool),

    /// Function call, supports chaining.
    ///
    /// Examples:
    /// - `f(1, 2)`
    /// - `f(1)(2)`
    Call {
        callable: Id<Expr>,
        args: Vec<Id<Expr>>,
    },

    /// Floating-point literal.
    ///
    /// Examples:
    /// - `3.14`
    /// - `-0.5`
    Float(f64),

    /// Function expression. The body extends as far right as possible.
    ///
    /// Examples:
    /// - `@(x: i32) -> i32 x`
    /// - `$(x) x`
    Fn {
        kind: FnKind,
        params: Vec<Bind>,
        ret: Option<Id<Expr>>,
        body: Id<Expr>,
    },

    /// Named reference, resolved later in sema.
    ///
    /// Carries no data; the name is recovered via `span.text(src)`.
    ///
    /// Examples:
    /// - `foo`
    /// - `i32`
    Ident,

    /// Integer literal.
    ///
    /// Examples:
    /// - `42`
    /// - `-1`
    Int(i64),

    /// Parenthesized structure of possibly-named fields.
    ///
    /// Uses [`Bind`] because parenthesized expressions are
    /// syntactically ambiguous between types and values — a `Bind` with only
    /// `lhs` set covers both cases.
    ///
    /// Examples:
    /// - `(a, b)`
    /// - `(x: i32, y: f32)`
    /// - `(expr)`
    Tuple(Vec<Bind>),

    /// Placeholder for a malformed expression during error recovery.
    Invalid(Id<Error>),
}

/// Distinguishes captureless (`@`) from capturing (`$`) function expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FnKind {
    /// Captures its runtime environment (`$`).
    Closure,
    /// Can only capture compile-time data (`@`).
    Static,
}

/// Binary arithmetic operator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
}
