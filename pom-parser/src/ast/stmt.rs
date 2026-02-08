use pom_utils::{arena::Id, span::Span};

use crate::ast::expr::Expr;

/// A statement node: [`StmtKind`] paired with its source [`Span`].
#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// The kind of a statement node.
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    /// Variable binding.
    ///
    /// Examples:
    /// - `name: type = value;`
    /// - `name := value;`
    /// - `name: type;`
    Bind(Bind),

    /// Bare expression followed by `;`.
    ///
    /// Examples:
    /// - `1 + 2;`
    Expr(Id<Expr>),
}

/// A binding. This is the most versatile node in the AST — it appears in
/// statements, function parameters, and tuple elements, using different
/// subsets of its fields depending on context:
///
/// - Statement binding (all fields set, or `ty`/`rhs` omitted):
///   - `name: type = value;`
///   - `name := value;` (`ty` is `None`)
///   - `name: type;` (`rhs` is `None`)
/// - Function parameter:
///   - `x: i32` (`lhs` + `ty`)
///   - `x` (bare `lhs` only)
/// - Tuple value element — each element is a `Bind` with only `lhs` set:
///   - `(1, 2)`
/// - Tuple type element — same shape as a value element; the parser cannot
///   distinguish these, so disambiguation is deferred to sema:
///   - `(i32, f32)`
/// - Named tuple field — `lhs` + `ty`, used for both named value tuples
///   and struct-like type signatures:
///   - `(x: i32, y: f32)`
///
/// `lhs` is an expression (rather than a plain identifier) for generality.
#[derive(Debug, PartialEq, Eq)]
pub struct Bind {
    pub lhs: Id<Expr>,
    pub ty: Option<TypeAnnotation>,
    pub rhs: Option<Id<Expr>>,
}

/// The type annotation in a binding.
#[derive(Debug, PartialEq, Eq)]
pub enum TypeAnnotation {
    /// The `type` keyword — annotates that the rhs is a kind.
    ///
    /// Examples:
    /// - `foo: type = i32;`
    /// - `foo: type = (i32) -> i32;`
    Type,

    /// A type expression.
    ///
    /// Examples:
    /// - `x: i32 = ...`
    Expr(Id<Expr>),
}
