use pom_utils::{arena::Id, span::Span};

use crate::ir::{Sym, expr::Expr};

/// IR statement node.
#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// IR statement kind.
///
/// Unlike the AST, `Block` exists here as a statement variant (the AST only has
/// it as an expression).
#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Bind(Bind),

    /// Block of statements — present in IR but not in AST's `StmtKind`.
    Block(Vec<Id<Stmt>>),

    Expr(Id<Expr>),
}

/// IR binding with a resolved symbol.
///
/// - `sym`: resolved symbol in the `Ir.symbols` arena (type info lives here, not in `ty`).
/// - `lhs`: the name expression.
/// - `rhs`: optional initializer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bind {
    pub sym: Id<Sym>,
    pub lhs: Id<Expr>,
    pub rhs: Option<Id<Expr>>,
}
