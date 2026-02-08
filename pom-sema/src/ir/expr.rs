use pom_utils::{arena::Id, span::Span};

use crate::{
    ast,
    ir::{Sym, stmt::Stmt},
    lowering::error::Error,
};

/// IR expression node.
#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// IR expression kind.
///
/// Mirrors the AST [`ExprKind`](crate::ast::ExprKind) but with `Ident` resolved
/// to an `Id<Sym>`. `FnType`, `Fn`, and `Tuple` are not yet lowered (`todo!()`).
#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Binary {
        lhs: Id<Expr>,
        op: ast::BinaryOp,
        rhs: Id<Expr>,
    },

    Block(Vec<Id<Stmt>>),

    Call {
        callable: Id<Expr>,
        args: Vec<Id<Expr>>,
    },

    /// Resolved identifier — points to a symbol in the `Ir.symbols` arena.
    Ident(Id<Sym>),

    Bool(bool),

    Float(f64),

    Int(i64),

    Invalid(Id<Error>),
}
