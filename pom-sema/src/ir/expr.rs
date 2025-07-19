use pom_utils::{arena::Id, span::Span};

use crate::{
    ast,
    ir::{Sym, stmt::Stmt},
    lowering::error::Error,
};

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

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

    Ident(Id<Sym>),

    Literal(ast::Literal),

    Invalid(Id<Error>),
}
