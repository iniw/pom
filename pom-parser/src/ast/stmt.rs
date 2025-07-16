use pom_utils::{arena::Id, span::Span};

use crate::ast::expr::Expr;

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Bind {
        lhs: Id<Expr>,
        kind: Option<BindKind>,
        rhs: Option<Id<Expr>>,
    },

    Block(Vec<Id<Stmt>>),

    Expr(Id<Expr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindKind {
    Expr(Id<Expr>),
    Fn,
    Type,
}
