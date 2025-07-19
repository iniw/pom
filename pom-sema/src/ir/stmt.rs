use pom_utils::{arena::Id, span::Span};

use crate::ir::{Sym, expr::Expr};

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Bind(Bind),

    Block(Vec<Id<Stmt>>),

    Expr(Id<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bind {
    pub lhs: Id<Expr>,
    pub sym: Id<Sym>,
    pub rhs: Option<Id<Expr>>,
}
