use pom_utils::{arena::Id, span::Span};

use crate::ast::expr::Expr;

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

#[derive(Debug, PartialEq, Eq)]
pub struct Bind {
    pub lhs: Id<Expr>,
    pub kind: BindKind,
    pub rhs: Option<Id<Expr>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BindKind {
    Expr(Id<Expr>),
    Fn { params: Vec<Bind> },
    Type,

    Infer,
}
