use pom_utils::{arena::Id, span::Span};

use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    Bind {
        lhs: Id<Expr>,
        kind: Option<Id<Expr>>,
        rhs: Option<Id<Expr>>,
    },

    Block(Vec<Id<Stmt>>),

    Expr(Id<Expr>),

    Invalid,
}
