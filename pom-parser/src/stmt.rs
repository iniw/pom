use pom_utils::{arena::Id, span::Span};

use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    Block(Vec<Id<Stmt>>),

    Expr(Id<Expr>),

    Decl {
        lhs: Id<Expr>,
        kind: Option<Id<Expr>>,
        rhs: Id<Expr>,
    },

    Invalid,
}
