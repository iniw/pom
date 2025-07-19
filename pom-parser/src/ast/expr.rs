use pom_utils::{arena::Id, span::Span};

use crate::{ast::stmt::Stmt, error::Error};

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Binary {
        lhs: Id<Expr>,
        op: BinaryOp,
        rhs: Id<Expr>,
    },

    Block(Vec<Id<Stmt>>),

    Call {
        callable: Id<Expr>,
        args: Vec<Id<Expr>>,
    },

    Ident,

    Literal(Literal),

    Paren(Id<Expr>),

    Tuple(Vec<Id<Expr>>),

    Invalid(Id<Error>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
}
