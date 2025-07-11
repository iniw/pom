use pom_utils::{arena::Id, span::Span};

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

    Bool(bool),

    Number(Number),

    Ident,

    Invalid,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}
