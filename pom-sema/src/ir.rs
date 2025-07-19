use pom_utils::arena::{Arena, Id};

use crate::ir::{
    expr::Expr,
    stmt::{Bind, Stmt},
};

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq, Default)]
pub struct Ir {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,

    pub symbols: Arena<Sym>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Sym {
    pub kind: SymKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymKind {
    Expr(Id<Expr>),
    Fn { params: Vec<Bind> },
    Infer,
    Type,
}
