use pom_utils::arena::{Arena, Id};

use crate::ir::{
    expr::Expr,
    stmt::{BindKind, Stmt},
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sym {
    pub kind: Option<BindKind>,
    pub init: Option<Id<Expr>>,
}
