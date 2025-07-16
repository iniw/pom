use pom_utils::arena::{Arena, Id};

use crate::ast::{expr::Expr, stmt::Stmt};

pub mod expr;
pub mod stmt;

#[derive(Debug, Default, PartialEq)]
pub struct Ast {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
}
