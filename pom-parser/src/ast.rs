use pom_utils::arena::{Arena, Id};

use crate::ast::{expr::Expr, stmt::Stmt};

pub mod expr;
pub mod stmt;

/// Root AST container.
///
/// `items` holds the top-level statements (by id). All statement and expression
/// nodes are owned by the `stmts` and `exprs` arenas respectively.
#[derive(Debug, Default, PartialEq)]
pub struct Ast {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,
}
