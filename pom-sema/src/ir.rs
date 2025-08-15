use pom_utils::arena::{Arena, Id};

use crate::ir::{
    expr::Expr,
    stmt::{Bind, Stmt},
};

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq, Eq)]
pub struct Builtins {
    pub bool: Id<Type>,
    pub i32: Id<Type>,
    pub f32: Id<Type>,
}

#[derive(Debug, PartialEq)]
pub struct Ir {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,

    pub symbols: Arena<Sym>,

    pub types: Arena<Type>,
    pub builtins: Builtins,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Sym {
    pub kind: SymKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymKind {
    Expr(Id<Expr>),

    Fn { params: Vec<Bind> },
    Type(Option<Id<Type>>),

    Infer,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Type {}
