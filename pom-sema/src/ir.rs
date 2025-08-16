use pom_utils::arena::{Arena, Id};

use crate::ir::{
    expr::Expr,
    stmt::{Bind, Stmt},
};

pub mod expr;
pub mod stmt;

#[derive(Debug, PartialEq)]
pub struct Ir {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,

    pub symbols: Arena<Sym>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Sym {
    Resolved(Type),

    Compute(Id<Expr>),

    NewType,

    Infer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Kind(Kind),
    Data(Data),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Fn(FnSignature),
    Type(TypeCtor),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Data {
    Fn(FnSignature),
    Type(TypeCtor),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub params: Vec<Bind>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeCtor {
    // Nullary
    Bool,
    I32,
    F32,

    // N-ary
    Struct(Vec<TypeCtor>),
}
