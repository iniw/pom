use pom_utils::arena::{Arena, Id};

use crate::ir::{
    expr::Expr,
    stmt::{Bind, Stmt},
};

pub mod expr;
pub mod stmt;

/// Intermediate representation produced by lowering.
///
/// Like [`Ast`](pom_parser::ast::Ast) but adds a `symbols` arena for resolved names.
#[derive(Debug, PartialEq)]
pub struct Ir {
    pub items: Vec<Id<Stmt>>,
    pub stmts: Arena<Stmt>,
    pub exprs: Arena<Expr>,

    pub symbols: Arena<Sym>,
}

/// Symbol state after lowering.
#[derive(Debug, PartialEq, Eq)]
pub enum Sym {
    /// Fully resolved type.
    Resolved(Type),

    /// Type is given by an expression (to be evaluated during typechecking).
    Compute(Id<Expr>),

    /// Introduces a new type (`T: type = ...`).
    NewType,

    /// Type to be inferred during typechecking.
    Infer,
}

/// Type classification: [`Kind`] (type-level) or [`Data`] (value-level).
///
/// For example, `i32` as a type is `Kind`, while the value `42` has type `Data`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Kind(Kind),
    Data(Data),
}

/// Type-level type (e.g. `i32` as a type expression, or a function signature at the type level).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Fn(FnSignature),
    Type(TypeCtor),
}

/// Value-level type (e.g. the type of the value `42` is `Data::Type(TypeCtor::I32)`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Data {
    Fn(FnSignature),
    Type(TypeCtor),
}

/// Function type signature (params only for now).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSignature {
    pub params: Vec<Bind>,
}

/// Concrete type constructors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeCtor {
    // Nullary
    Bool,
    I32,
    F32,

    // N-ary
    Struct(Vec<TypeCtor>),
}
