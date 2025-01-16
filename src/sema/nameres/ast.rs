#![expect(dead_code)]
use crate::{lex::span::Spanned, pool::Handle};

pub type SymbolId = Handle<DeclarationInfo>;

#[derive(Debug)]
pub enum Stmt {
    Declaration {
        identifier: SymbolId,
        info: DeclarationInfo,
    },
    Expr(Handle<Spanned<Expr>>),
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        lhs: Handle<Spanned<Expr>>,
        op: BinaryOp,
        rhs: Handle<Spanned<Expr>>,
    },
    Block {
        stmts: Vec<Handle<Spanned<Stmt>>>,
    },
    Call(Handle<Spanned<Expr>>),
    Literal(Literal),
    Symbol {
        identifier: SymbolId,
    },
}

#[derive(Debug)]
pub enum Literal {
    Number(u32),
    Boolean(bool),
}

#[derive(Debug)]
pub enum BinaryOp {
    Div,
    Mul,
    Add,
    Sub,
}

#[derive(Debug)]
pub enum DeclarationInfo {
    Kind(Kind),
    Value(Handle<Spanned<Expr>>),
    KindAndValue(Kind, Handle<Spanned<Expr>>),
}

#[derive(Debug)]
pub enum Kind {
    Bool,
    Int,

    Fn,

    Bottom,
}
