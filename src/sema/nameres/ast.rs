#![expect(dead_code)]
use crate::{lex::span::Spanned, pool::Handle};

pub type SymbolId = Handle<Symbol>;

#[derive(Debug)]
pub enum Stmt {
    Expr(Handle<Spanned<Expr>>),
    SymbolDecl(SymbolId),
    Print(Handle<Spanned<Expr>>),
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        lhs: Handle<Spanned<Expr>>,
        op: BinaryOp,
        rhs: Handle<Spanned<Expr>>,
    },
    Block(Vec<Handle<Spanned<Stmt>>>),
    Call(Handle<Spanned<Expr>>),
    Literal(Literal),
    Symbol(SymbolId),
}

#[derive(Debug)]
pub enum Literal {
    Number(u32),
}

#[derive(Debug)]
pub enum BinaryOp {
    Div,
    Mul,
    Add,
    Sub,
}

#[derive(Debug)]
pub struct SymbolDecl {
    pub identifier: SymbolId,
    pub info: SymbolInfo,
}

#[derive(Debug)]
pub enum SymbolInfo {
    Var(VarInfo),
    Fn(Handle<Spanned<Expr>>),
}

#[derive(Debug)]
pub enum VarInfo {
    Type(SymbolId),
    Value(Handle<Spanned<Expr>>),
    TypeAndValue(SymbolId, Handle<Expr>),
}
