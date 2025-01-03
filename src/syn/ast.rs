use crate::{lex::span::Spanned, pool::Handle};

#[derive(Debug, Clone)]
pub enum Stmt<'lex> {
    Expr(Handle<Spanned<Expr<'lex>>>),
    SymbolDecl(SymbolDecl<'lex>),
    Print(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolDecl<'lex> {
    pub identifier: &'lex str,
    pub info: SymbolInfo<'lex>,
}

#[derive(Debug, Clone)]
pub enum Expr<'lex> {
    Binary {
        left: Handle<Spanned<Self>>,
        op: BinaryOp,
        right: Handle<Spanned<Self>>,
    },
    Block(Vec<Handle<Spanned<Stmt<'lex>>>>),
    Call(Handle<Spanned<Self>>),
    Literal(Literal),
    Symbol(&'lex str),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Number(u32),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Div,
    Mul,
    Add,
    Sub,
}

#[derive(Debug, Copy, Clone)]
pub enum VarInfo<'lex> {
    Type(&'lex str),
    Value(Handle<Spanned<Expr<'lex>>>),
    TypeAndValue(&'lex str, Handle<Expr<'lex>>),
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolInfo<'lex> {
    Var(VarInfo<'lex>),
    Fn(Handle<Spanned<Stmt<'lex>>>),
}
