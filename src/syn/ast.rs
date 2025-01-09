use crate::{lex::span::Spanned, pool::Handle};

#[derive(Debug)]
pub enum Stmt<'lex> {
    Expr(Handle<Spanned<Expr<'lex>>>),
    SymbolDecl(SymbolDecl<'lex>),
    Print(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug)]
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
pub struct SymbolDecl<'lex> {
    pub identifier: &'lex str,
    pub info: SymbolInfo<'lex>,
}

#[derive(Debug)]
pub enum SymbolInfo<'lex> {
    Var(VarInfo<'lex>),
    Fn(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug)]
pub enum VarInfo<'lex> {
    Type(&'lex str),
    Value(Handle<Spanned<Expr<'lex>>>),
    TypeAndValue(&'lex str, Handle<Expr<'lex>>),
}
