use crate::{lex::span::Spanned, pool::Handle};

#[derive(Debug)]
pub enum Stmt<'lex> {
    Declaration {
        identifier: &'lex str,
        info: DeclarationInfo<'lex>,
    },
    Expr(Handle<Spanned<Expr<'lex>>>),
    Print(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug)]
pub enum Expr<'lex> {
    Binary {
        lhs: Handle<Spanned<Expr<'lex>>>,
        op: BinaryOp,
        rhs: Handle<Spanned<Expr<'lex>>>,
    },
    Block {
        stmts: Vec<Handle<Spanned<Stmt<'lex>>>>,
    },
    Call(Handle<Spanned<Expr<'lex>>>),
    Literal(Literal),
    Symbol {
        identifier: &'lex str,
    },
}

#[derive(Debug)]
pub enum Literal {
    Number(u32),
    Record,
}

#[derive(Debug)]
pub enum BinaryOp {
    Div,
    Mul,
    Add,
    Sub,
}

#[derive(Debug)]
pub enum DeclarationInfo<'lex> {
    Kind(Kind<'lex>),
    Value(Handle<Spanned<Expr<'lex>>>),
    KindAndValue(Kind<'lex>, Handle<Expr<'lex>>),
}

#[derive(Debug)]
enum Kind<'lex> {
    Data(&'lex str),
    Fn,
    Unresolved,
}
