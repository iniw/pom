use crate::lex;

pub struct Parser<'lex> {
    tokens: &'lex [lex::ContextualizedToken<'lex>],
}

impl<'lex> Parser<'lex> {
    pub fn new(tokens: &'lex [lex::ContextualizedToken<'lex>]) -> Self {
        Self { tokens }
    }

    pub fn parse() {}
}

pub enum Kind {
    Fn,
}

pub enum Stmt {
    EntityDecl(Kind, Option<Expr>),
}

pub enum Expr {}
