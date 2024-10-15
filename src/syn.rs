use std::iter::Peekable;

use crate::{
    lex::{Span, Spanned, Token},
    pool::{Handle, Pool},
};

macro_rules! chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(ref ctx) = $tokens.next_if(|next| match next.data {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            ChaseResult::Caught(&ctx.data)
        } else {
            ChaseResult::Missing($tokens.peek().expect("ICE: Shouldn't reach end of token stream when chasing."))
        }
    }};
}

macro_rules! spanned_chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(ref ctx) = $tokens.next_if(|next| match next.data {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            SpannedChaseResult::SpannedCaught(&ctx.data, ctx.span)
        } else {
            SpannedChaseResult::SpannedMissing($tokens.peek().expect("ICE: Shouldn't reach end of token stream when chasing."))
        }
    }};
}

pub struct Parser<I: Iterator> {
    tokens: Peekable<I>,
    exprs: Pool<Expr>,
    stmts: Pool<Stmt>,
    errors: Vec<Error>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>>> Parser<I> {
    pub fn new(tokens: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            exprs: Pool::new(),
            stmts: Pool::new(),
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> (Pool<Stmt>, Pool<Expr>, Vec<Error>) {
        self.parse_program();
        (self.stmts, self.exprs, self.errors)
    }

    fn parse_program(&mut self) {
        while let Missing(_) = chase!(self.tokens, Token::EndOfFile) {
            if let Err(error) = self.parse_statement() {
                self.errors.push(error);
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Handle<Stmt>, Error> {
        let expr = self.parse_expression()?;
        Ok(self.stmts.add(Stmt::Expr(expr)))
    }

    fn parse_expression(&mut self) -> Result<Handle<Expr>, Error> {
        let mut expr = self.parse_precedence1()?;

        while let Caught(_) = chase!(self.tokens, Token::Add) {
            let right = self.parse_precedence1()?;
            expr = self.exprs.add(Expr::Binary {
                left: expr,
                op: BinaryOp::Add,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence1(&mut self) -> Result<Handle<Expr>, Error> {
        match spanned_chase!(self.tokens, Token::Number(_)) {
            SpannedCaught(Token::Number(number), span) => {
                let number = number.parse().map_err(|_| Error {
                    kind: ErrorKind::InvalidNumericLiteral,
                    span,
                })?;
                Ok(self.exprs.add(Expr::Literal(Literal::Number(number))))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Stmt {
    Expr(Handle<Expr>),
}

#[derive(Debug, Copy, Clone)]
pub enum Expr {
    Binary {
        left: Handle<Expr>,
        op: BinaryOp,
        right: Handle<Expr>,
    },
    Literal(Literal),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Number(f64),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
}

#[derive(Debug, Copy, Clone)]
#[expect(dead_code)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumericLiteral,
}

#[derive(Debug, Copy, Clone)]
#[expect(dead_code)]
enum ChaseResult<'syn, 'lex> {
    Caught(&'syn Token<'lex>),
    Missing(&'syn Spanned<Token<'lex>>),
}
use ChaseResult::*;

#[derive(Debug, Copy, Clone)]
#[expect(dead_code)]
enum SpannedChaseResult<'syn, 'lex> {
    SpannedCaught(&'syn Token<'lex>, Span),
    SpannedMissing(&'syn Spanned<Token<'lex>>),
}
use SpannedChaseResult::*;
