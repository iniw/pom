use std::iter::Peekable;

use crate::lex::{Spanned, Token};

macro_rules! eat {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(ctx) = $tokens.next_if(|next| match next.data {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            Found(ctx.data)
        } else {
            NotFound($tokens.peek().copied())
        }
    }};
}

#[derive(Debug, Copy, Clone)]
enum EatResult<'lex> {
    Found(Token<'lex>),
    NotFound(Option<Spanned<Token<'lex>>>),
}
use EatResult::*;

#[derive(Debug)]
pub struct Parser<'lex, I: Iterator<Item = Spanned<Token<'lex>>>> {
    tokens: Peekable<I>,
    exprs: Pool<Expr>,
    stmts: Pool<Stmt>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>>> Parser<'lex, I> {
    pub fn new(tokens: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            exprs: Pool::new(),
            stmts: Pool::new(),
        }
    }

    pub fn parse(mut self) -> (Vec<Stmt>, Vec<SyntaxError>) {
        self.parse_program()
    }

    fn parse_program(&mut self) -> (Vec<Stmt>, Vec<SyntaxError>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while self.tokens.peek().is_some() {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(error),
            }
        }

        (statements, errors)
    }

    fn parse_statement(&mut self) -> Result<Stmt, SyntaxError> {
        Ok(Stmt::Expr(self.parse_expression()?))
    }

    fn parse_expression(&mut self) -> Result<Handle, SyntaxError> {
        let mut expr = self.parse_precedence1()?;
        while let Found(_) = eat!(self.tokens, Token::Add) {
            let right = self.parse_precedence1()?;
            expr = self.exprs.add(Expr::Binary {
                left: expr,
                op: BinaryOp::Add,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence1(&mut self) -> Result<Handle, SyntaxError> {
        match dbg!(eat!(self.tokens, Token::Number(_))) {
            Found(Token::Number(num)) => Ok(self.exprs.add(Expr::Literal(Literal::Number(
                num.parse().expect("Failed to parse numeric literal"),
            )))),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Stmt {
    Expr(Handle),
}

#[derive(Debug, Copy, Clone)]
pub enum Expr {
    Binary {
        left: Handle,
        op: BinaryOp,
        right: Handle,
    },
    Literal(Literal),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Number(f64),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Handle(u32);

#[derive(Debug, Clone)]
pub struct Pool<T> {
    pool: Vec<T>,
}

impl<T> Pool<T> {
    fn new() -> Self {
        Self {
            pool: Vec::with_capacity(1000),
        }
    }
    fn add(&mut self, data: T) -> Handle {
        self.pool.push(data);
        Handle((self.pool.len() - 1) as u32)
    }

    fn get(&self, handle: Handle) -> &T {
        &self.pool[handle.0 as usize]
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
}

#[derive(Debug, Copy, Clone)]
pub enum SyntaxError {}
