use std::iter::Peekable;

use crate::{
    lex::{Span, Spanned, Token},
    pool::{Handle, Pool},
};

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

macro_rules! chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        match spanned_chase!($tokens, $pattern $(if $guard)?) {
            SpannedChaseResult::SpannedCaught(token, _) => ChaseResult::Caught(token),
            SpannedChaseResult::SpannedMissing(token) => ChaseResult::Missing(token),
        }
    }};
}

pub struct Parser<I: Iterator + std::fmt::Debug> {
    tokens: Peekable<I>,
    exprs: Pool<Expr>,
    stmts: Pool<Stmt>,
    errors: Vec<Error>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>> + std::fmt::Debug> Parser<I> {
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
                self.synchronize();
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Handle<Stmt>, Error> {
        match chase!(self.tokens, Token::Semicolon) {
            Caught(Token::Semicolon) => Ok(self.stmts.add(Stmt::Empty)),
            Missing(_) => {
                let expr = self.parse_expression()?;

                if let Missing(token) = chase!(self.tokens, Token::Semicolon) {
                    return Err(Error {
                        kind: ErrorKind::MissingSemicolon,
                        span: token.span,
                    });
                }

                Ok(self.stmts.add(Stmt::Expr(expr)))
            }

            // This is just here to make the compiler shut up about missing match arms.
            Caught(_) => unreachable!(),
        }
    }

    fn parse_expression(&mut self) -> Result<Handle<Expr>, Error> {
        self.parse_precedence0()
    }

    fn parse_precedence0(&mut self) -> Result<Handle<Expr>, Error> {
        let mut expr = self.parse_precedence1()?;

        while let Caught(token) = chase!(self.tokens, Token::Plus | Token::Minus) {
            let op = match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            let right = self.parse_precedence1()?;
            expr = self.exprs.add(Expr::Binary {
                left: expr,
                op,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence1(&mut self) -> Result<Handle<Expr>, Error> {
        let mut expr = self.parse_precedence2()?;

        while let Caught(token) = chase!(self.tokens, Token::Slash | Token::Star) {
            let op = match token {
                Token::Slash => BinaryOp::Div,
                Token::Star => BinaryOp::Mul,
                _ => unreachable!(),
            };

            let right = self.parse_precedence2()?;
            expr = self.exprs.add(Expr::Binary {
                left: expr,
                op,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence2(&mut self) -> Result<Handle<Expr>, Error> {
        match spanned_chase!(self.tokens, Token::Number(_) | Token::LeftParenthesis) {
            SpannedCaught(Token::Number(number), span) => {
                let number = number.parse().map_err(|_| Error {
                    kind: ErrorKind::InvalidNumericLiteral,
                    span,
                })?;
                Ok(self.exprs.add(Expr::Literal(Literal::Number(number))))
            }
            SpannedCaught(Token::LeftParenthesis, _) => {
                let expr = self.parse_expression()?;

                if let Missing(token) = chase!(self.tokens, Token::RightParenthesis) {
                    return Err(Error {
                        kind: ErrorKind::MissingRightParenthesis,
                        span: token.span,
                    });
                }

                Ok(expr)
            }
            SpannedMissing(token) => Err(Error {
                kind: ErrorKind::UnexpectedToken,
                span: token.span,
            }),

            // This is just here to make the compiler shut up about missing match arms.
            SpannedCaught(..) => unreachable!(),
        }
    }

    fn synchronize(&mut self) {
        // Consume tokens until we hit the beginning of a statement (or EOF)
        while self
            .tokens
            .next_if(|token| !matches!(token.data, Token::Semicolon | Token::EndOfFile))
            .is_some()
        {}
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Stmt {
    Expr(Handle<Expr>),
    Empty,
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
    Sub,
    Div,
    Mul,
}

#[derive(Debug, Copy, Clone)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        match self.kind {
            ErrorKind::InvalidNumericLiteral => {
                format!("Invalid numeric literal {}", self.span.render(source_code))
            }
            ErrorKind::MissingSemicolon => {
                format!(
                    "Expected semicolon at the end of a statement, got {}",
                    self.span.render(source_code)
                )
            }
            ErrorKind::MissingRightParenthesis => {
                format!(
                    "Expected closing parenthesis at the end of a grouped expression, got {}",
                    self.span.render(source_code)
                )
            }
            ErrorKind::UnexpectedToken => {
                format!("Unexpected token {}", self.span.render(source_code))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumericLiteral,
    MissingSemicolon,
    MissingRightParenthesis,
    UnexpectedToken,
}

#[derive(Debug, Copy, Clone)]
enum ChaseResult<'syn, 'lex> {
    Caught(&'syn Token<'lex>),
    Missing(&'syn Spanned<Token<'lex>>),
}
use ChaseResult::*;

#[derive(Debug, Copy, Clone)]
enum SpannedChaseResult<'syn, 'lex> {
    SpannedCaught(&'syn Token<'lex>, Span),
    SpannedMissing(&'syn Spanned<Token<'lex>>),
}
use SpannedChaseResult::*;
