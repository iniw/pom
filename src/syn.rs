use std::iter::Peekable;

use crate::{
    lex::{Span, Spanned, Token},
    pool::{Handle, Pool},
};

macro_rules! spanned_chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(ctx) = $tokens.next_if(|next| match next.data {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            SpannedChaseResult::SpannedCaught(ctx.data, ctx.span)
        } else {
            SpannedChaseResult::SpannedMissing(*$tokens.peek().expect("ICE: Shouldn't reach end of token stream when chasing."))
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

pub struct Parser<'lex, I: Iterator + std::fmt::Debug> {
    tokens: Peekable<I>,
    exprs: Pool<Expr<'lex>>,
    stmts: Pool<Stmt<'lex>>,
    outer_stmts: Pool<Stmt<'lex>>,
    errors: Vec<Error>,
    override_expr: Option<Handle<Expr<'lex>>>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>> + std::fmt::Debug> Parser<'lex, I> {
    pub fn new(tokens: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            exprs: Pool::new(),
            stmts: Pool::new(),
            outer_stmts: Pool::new(),
            errors: Vec::new(),
            override_expr: None,
        }
    }

    pub fn parse(
        mut self,
    ) -> (
        Pool<Stmt<'lex>>,
        Pool<Stmt<'lex>>,
        Pool<Expr<'lex>>,
        Vec<Error>,
    ) {
        self.parse_program();
        (self.outer_stmts, self.stmts, self.exprs, self.errors)
    }

    fn parse_program(&mut self) {
        while let Missing(_) = chase!(self.tokens, Token::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => {
                    self.outer_stmts.add(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt<'lex>, Error> {
        match chase!(self.tokens, Token::Print | Token::Symbol(_)) {
            Caught(Token::Symbol(name)) => match chase!(self.tokens, Token::Colon) {
                Caught(_) => match chase!(self.tokens, Token::Equal | Token::Fn) {
                    Caught(Token::Equal) => {
                        let expr = self.parse_expression_and_semicolon()?;
                        Ok(Stmt::SymbolDecl(SymbolDecl {
                            identifier: name,
                            info: SymbolInfo::Var(VarInfo::Value(expr)),
                        }))
                    }
                    Caught(Token::Fn) => {
                        if let Missing(token) = chase!(self.tokens, Token::Equal) {
                            return Err(Error {
                                kind: ErrorKind::UnexpectedToken,
                                span: token.span,
                            });
                        }

                        let expr = self.parse_expression_and_semicolon()?;
                        Ok(Stmt::SymbolDecl(SymbolDecl {
                            identifier: name,
                            info: SymbolInfo::Fn(expr),
                        }))
                    }
                    Missing(token) => Err(Error {
                        kind: ErrorKind::UnknownKind,
                        span: token.span,
                    }),

                    _ => unimplemented!("Implement other kinds"),
                },
                Missing(_) => {
                    self.override_expr = Some(self.exprs.add(Expr::Symbol(name)));
                    self.parse_statement_expression()
                }
            },
            Caught(Token::Print) => {
                let expr = self.parse_expression_and_semicolon()?;
                Ok(Stmt::Print(expr))
            }
            Missing(_) => self.parse_statement_expression(),

            // This is just here to make the compiler shut up about missing match arms.
            _ => unreachable!(),
        }
    }

    fn parse_statement_expression(&mut self) -> Result<Stmt<'lex>, Error> {
        let expr = self.parse_expression_and_semicolon()?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expression(&mut self) -> Result<Handle<Expr<'lex>>, Error> {
        self.parse_precedence0()
    }

    fn parse_expression_and_semicolon(&mut self) -> Result<Handle<Expr<'lex>>, Error> {
        let expr = self.parse_expression()?;

        if let Missing(token) = chase!(self.tokens, Token::Semicolon) {
            return Err(Error {
                kind: ErrorKind::MissingSemicolon,
                span: token.span,
            });
        }

        Ok(expr)
    }

    fn parse_precedence0(&mut self) -> Result<Handle<Expr<'lex>>, Error> {
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

    fn parse_precedence1(&mut self) -> Result<Handle<Expr<'lex>>, Error> {
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

    fn parse_precedence2(&mut self) -> Result<Handle<Expr<'lex>>, Error> {
        if let Some(override_expr) = self.override_expr.take() {
            return Ok(override_expr);
        }

        match spanned_chase!(
            self.tokens,
            Token::Number(_) | Token::Symbol(_) | Token::LeftBrace | Token::LeftParenthesis
        ) {
            SpannedCaught(Token::Number(number), span) => {
                let number = number.parse().map_err(|_| Error {
                    kind: ErrorKind::InvalidNumericLiteral,
                    span,
                })?;
                Ok(self.exprs.add(Expr::Literal(Literal::Number(number))))
            }

            SpannedCaught(Token::LeftBrace, _) => {
                let mut exprs = Vec::new();
                while let Missing(_) = chase!(self.tokens, Token::RightBrace) {
                    let stmt = self.parse_statement()?;
                    exprs.push(self.stmts.add(stmt));
                }
                Ok(self.exprs.add(Expr::Block(exprs)))
            }

            SpannedCaught(Token::Symbol(name), _) => Ok(self.exprs.add(Expr::Symbol(name))),

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
            _ => unreachable!(),
        }
    }

    fn synchronize(&mut self) {
        // Consume tokens until we hit a semicolon, indicating the end of a statement. (or EOF).
        while let Some(token) = self.tokens.peek() {
            match token.data {
                Token::Semicolon => {
                    self.tokens.next();
                    return;
                }
                Token::EndOfFile => return,
                _ => {
                    // Consume
                    self.tokens.next();
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<'lex> {
    Expr(Handle<Expr<'lex>>),
    SymbolDecl(SymbolDecl<'lex>),
    Print(Handle<Expr<'lex>>),
}

#[derive(Debug, Clone)]
pub struct SymbolDecl<'lex> {
    pub identifier: &'lex str,
    pub info: SymbolInfo<'lex>,
}

#[derive(Debug, Clone)]
pub enum Expr<'lex> {
    Binary {
        left: Handle<Expr<'lex>>,
        op: BinaryOp,
        right: Handle<Expr<'lex>>,
    },
    Block(Vec<Handle<Stmt<'lex>>>),
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
    Value(Handle<Expr<'lex>>),
    TypeAndValue(&'lex str, Handle<Expr<'lex>>),
}

#[derive(Debug, Copy, Clone)]
pub enum SymbolInfo<'lex> {
    Var(VarInfo<'lex>),
    Fn(Handle<Expr<'lex>>),
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
            ErrorKind::UnknownKind => {
                format!("Unexpected symbol kind {}", self.span.render(source_code))
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
    UnknownKind,
}

#[derive(Debug, Copy, Clone)]
enum ChaseResult<'lex> {
    Caught(Token<'lex>),
    Missing(Spanned<Token<'lex>>),
}
use ChaseResult::*;

#[derive(Debug, Copy, Clone)]
enum SpannedChaseResult<'lex> {
    SpannedCaught(Token<'lex>, Span),
    SpannedMissing(Spanned<Token<'lex>>),
}
use SpannedChaseResult::*;
