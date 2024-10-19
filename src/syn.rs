use std::iter::Peekable;

use crate::{
    lex::{Span, Spanned, Token},
    pool::{Handle, Pool},
};

macro_rules! chase {
    ($tokens:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        if let Some(token) = $tokens.next_if(|next| match **next {
                $pattern $(if $guard)? => true,
                _ => false,
            }) {
            ChaseResult::Caught(token)
        } else {
            ChaseResult::Missing(*$tokens.peek().expect("ICE: Shouldn't reach end of token stream when chasing."))
        }
    }};
}

pub struct Parser<'lex, I: Iterator + std::fmt::Debug> {
    tokens: Peekable<I>,

    stmts: Pool<Spanned<Stmt<'lex>>>,
    exprs: Pool<Spanned<Expr<'lex>>>,
    outer_stmts: Pool<Spanned<Stmt<'lex>>>,

    errors: Vec<Error>,

    stmts_span_stack: Vec<Span>,
    exprs_span_stack: Vec<Span>,

    override_expr: Option<Spanned<Expr<'lex>>>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>> + std::fmt::Debug> Parser<'lex, I> {
    pub fn new(tokens: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),

            stmts: Pool::new(),
            exprs: Pool::new(),
            outer_stmts: Pool::new(),

            errors: Vec::new(),

            stmts_span_stack: Vec::new(),
            // FIXME: Do this correctly. Fuck.
            exprs_span_stack: Vec::new(),

            override_expr: None,
        }
    }

    pub fn parse(
        mut self,
    ) -> (
        Pool<Spanned<Stmt<'lex>>>,
        Pool<Spanned<Stmt<'lex>>>,
        Pool<Spanned<Expr<'lex>>>,
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

    fn parse_statement(&mut self) -> Result<Spanned<Stmt<'lex>>, Error> {
        self.statement_span_checkpoint();

        match chase!(self.tokens, Token::Symbol(_) | Token::Print) {
            Caught(Spanned(token, span)) => match token {
                Token::Symbol(identifier) => match chase!(self.tokens, Token::Colon) {
                    Caught(_) => match chase!(self.tokens, Token::Equal | Token::Fn) {
                        Caught(Spanned(token, _)) => match token {
                            Token::Equal => {
                                let expr = self.parse_expression_and_semicolon()?;
                                Ok(self.new_stmt(Stmt::SymbolDecl(SymbolDecl {
                                    identifier,
                                    info: SymbolInfo::Var(VarInfo::Value(expr)),
                                })))
                            }
                            Token::Fn => {
                                if let Missing(token) = chase!(self.tokens, Token::Equal) {
                                    return Err(Error(Spanned(
                                        ErrorKind::UnexpectedToken,
                                        token.span(),
                                    )));
                                }

                                let expr = self.parse_expression_and_semicolon()?;
                                Ok(self.new_stmt(Stmt::SymbolDecl(SymbolDecl {
                                    identifier,
                                    info: SymbolInfo::Fn(expr),
                                })))
                            }

                            // This is just here to make the compiler shut up about missing match arms.
                            _ => unreachable!(),
                        },
                        Missing(token) => {
                            Err(Error(Spanned(ErrorKind::UnexpectedKind, token.span())))
                        }
                    },
                    // Failed to find a colon, meaning this is just a normal expression beginning with
                    // a symbol.
                    // We can't backtrack on our chased tokens, so we do a little hack here for
                    // overriding the next parsed expression with the consumed symbol.
                    Missing(_) => {
                        self.override_expr = Some(Spanned(Expr::Symbol(identifier), span));
                        self.parse_statement_expression()
                    }
                },
                Token::Print => {
                    let expr = self.parse_expression_and_semicolon()?;
                    Ok(self.new_stmt(Stmt::Print(expr)))
                }
                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            },
            Missing(_) => self.parse_statement_expression(),
        }
    }

    fn parse_statement_expression(&mut self) -> Result<Spanned<Stmt<'lex>>, Error> {
        let expr = self.parse_expression_and_semicolon()?;
        Ok(self.new_stmt(Stmt::Expr(expr)))
    }

    fn parse_expression(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        self.expression_span_checkpoint();
        self.parse_precedence0()
    }

    fn parse_expression_and_semicolon(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr = self.parse_expression()?;

        if let Missing(token) = chase!(self.tokens, Token::Semicolon) {
            return Err(Error(Spanned(ErrorKind::MissingSemicolon, token.span())));
        }

        Ok(expr)
    }

    fn parse_precedence0(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        self.expression_span_checkpoint();
        let mut expr = self.parse_precedence1()?;

        while let Caught(Spanned(token, _)) = chase!(self.tokens, Token::Plus | Token::Minus) {
            let op = match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            self.expression_span_checkpoint();
            let right = self.parse_precedence1()?;
            expr = self.add_expr(Expr::Binary {
                left: expr,
                op,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence1(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        self.expression_span_checkpoint();
        let mut expr = self.parse_precedence2()?;

        while let Caught(Spanned(token, _)) = chase!(self.tokens, Token::Slash | Token::Star) {
            let op = match token {
                Token::Slash => BinaryOp::Div,
                Token::Star => BinaryOp::Mul,
                _ => unreachable!(),
            };

            self.expression_span_checkpoint();
            let right = self.parse_precedence2()?;
            expr = self.add_expr(Expr::Binary {
                left: expr,
                op,
                right,
            });
        }

        Ok(expr)
    }

    fn parse_precedence2(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        if let Some(override_expr) = self.override_expr.take() {
            *self.exprs_span_stack.last_mut().unwrap() = override_expr.span();
            return Ok(self.add_expr(override_expr.0));
        }

        match chase!(
            self.tokens,
            Token::Number(_) | Token::Symbol(_) | Token::LeftBrace | Token::LeftParenthesis
        ) {
            Caught(Spanned(token, span)) => match token {
                Token::Number(number) => {
                    let number = number
                        .parse()
                        .map_err(|_| Error(Spanned(ErrorKind::InvalidNumericLiteral, span)))?;
                    Ok(self.add_expr(Expr::Literal(Literal::Number(number))))
                }
                Token::LeftBrace => {
                    let mut exprs = Vec::new();
                    while let Missing(_) = chase!(self.tokens, Token::RightBrace) {
                        match self.parse_statement() {
                            Ok(stmt) => exprs.push(self.stmts.add(stmt)),
                            Err(err) => {
                                self.errors.push(err);
                                self.synchronize();
                            }
                        }
                    }
                    Ok(self.add_expr(Expr::Block(exprs)))
                }
                Token::Symbol(name) => Ok(self.add_expr(Expr::Symbol(name))),
                Token::LeftParenthesis => {
                    let expr = self.parse_expression()?;

                    if let Missing(token) = chase!(self.tokens, Token::RightParenthesis) {
                        return Err(Error(Spanned(
                            ErrorKind::MissingRightParenthesis,
                            token.span(),
                        )));
                    }

                    Ok(expr)
                }

                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            },
            Missing(token) => Err(Error(Spanned(ErrorKind::UnexpectedToken, token.span()))),
        }
    }

    fn new_stmt(&mut self, stmt: Stmt<'lex>) -> Spanned<Stmt<'lex>> {
        let mut span = self.stmts_span_stack.pop().unwrap();
        span.end = self.tokens.peek().unwrap().span().start;
        Spanned(stmt, span)
    }

    fn add_expr(&mut self, expr: Expr<'lex>) -> Handle<Spanned<Expr<'lex>>> {
        let mut span = self.exprs_span_stack.pop().unwrap();
        span.end = self.tokens.peek().unwrap().span().start;
        self.exprs.add(Spanned(expr, span))
    }

    fn statement_span_checkpoint(&mut self) {
        let start = self.tokens.peek().unwrap().span().start;
        // End will be filled later
        self.stmts_span_stack.push(Span { start, end: 0 });
    }

    fn expression_span_checkpoint(&mut self) {
        let start = self.tokens.peek().unwrap().span().start;
        // End will be filled later
        self.exprs_span_stack.push(Span { start, end: 0 });
    }

    fn synchronize(&mut self) {
        // Consume tokens until we hit a semicolon, indicating the end of a statement. (or EOF).
        while let Some(Spanned(token, _)) = self.tokens.peek() {
            match token {
                Token::EndOfFile => return,
                Token::Semicolon => {
                    self.tokens.next();
                    return;
                }
                _ => {
                    self.tokens.next();
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<'lex> {
    Expr(Handle<Spanned<Expr<'lex>>>),
    SymbolDecl(SymbolDecl<'lex>),
    Print(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug, Clone)]
pub struct SymbolDecl<'lex> {
    pub identifier: &'lex str,
    pub info: SymbolInfo<'lex>,
}

#[derive(Debug, Clone)]
pub enum Expr<'lex> {
    Binary {
        left: Handle<Spanned<Expr<'lex>>>,
        op: BinaryOp,
        right: Handle<Spanned<Expr<'lex>>>,
    },
    Block(Vec<Handle<Spanned<Stmt<'lex>>>>),
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
    Fn(Handle<Spanned<Expr<'lex>>>),
}

#[derive(Debug, Copy, Clone)]
pub struct Error(Spanned<ErrorKind>);

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        let Spanned(kind, span) = &self.0;
        match kind {
            ErrorKind::InvalidNumericLiteral => {
                format!("Invalid numeric literal {}", span.render(source_code))
            }
            ErrorKind::MissingRightParenthesis => {
                format!(
                    "Expected closing parenthesis at the end of a grouped expression, got {}",
                    span.render(source_code)
                )
            }
            ErrorKind::MissingSemicolon => {
                format!(
                    "Expected semicolon at the end of a statement, got {}",
                    span.render(source_code)
                )
            }
            ErrorKind::UnexpectedKind => {
                format!("Unexpected symbol kind {}", span.render(source_code))
            }
            ErrorKind::UnexpectedToken => {
                format!("Unexpected token {}", span.render(source_code))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumericLiteral,
    MissingRightParenthesis,
    MissingSemicolon,
    UnexpectedKind,
    UnexpectedToken,
}

#[derive(Debug, Copy, Clone)]
enum ChaseResult<'lex> {
    Caught(Spanned<Token<'lex>>),
    Missing(Spanned<Token<'lex>>),
}
use ChaseResult::*;
