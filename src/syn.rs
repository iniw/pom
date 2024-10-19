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

    override_expr: Option<Spanned<Expr<'lex>>>,
}

pub struct ParseResult<'lex> {
    pub outer_stmts: Pool<Spanned<Stmt<'lex>>>,
    pub stmts: Pool<Spanned<Stmt<'lex>>>,
    pub exprs: Pool<Spanned<Expr<'lex>>>,
    pub errors: Vec<Error>,
}

impl<'lex, I: Iterator<Item = Spanned<Token<'lex>>> + std::fmt::Debug> Parser<'lex, I> {
    pub fn new(tokens: impl IntoIterator<IntoIter = I>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),

            stmts: Pool::new(),
            exprs: Pool::new(),
            outer_stmts: Pool::new(),

            errors: Vec::new(),

            override_expr: None,
        }
    }

    pub fn parse(mut self) -> ParseResult<'lex> {
        self.parse_program();

        ParseResult {
            outer_stmts: self.outer_stmts,
            stmts: self.stmts,
            exprs: self.exprs,
            errors: self.errors,
        }
    }

    fn parse_program(&mut self) {
        while let Missing(_) = chase!(self.tokens, Token::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => _ = self.outer_stmts.add(stmt),
                Err(err) => self.handle_error(err),
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Spanned<Stmt<'lex>>, Error> {
        let stmt_start = self.span_start();

        match chase!(self.tokens, Token::Symbol(_) | Token::Print) {
            Caught(Spanned(token, span)) => match token {
                Token::Symbol(identifier) => match chase!(self.tokens, Token::Colon) {
                    Caught(_) => match chase!(self.tokens, Token::Equal | Token::Fn) {
                        Caught(Spanned(token, _)) => match token {
                            Token::Equal => {
                                let expr = self.parse_expression_and_semicolon()?;
                                Ok(self.new_stmt(
                                    Stmt::SymbolDecl(SymbolDecl {
                                        identifier,
                                        info: SymbolInfo::Var(VarInfo::Value(expr)),
                                    }),
                                    stmt_start,
                                ))
                            }
                            Token::Fn => {
                                if let Missing(token) = chase!(self.tokens, Token::Equal) {
                                    return Err(Error(Spanned(
                                        ErrorKind::UnexpectedToken,
                                        token.span(),
                                    )));
                                }

                                let expr = self.parse_expression_and_semicolon()?;
                                Ok(self.new_stmt(
                                    Stmt::SymbolDecl(SymbolDecl {
                                        identifier,
                                        info: SymbolInfo::Fn(expr),
                                    }),
                                    stmt_start,
                                ))
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
                        self.parse_statement_expression(stmt_start)
                    }
                },
                Token::Print => {
                    let expr = self.parse_expression_and_semicolon()?;
                    Ok(self.new_stmt(Stmt::Print(expr), stmt_start))
                }
                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            },
            Missing(_) => self.parse_statement_expression(stmt_start),
        }
    }

    fn parse_statement_expression(
        &mut self,
        stmt_start: u32,
    ) -> Result<Spanned<Stmt<'lex>>, Error> {
        let expr = self.parse_expression_and_semicolon()?;
        Ok(self.new_stmt(Stmt::Expr(expr), stmt_start))
    }

    fn parse_expression_and_semicolon(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr = self.parse_expression()?;

        if let Missing(token) = chase!(self.tokens, Token::Semicolon) {
            return Err(Error(Spanned(ErrorKind::ExpectedSemicolon, token.span())));
        }

        Ok(expr)
    }

    fn parse_expression(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        self.parse_precedence0()
    }

    fn parse_precedence0(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.span_start();
        let mut expr = self.parse_precedence1()?;

        while let Caught(Spanned(token, _)) = chase!(self.tokens, Token::Plus | Token::Minus) {
            let op = match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            let right = self.parse_precedence1()?;
            expr = self.add_expr(
                Expr::Binary {
                    left: expr,
                    op,
                    right,
                },
                expr_start,
            );
        }

        Ok(expr)
    }

    fn parse_precedence1(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.span_start();
        let mut expr = self.parse_precedence2()?;

        while let Caught(Spanned(token, _)) = chase!(self.tokens, Token::Slash | Token::Star) {
            let op = match token {
                Token::Slash => BinaryOp::Div,
                Token::Star => BinaryOp::Mul,
                _ => unreachable!(),
            };

            let right = self.parse_precedence2()?;
            expr = self.add_expr(
                Expr::Binary {
                    left: expr,
                    op,
                    right,
                },
                expr_start,
            );
        }

        Ok(expr)
    }

    fn parse_precedence2(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.span_start();
        let mut expr = self.parse_precedence3()?;

        while let Caught(Spanned(_, span)) = chase!(self.tokens, Token::LeftParenthesis) {
            if let Missing(token) = chase!(self.tokens, Token::RightParenthesis) {
                return Err(Error(Spanned(
                    ErrorKind::UnclosedFunctionParenthesis(span),
                    token.span(),
                )));
            }
            expr = self.add_expr(Expr::Call(expr), expr_start);
        }

        Ok(expr)
    }

    fn parse_precedence3(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        if let Some(Spanned(expr, span)) = self.override_expr.take() {
            return Ok(self.add_spanned_expr(expr, span));
        }

        let expr_start = self.span_start();
        match chase!(
            self.tokens,
            Token::Symbol(_) | Token::Number(_) | Token::LeftBrace | Token::LeftParenthesis
        ) {
            Caught(Spanned(token, span)) => match token {
                Token::Symbol(name) => Ok(self.add_expr(Expr::Symbol(name), expr_start)),
                Token::Number(number) => {
                    let number = number
                        .parse()
                        .map_err(|_| Error(Spanned(ErrorKind::InvalidNumericLiteral, span)))?;
                    Ok(self.add_expr(Expr::Literal(Literal::Number(number)), expr_start))
                }
                Token::LeftBrace => {
                    let mut stmts = Vec::new();
                    while let Missing(token) = chase!(self.tokens, Token::RightBrace) {
                        if let Token::EndOfFile = *token {
                            return Err(Error(Spanned(ErrorKind::UnclosedBraces, span)));
                        }

                        match self.parse_statement() {
                            Ok(stmt) => stmts.push(self.stmts.add(stmt)),
                            Err(err) => self.handle_error(err),
                        }
                    }
                    Ok(self.add_expr(Expr::Block(stmts), expr_start))
                }
                Token::LeftParenthesis => {
                    let expr = self.parse_expression()?;

                    if let Missing(token) = chase!(self.tokens, Token::RightParenthesis) {
                        return Err(Error(Spanned(
                            ErrorKind::UnclosedExpressionParenthesis(span),
                            token.span(),
                        )));
                    }

                    Ok(expr)
                }

                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            },
            Missing(token) => Err(Error(Spanned(ErrorKind::ExpectedExpression, token.span()))),
        }
    }

    fn new_stmt(&mut self, stmt: Stmt<'lex>, span_start: u32) -> Spanned<Stmt<'lex>> {
        let span = Span {
            start: span_start,
            end: self.span_start(),
        };
        Spanned(stmt, span)
    }

    fn add_expr(&mut self, expr: Expr<'lex>, span_start: u32) -> Handle<Spanned<Expr<'lex>>> {
        let end = self.span_start();
        self.add_spanned_expr(
            expr,
            Span {
                start: span_start,
                end,
            },
        )
    }

    fn add_spanned_expr(&mut self, expr: Expr<'lex>, span: Span) -> Handle<Spanned<Expr<'lex>>> {
        self.exprs.add(Spanned(expr, span))
    }

    fn span_start(&mut self) -> u32 {
        self.tokens.peek().unwrap().span().start
    }

    fn handle_error(&mut self, error: Error) {
        self.errors.push(error);
        self.sync();
    }

    fn sync(&mut self) {
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
    Call(Handle<Spanned<Expr<'lex>>>),
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
                format!("Invalid numeric literal {}.", span.render(source_code))
            }
            ErrorKind::ExpectedExpression => {
                format!(
                    "Expected expression instead of {}.",
                    span.render(source_code)
                )
            }
            ErrorKind::ExpectedSemicolon => {
                format!(
                    "Expected semicolon to finalize statement, got {} instead.",
                    span.render(source_code)
                )
            }
            ErrorKind::UnclosedBraces => {
                format!(
                    "Unclosed braces beginning @ {}:{}",
                    span.line(source_code),
                    span.column(source_code),
                )
            }
            ErrorKind::UnclosedFunctionParenthesis(opening) => {
                format!(
                    "Unclosed parenthesis for function call beginning @ {}:{}, got {} instead.",
                    opening.line(source_code),
                    opening.column(source_code),
                    span.render(source_code)
                )
            }
            ErrorKind::UnclosedExpressionParenthesis(opening) => {
                format!(
                    "Unclosed parenthesis for parenthesized expression beginning @ {}:{}, got {} instead.",
                    opening.line(source_code),
                    opening.column(source_code),
                    span.render(source_code)
                )
            }
            ErrorKind::UnexpectedKind => {
                format!("Unexpected symbol kind {}.", span.render(source_code))
            }
            ErrorKind::UnexpectedToken => {
                format!("Unexpected token {}.", span.render(source_code))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumericLiteral,

    ExpectedExpression,
    ExpectedSemicolon,

    UnclosedBraces,
    UnclosedFunctionParenthesis(Span),
    UnclosedExpressionParenthesis(Span),

    UnexpectedKind,
    UnexpectedToken,
}

#[derive(Debug, Copy, Clone)]
enum ChaseResult<'lex> {
    Caught(Spanned<Token<'lex>>),
    Missing(Spanned<Token<'lex>>),
}
use ChaseResult::*;
