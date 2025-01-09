use crate::{
    lex::{
        span::{Span, Spanned},
        Token,
    },
    pool::{Handle, Pool},
};

pub mod ast;
pub mod chase;
pub mod error;

use ast::{BinaryOp, Expr, Literal, Stmt, SymbolDecl, SymbolInfo, VarInfo};
use chase::{
    chase, spanned_chase,
    ChaseResult::{Caught, Missing},
};
use error::{Error, ErrorKind};

#[derive(Debug)]
pub struct Parser<'lex> {
    tokens: &'lex [Spanned<Token<'lex>>],
    position: usize,

    result: ParseResult<'lex>,
}

#[derive(Debug)]
pub struct ParseResult<'lex> {
    pub global_stmts: Vec<Handle<Spanned<Stmt<'lex>>>>,

    pub stmts: Pool<Spanned<Stmt<'lex>>>,
    pub exprs: Pool<Spanned<Expr<'lex>>>,

    pub errors: Vec<Error>,
}

impl<'lex> Parser<'lex> {
    pub fn new(tokens: &'lex [Spanned<Token<'lex>>]) -> Self {
        Self {
            tokens,
            position: 0,

            result: ParseResult {
                global_stmts: Vec::with_capacity(1_000),

                stmts: Pool::with_capacity(10_000),
                exprs: Pool::with_capacity(100_000),

                errors: Vec::new(),
            },
        }
    }

    pub fn parse(mut self) -> ParseResult<'lex> {
        self.parse_program();
        self.result
    }

    fn parse_program(&mut self) {
        while let Missing(_) = chase!(self, Token::EndOfFile) {
            match self.parse_statement_and_semicolon() {
                Ok(stmt) => self.result.global_stmts.push(stmt),
                Err(err) => self.push_error(err),
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Handle<Spanned<Stmt<'lex>>>, Error> {
        let stmt_start = self.current_token_start();

        match chase!(self, Token::Symbol(_), Token::Colon) {
            [Caught(Token::Symbol(identifier)), Caught(_)] => {
                match chase!(self, Token::Equal | Token::Fn) {
                    Caught(Token::Equal) => {
                        let value = self.parse_expression()?;
                        Ok(self.push_stmt(
                            Stmt::SymbolDecl(SymbolDecl {
                                identifier,
                                info: SymbolInfo::Var(VarInfo::Value(value)),
                            }),
                            stmt_start,
                        ))
                    }
                    Caught(Token::Fn) => {
                        if let Missing(token) = chase!(self, Token::Equal) {
                            return Err(Error(Spanned(ErrorKind::UnexpectedToken, token.span())));
                        }

                        let body = self.parse_expression()?;
                        Ok(self.push_stmt(
                            Stmt::SymbolDecl(SymbolDecl {
                                identifier,
                                info: SymbolInfo::Fn(body),
                            }),
                            stmt_start,
                        ))
                    }

                    Missing(token) => Err(Error(Spanned(ErrorKind::UnexpectedKind, token.span()))),

                    // This is just here to make the compiler shut up about missing match arms.
                    _ => unreachable!(),
                }
            }
            _ => match chase!(self, Token::Print) {
                Caught(_) => {
                    let expr = self.parse_expression()?;
                    Ok(self.push_stmt(Stmt::Print(expr), stmt_start))
                }
                Missing(_) => {
                    let expr = self.parse_expression()?;
                    Ok(self.push_stmt(Stmt::Expr(expr), stmt_start))
                }
            },
        }
    }

    fn parse_statement_and_semicolon(&mut self) -> Result<Handle<Spanned<Stmt<'lex>>>, Error> {
        let stmt = self.parse_statement()?;

        if let Missing(token) = chase!(self, Token::Semicolon) {
            return Err(Error(Spanned(ErrorKind::ExpectedSemicolon, token.span())));
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        self.parse_precedence0()
    }

    fn parse_precedence0(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.current_token_start();

        let mut lhs = self.parse_precedence1()?;

        while let Caught(token) = chase!(self, Token::Plus | Token::Minus) {
            let op = match token {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,

                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            };

            let rhs = self.parse_precedence1()?;
            lhs = self.push_expr(Expr::Binary { lhs, op, rhs }, expr_start);
        }

        Ok(lhs)
    }

    fn parse_precedence1(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.current_token_start();

        let mut lhs = self.parse_precedence2()?;

        while let Caught(token) = chase!(self, Token::Slash | Token::Star) {
            let op = match token {
                Token::Slash => BinaryOp::Div,
                Token::Star => BinaryOp::Mul,

                // This is just here to make the compiler shut up about missing match arms.
                _ => unreachable!(),
            };

            let rhs = self.parse_precedence2()?;
            lhs = self.push_expr(Expr::Binary { lhs, op, rhs }, expr_start);
        }

        Ok(lhs)
    }

    fn parse_precedence2(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.current_token_start();

        let mut expr = self.parse_precedence3()?;

        while let Caught(left_paren) = spanned_chase!(self, Token::LeftParenthesis) {
            if let Missing(token) = chase!(self, Token::RightParenthesis) {
                return Err(Error(Spanned(
                    ErrorKind::UnclosedFunctionParenthesis(left_paren.span()),
                    token.span(),
                )));
            }
            expr = self.push_expr(Expr::Call(expr), expr_start);
        }

        Ok(expr)
    }

    fn parse_precedence3(&mut self) -> Result<Handle<Spanned<Expr<'lex>>>, Error> {
        let expr_start = self.current_token_start();

        match spanned_chase!(
            self,
            Token::Symbol(_) | Token::Number(_) | Token::LeftBrace | Token::LeftParenthesis
        ) {
            Caught(Spanned(token, span)) => match token {
                Token::Symbol(name) => Ok(self.push_expr(Expr::Symbol(name), expr_start)),
                Token::Number(number) => {
                    let number = number
                        .parse()
                        .map_err(|_| Error(Spanned(ErrorKind::InvalidNumericLiteral, span)))?;

                    Ok(self.push_expr(Expr::Literal(Literal::Number(number)), expr_start))
                }
                Token::LeftBrace => {
                    let mut stmts = Vec::new();

                    while let Missing(token) = chase!(self, Token::RightBrace) {
                        if let Token::EndOfFile = *token {
                            return Err(Error(Spanned(ErrorKind::UnclosedBraces, span)));
                        }

                        match self.parse_statement() {
                            Ok(stmt) => {
                                stmts.push(stmt);

                                match chase!(self, Token::RightBrace | Token::Semicolon) {
                                    Caught(Token::RightBrace) => break,
                                    Caught(Token::Semicolon) => continue,

                                    Missing(token) => self.push_error(Error(Spanned(
                                        ErrorKind::ExpectedSemicolon,
                                        token.span(),
                                    ))),

                                    // This is just here to make the compiler shut up about missing match arms.
                                    _ => unreachable!(),
                                }
                            }
                            Err(err) => self.push_error(err),
                        }
                    }

                    Ok(self.push_expr(Expr::Block(stmts), expr_start))
                }
                Token::LeftParenthesis => {
                    let expr = self.parse_expression()?;

                    if let Missing(token) = chase!(self, Token::RightParenthesis) {
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

    fn push_stmt(&mut self, stmt: Stmt<'lex>, stmt_start: u32) -> Handle<Spanned<Stmt<'lex>>> {
        let stmt_end = self.current_token_start();
        self.result.stmts.push(Spanned(
            stmt,
            Span {
                start: stmt_start,
                end: stmt_end,
            },
        ))
    }

    fn push_expr(&mut self, expr: Expr<'lex>, expr_start: u32) -> Handle<Spanned<Expr<'lex>>> {
        let expr_end = self.current_token_start();
        self.result.exprs.push(Spanned(
            expr,
            Span {
                start: expr_start,
                end: expr_end,
            },
        ))
    }

    fn push_error(&mut self, error: Error) {
        self.result.errors.push(error);
        self.recover();
    }

    /// Consumes tokens until we hit a semicolon, that marks a statement boundary, or EOF.
    fn recover(&mut self) {
        while let Some(Spanned(token, _)) = self.peek() {
            match token {
                Token::EndOfFile => return,
                Token::Semicolon => {
                    self.position += 1;
                    return;
                }
                _ => {
                    self.position += 1;
                }
            }
        }
    }

    fn current_token_start(&mut self) -> u32 {
        self.peek()
            .expect("ICE: Hit end of token stream when getting the start of the current token")
            .span()
            .start
    }

    fn peek(&self) -> Option<&Spanned<Token<'lex>>> {
        self.tokens.get(self.position)
    }
}
