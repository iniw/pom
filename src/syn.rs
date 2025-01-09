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

    stmts: Pool<Spanned<Stmt<'lex>>>,
    exprs: Pool<Spanned<Expr<'lex>>>,
    outer_stmts: Pool<Spanned<Stmt<'lex>>>,
    errors: Vec<Error>,
}

pub struct ParseResult<'lex> {
    pub outer_stmts: Pool<Spanned<Stmt<'lex>>>,
    pub stmts: Pool<Spanned<Stmt<'lex>>>,
    pub exprs: Pool<Spanned<Expr<'lex>>>,
    pub errors: Vec<Error>,
}

impl<'lex> Parser<'lex> {
    pub fn new(tokens: &'lex [Spanned<Token<'lex>>]) -> Self {
        Self {
            tokens,
            position: 0,

            stmts: Pool::new(),
            exprs: Pool::new(),
            outer_stmts: Pool::new(),

            errors: Vec::new(),
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
        while let Missing(_) = chase!(self, Token::EndOfFile) {
            match self.parse_statement_and_semicolon() {
                Ok(stmt) => _ = self.outer_stmts.push(stmt),
                Err(err) => self.handle_error(err),
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Spanned<Stmt<'lex>>, Error> {
        let stmt_start = self.span_start();

        match chase!(self, Token::Symbol(_), Token::Colon) {
            [Caught(Token::Symbol(identifier)), Caught(_)] => {
                match chase!(self, Token::Equal | Token::Fn) {
                    Caught(Token::Equal) => {
                        let expr = self.parse_expression()?;
                        Ok(self.new_stmt(
                            Stmt::SymbolDecl(SymbolDecl {
                                identifier,
                                info: SymbolInfo::Var(VarInfo::Value(expr)),
                            }),
                            stmt_start,
                        ))
                    }
                    Caught(Token::Fn) => {
                        if let Missing(token) = chase!(self, Token::Equal) {
                            return Err(Error(Spanned(ErrorKind::UnexpectedToken, token.span())));
                        }

                        let body = self.parse_expression()?;
                        Ok(self.new_stmt(
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
                    Ok(self.new_stmt(Stmt::Print(expr), stmt_start))
                }
                Missing(_) => {
                    let expr = self.parse_expression()?;
                    Ok(self.new_stmt(Stmt::Expr(expr), stmt_start))
                }
            },
        }
    }

    fn parse_statement_and_semicolon(&mut self) -> Result<Spanned<Stmt<'lex>>, Error> {
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
        let expr_start = self.span_start();
        let mut expr = self.parse_precedence1()?;

        while let Caught(token) = chase!(self, Token::Plus | Token::Minus) {
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

        while let Caught(token) = chase!(self, Token::Slash | Token::Star) {
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

        while let Caught(Spanned(_, span)) = spanned_chase!(self, Token::LeftParenthesis) {
            if let Missing(token) = chase!(self, Token::RightParenthesis) {
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
        let expr_start = self.span_start();
        match spanned_chase!(
            self,
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

                    while let Missing(token) = chase!(self, Token::RightBrace) {
                        if let Token::EndOfFile = *token {
                            return Err(Error(Spanned(ErrorKind::UnclosedBraces, span)));
                        }

                        match self.parse_statement() {
                            Ok(stmt) => stmts.push(self.stmts.push(stmt)),
                            Err(err) => self.handle_error(err),
                        }

                        match chase!(self, Token::RightBrace | Token::Semicolon) {
                            Caught(Token::RightBrace) => break,
                            Caught(Token::Semicolon) => continue,

                            Missing(token) => self.handle_error(Error(Spanned(
                                ErrorKind::UnexpectedToken,
                                token.span(),
                            ))),

                            _ => unreachable!(),
                        }
                    }

                    Ok(self.add_expr(Expr::Block(stmts), expr_start))
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
        self.exprs.push(Spanned(expr, span))
    }

    fn span_start(&mut self) -> u32 {
        self.peek().unwrap().span().start
    }

    fn handle_error(&mut self, error: Error) {
        self.errors.push(error);
        self.sync();
    }

    fn sync(&mut self) {
        // Consume tokens until we hit a semicolon, indicating the end of a statement. (or EOF).
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

    fn peek(&self) -> Option<&Spanned<Token<'lex>>> {
        self.tokens.get(self.position)
    }
}
