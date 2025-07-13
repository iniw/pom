use pom_lexer::{Tokens, token::TokenKind};
use pom_utils::{
    arena::{Arena, Id},
    span::Span,
};

use crate::{
    Ast, Errors,
    chase::{ChaseResult::*, chase},
    error::{Error, ErrorKind},
    expr::{BinaryOp, Expr, ExprKind, Number},
    stmt::{Stmt, StmtKind},
};

pub struct Parser<'src> {
    src: &'src str,
    tokens: Tokens,

    cursor: usize,
    ast: Ast,
    errors: Errors,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str, tokens: Tokens) -> Self {
        Self {
            src,
            tokens,

            cursor: 0,
            ast: Ast {
                items: Vec::new(),
                stmts: Arena::new(),
                exprs: Arena::new(),
            },
            errors: Errors::new(),
        }
    }

    pub fn parse(mut self) -> (Ast, Errors) {
        self.parse_program();
        (self.ast, self.errors)
    }

    fn parse_program(&mut self) {
        while let Missing(_) = chase!(self, TokenKind::Eof) {
            match self.parse_stmt() {
                Ok(stmt) => {
                    self.ast.items.push(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                    self.skip_to_next_statement();
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        match chase!(self, TokenKind::LBrace) {
            Caught(_) => {
                let mut stmts = Vec::new();

                while let Missing(token) = chase!(self, TokenKind::RBrace) {
                    if token.kind == TokenKind::Eof {
                        return Err(Error {
                            kind: ErrorKind::UnbalancedBlock,
                            span: self.span(cp),
                        });
                    }

                    match self.parse_stmt() {
                        Ok(stmt) => stmts.push(stmt),
                        Err(err) => {
                            self.errors.push(err);
                            self.skip_to_next_statement();
                        }
                    }
                }

                Ok(self.ast.stmts.push(Stmt {
                    kind: StmtKind::Block(stmts),
                    span: self.span(cp),
                }))
            }
            Missing(_) => self.parse_expr_or_decl(),
        }
    }

    fn parse_expr_or_decl(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        let expr = self.parse_expr()?;

        let stmt = match chase!(self, TokenKind::Colon) {
            Caught(_) => {
                // We're in a declaration, the expr we just parsed becomes the lhs.
                let lhs = expr;

                match chase!(self, TokenKind::Equal) {
                    Caught(_) => {
                        let rhs = self.parse_expr()?;

                        StmtKind::Decl {
                            lhs,
                            kind: None,
                            rhs,
                        }
                    }
                    Missing(_) => {
                        let kind = self.parse_expr()?;

                        match chase!(self, TokenKind::Equal) {
                            Caught(_) => {
                                let rhs = self.parse_expr()?;

                                StmtKind::Decl {
                                    lhs,
                                    kind: Some(kind),
                                    rhs,
                                }
                            }
                            Missing(token) => {
                                return Err(Error {
                                    kind: ErrorKind::UnexpectedToken {
                                        expected: &[TokenKind::Equal],
                                        got: token,
                                    },
                                    span: self.span(cp),
                                });
                            }
                        }
                    }
                }
            }
            Missing(_) => StmtKind::Expr(expr),
        };

        if let Missing(token) = chase!(self, TokenKind::Semicolon) {
            return Err(Error {
                kind: ErrorKind::UnexpectedToken {
                    expected: &[TokenKind::Semicolon],
                    got: token,
                },
                span: self.span(cp),
            });
        }

        Ok(self.ast.stmts.push(Stmt {
            kind: stmt,
            span: self.span(cp),
        }))
    }

    fn parse_expr(&mut self) -> ErrorOr<Id<Expr>> {
        self.parse_expr_precedence0()
    }

    fn parse_expr_precedence0(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_expr_precedence1()?;

        while let Caught(token) = chase!(self, TokenKind::Plus | TokenKind::Minus) {
            let rhs = self.parse_expr_precedence1()?;

            let op = match token.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => unreachable!("The possible patterns are constrained by a previous match."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.span(cp),
            });
        }

        Ok(lhs)
    }

    fn parse_expr_precedence1(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_expr_precedence2()?;

        while let Caught(token) = chase!(self, TokenKind::Star | TokenKind::Slash) {
            let rhs = self.parse_expr_precedence2()?;

            let op = match token.kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                _ => unreachable!("The possible patterns are constrained by a previous match."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.span(cp),
            });
        }

        Ok(lhs)
    }

    fn parse_expr_precedence2(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let token = match chase!(
            self,
            TokenKind::Bool
                | TokenKind::Float
                | TokenKind::Ident
                | TokenKind::Int
                | TokenKind::LParen
        ) {
            Caught(token) => token,
            Missing(token) => {
                return Err(Error {
                    kind: ErrorKind::UnexpectedToken {
                        expected: &[
                            TokenKind::Bool,
                            TokenKind::Float,
                            TokenKind::Ident,
                            TokenKind::Int,
                            TokenKind::LParen,
                        ],
                        got: token,
                    },
                    span: self.span(cp),
                });
            }
        };

        match token.kind {
            TokenKind::Bool => match token.span.text(self.src) {
                "true" => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Bool(true),
                    span: self.span(cp),
                })),
                "false" => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Bool(false),
                    span: self.span(cp),
                })),
                _ => unreachable!(
                    "The lexer should never produce `Bool` for anything other than 'true' or 'false'"
                ),
            },
            TokenKind::Float => match token.span.text(self.src).parse() {
                Ok(float) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Float(float)),
                    span: self.span(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidFloatLiteral(err),
                    span: self.span(cp),
                }),
            },
            TokenKind::Ident => Ok(self.ast.exprs.push(Expr {
                kind: ExprKind::Ident,
                span: self.span(cp),
            })),
            TokenKind::Int => match token.span.text(self.src).parse() {
                Ok(int) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Int(int)),
                    span: self.span(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidIntLiteral(err),
                    span: self.span(cp),
                }),
            },
            TokenKind::LParen => match chase!(self, TokenKind::RParen) {
                Caught(_) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Paren(None),
                    span: self.span(cp),
                })),
                Missing(_) => {
                    let expr = self.parse_expr()?;

                    if let Missing(token) = chase!(self, TokenKind::RParen) {
                        self.errors.push(Error {
                            kind: ErrorKind::UnexpectedToken {
                                expected: &[TokenKind::RParen],
                                got: token,
                            },
                            span: self.span(cp),
                        });
                        return Err(Error {
                            kind: ErrorKind::UnbalancedParen,
                            span: self.span(cp),
                        });
                    }

                    Ok(self.ast.exprs.push(Expr {
                        kind: ExprKind::Paren(Some(expr)),
                        span: self.span(cp),
                    }))
                }
            },
            _ => unreachable!("The possible patterns are constrained by a previous match."),
        }
    }

    fn skip_to_next_statement(&mut self) {
        while let Missing(token) = chase!(self, TokenKind::Semicolon) {
            if token.kind == TokenKind::Eof {
                return;
            }
            self.cursor += 1;
        }
    }

    fn checkpoint(&self) -> usize {
        self.cursor
    }

    fn span(&self, checkpoint: usize) -> Span {
        let previous = self.cursor.saturating_sub(1);
        Span {
            start: self.tokens[usize::min(checkpoint, previous)].span.start,
            end: self.tokens[previous].span.end,
        }
    }
}

type ErrorOr<T> = Result<T, Error>;
