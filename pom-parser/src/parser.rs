use pom_lexer::{
    Tokens,
    token::{Token, TokenKind},
};
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
    pos: usize,

    ast: Ast,
    errors: Errors,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str, tokens: Tokens) -> Self {
        Self {
            src,

            tokens,
            pos: 0,

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
            match self.parse_statement() {
                Ok(stmt) => {
                    self.ast.items.push(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                }
            }
        }
    }

    fn parse_statement(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        let expr = self.parse_expression()?;

        Ok(self.ast.stmts.push(Stmt {
            kind: StmtKind::Expr(expr),
            span: self.span(cp),
        }))
    }

    fn parse_expression(&mut self) -> ErrorOr<Id<Expr>> {
        self.parse_precedence0()
    }

    fn parse_precedence0(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_precedence1()?;

        while let Caught(token) = chase!(self, TokenKind::Plus | TokenKind::Minus) {
            let rhs = self.parse_precedence1()?;

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

    fn parse_precedence1(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_precedence2()?;

        while let Caught(token) = chase!(self, TokenKind::Star | TokenKind::Slash) {
            let rhs = self.parse_precedence2()?;

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

    fn parse_precedence2(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let token = match chase!(self, TokenKind::Int | TokenKind::Float | TokenKind::Bool(_)) {
            Caught(token) => token,
            Missing(token) => {
                return Err(Error {
                    kind: ErrorKind::UnexpectedToken(token.kind),
                    span: token.span,
                });
            }
        };

        match token.kind {
            TokenKind::Int => match token.span.text(self.src).parse() {
                Ok(int) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Int(int)),
                    span: self.span(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidIntLiteral(err),
                    span: token.span,
                }),
            },
            TokenKind::Float => match token.span.text(self.src).parse() {
                Ok(float) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Float(float)),
                    span: self.span(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidFloatLiteral(err),
                    span: token.span,
                }),
            },
            TokenKind::Bool(bool) => Ok(self.ast.exprs.push(Expr {
                kind: ExprKind::Bool(bool),
                span: token.span,
            })),
            _ => unreachable!("The possible patterns are constrained by a previous match."),
        }
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.pos).copied().unwrap_or(Token::eof())
    }

    fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.pos)
    }

    fn span(&self, checkpoint: Checkpoint) -> Span {
        Span {
            start: self.tokens[checkpoint.0].span.start,
            end: self.tokens[self.pos - 1].span.end,
        }
    }
}

type ErrorOr<T> = Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct Checkpoint(usize);
