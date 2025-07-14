use pom_lexer::{
    Tokens,
    token::{
        Token,
        TokenKind::{self, *},
    },
};
use pom_utils::{
    arena::{Arena, Id},
    span::Span,
};

use crate::{
    Ast, Errors,
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
        while self.grab(Eof).is_err() {
            match self.parse_stmt() {
                Ok(stmt) => {
                    self.ast.items.push(stmt);
                }
                Err(err) => {
                    self.errors.push(err);
                    self.chase(Semicolon);
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        match self.grab(LBrace) {
            Ok(_) => {
                let mut stmts = Vec::new();

                while let Err(token) = self.grab(RBrace) {
                    if token.kind == Eof {
                        return Err(Error {
                            kind: ErrorKind::UnbalancedBlock,
                            span: self.spanned_since(cp),
                        });
                    }

                    match self.parse_stmt() {
                        Ok(stmt) => stmts.push(stmt),
                        Err(err) => {
                            self.errors.push(err);
                            self.chase(Semicolon);
                        }
                    }
                }

                Ok(self.ast.stmts.push(Stmt {
                    kind: StmtKind::Block(stmts),
                    span: self.spanned_since(cp),
                }))
            }
            Err(_) => self.parse_expr_or_bind(),
        }
    }

    fn parse_expr_or_bind(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        let expr = self.parse_expr()?;

        let stmt = match self.grab(Colon) {
            Ok(_) => {
                // We're in a binding, the expr we just parsed becomes the lhs.
                let lhs = expr;

                match self.grab(Equal) {
                    Ok(_) => {
                        let rhs = self.parse_expr()?;

                        StmtKind::Bind {
                            lhs,
                            kind: None,
                            rhs: Some(rhs),
                        }
                    }
                    Err(_) => {
                        let kind = self.parse_expr()?;

                        match self.grab(Equal) {
                            Ok(_) => {
                                let rhs = self.parse_expr()?;

                                StmtKind::Bind {
                                    lhs,
                                    kind: Some(kind),
                                    rhs: Some(rhs),
                                }
                            }
                            // Finding a semicolon after successfully parsing the kind indicates an uninitialized binding, that is, a binding with no rhs.
                            Err(token) if token.kind == Semicolon => StmtKind::Bind {
                                lhs,
                                kind: Some(kind),
                                rhs: None,
                            },
                            Err(token) => {
                                return Err(Error {
                                    kind: ErrorKind::UnexpectedToken {
                                        wanted: &[Equal, Semicolon],
                                        got: token,
                                    },
                                    span: self.spanned_since(cp),
                                });
                            }
                        }
                    }
                }
            }
            Err(_) => StmtKind::Expr(expr),
        };

        self.grab(Semicolon).map_err(|token| Error {
            kind: ErrorKind::UnexpectedToken {
                wanted: &[Semicolon],
                got: token,
            },
            span: self.spanned_since(cp),
        })?;

        Ok(self.ast.stmts.push(Stmt {
            kind: stmt,
            span: self.spanned_since(cp),
        }))
    }

    fn parse_expr(&mut self) -> ErrorOr<Id<Expr>> {
        self.parse_expr_precedence0()
    }

    fn parse_expr_precedence0(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_expr_precedence1()?;

        while let Ok(token) = self.grab_any(&[Plus, Minus]) {
            let rhs = self.parse_expr_precedence1()?;

            let op = match token.kind {
                Plus => BinaryOp::Add,
                Minus => BinaryOp::Sub,
                _ => unreachable!("The possible patterns are constrained by a previous match."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.spanned_since(cp),
            });
        }

        Ok(lhs)
    }

    fn parse_expr_precedence1(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        let mut lhs = self.parse_expr_precedence2()?;

        while let Ok(token) = self.grab_any(&[Star, Slash]) {
            let rhs = self.parse_expr_precedence2()?;

            let op = match token.kind {
                Star => BinaryOp::Mul,
                Slash => BinaryOp::Div,
                _ => unreachable!("The possible patterns are constrained by a previous match."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.spanned_since(cp),
            });
        }

        Ok(lhs)
    }

    fn parse_expr_precedence2(&mut self) -> ErrorOr<Id<Expr>> {
        let cp = self.checkpoint();

        const WANTED: &[TokenKind] = &[Bool, Float, Ident, Int, LParen];

        let token = self.grab_any(WANTED).map_err(|token| Error {
            kind: ErrorKind::UnexpectedToken {
                wanted: WANTED,
                got: token,
            },
            span: self.spanned_since(cp),
        })?;

        match token.kind {
            Bool => match token.span.text(self.src) {
                "true" => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Bool(true),
                    span: self.spanned_since(cp),
                })),
                "false" => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Bool(false),
                    span: self.spanned_since(cp),
                })),
                _ => unreachable!(
                    "The lexer should never produce `Bool` for anything other than 'true' or 'false'"
                ),
            },
            Float => match token.span.text(self.src).parse() {
                Ok(float) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Float(float)),
                    span: self.spanned_since(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidFloatLiteral(err),
                    span: self.spanned_since(cp),
                }),
            },
            Ident => Ok(self.ast.exprs.push(Expr {
                kind: ExprKind::Ident,
                span: self.spanned_since(cp),
            })),
            Int => match token.span.text(self.src).parse() {
                Ok(int) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Number(Number::Int(int)),
                    span: self.spanned_since(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidIntLiteral(err),
                    span: self.spanned_since(cp),
                }),
            },
            LParen => match self.grab(RParen) {
                Ok(_) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Paren(None),
                    span: self.spanned_since(cp),
                })),
                Err(_) => {
                    let expr = self.parse_expr()?;

                    self.grab(RParen).map_err(|token| Error {
                        kind: ErrorKind::UnexpectedToken {
                            wanted: &[RParen],
                            got: token,
                        },
                        span: self.spanned_since(cp),
                    })?;

                    Ok(self.ast.exprs.push(Expr {
                        kind: ExprKind::Paren(Some(expr)),
                        span: self.spanned_since(cp),
                    }))
                }
            },
            _ => unreachable!("The possible patterns are constrained by a previous match."),
        }
    }

    fn grab(&mut self, wanted: TokenKind) -> Result<Token, Token> {
        self.grab_with(|token| token == wanted)
    }

    fn grab_any(&mut self, wanted_list: &[TokenKind]) -> Result<Token, Token> {
        self.grab_with(|token| wanted_list.contains(&token))
    }

    fn grab_with(&mut self, f: impl FnOnce(TokenKind) -> bool) -> Result<Token, Token> {
        let token = self.tokens[self.cursor];
        if f(token.kind) {
            self.cursor += 1;
            Ok(token)
        } else {
            Err(token)
        }
    }

    fn chase(&mut self, wanted: TokenKind) -> Option<Token> {
        self.chase_with(|token| token == wanted)
    }

    #[expect(dead_code)]
    fn chase_any(&mut self, wanted_list: &[TokenKind]) -> Option<Token> {
        self.chase_with(|token| wanted_list.contains(&token))
    }

    fn chase_with(&mut self, f: impl Fn(TokenKind) -> bool) -> Option<Token> {
        loop {
            match self.grab_with(&f) {
                Ok(token) => return Some(token),
                Err(token) if token.kind == Eof => return None,
                _ => self.cursor += 1,
            }
        }
    }

    fn checkpoint(&self) -> usize {
        self.cursor
    }

    fn spanned_since(&self, checkpoint: usize) -> Span {
        let previous = self.cursor.saturating_sub(1);
        Span {
            start: self.tokens[usize::min(checkpoint, previous)].span.start,
            end: self.tokens[previous].span.end,
        }
    }
}

type ErrorOr<T> = Result<T, Error>;
