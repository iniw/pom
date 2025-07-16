use pom_lexer::token::{
    Token,
    TokenKind::{self, *},
    Tokens,
};
use pom_utils::{arena::Id, span::Span};

use crate::{
    Errors,
    ast::{
        Ast,
        expr::{BinaryOp, Expr, ExprKind, Literal},
        stmt::{BindKind, Stmt, StmtKind},
    },
    error::{Error, ErrorKind, ErrorOr},
};

pub struct Parser<'src> {
    src: &'src str,
    tokens: Tokens,

    ast: Ast,
    errors: Errors,

    cursor: usize,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str, tokens: Tokens) -> Self {
        Self {
            src,
            tokens,

            ast: Ast::default(),
            errors: Errors::new(),

            cursor: 0,
        }
    }

    pub fn parse(mut self) -> (Ast, Errors) {
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
        (self.ast, self.errors)
    }

    fn parse_stmt(&mut self) -> ErrorOr<Id<Stmt>> {
        let cp = self.checkpoint();

        match self.grab(LBrace) {
            Ok(_) => {
                let stmts = self.parse_block(cp)?;
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
                        let kind = match self.grab_any(&[Fn, Type]).map(|token| token.kind) {
                            Ok(Fn) => BindKind::Fn,
                            Ok(Type) => BindKind::Type,
                            Ok(_) => {
                                unreachable!(
                                    "The possible patterns are constrained by a previous `grab`."
                                )
                            }
                            Err(_) => BindKind::Expr(self.parse_expr()?),
                        };

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
                _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
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
                _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
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

        const WANTED: &[TokenKind] = &[Bool, Float, Ident, Int, LBrace, LParen];

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
                    kind: ExprKind::Literal(Literal::Bool(true)),
                    span: self.spanned_since(cp),
                })),
                "false" => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Literal(Literal::Bool(false)),
                    span: self.spanned_since(cp),
                })),
                _ => unreachable!(
                    "The lexer should never produce `Bool` for anything other than 'true' or 'false'"
                ),
            },
            Float => match token.span.text(self.src).parse() {
                Ok(float) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Literal(Literal::Float(float)),
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
                    kind: ExprKind::Literal(Literal::Int(int)),
                    span: self.spanned_since(cp),
                })),
                Err(err) => Err(Error {
                    kind: ErrorKind::InvalidIntLiteral(err),
                    span: self.spanned_since(cp),
                }),
            },
            LBrace => {
                let stmts = self.parse_block(cp)?;
                Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Block(stmts),
                    span: self.spanned_since(cp),
                }))
            }
            LParen => match self.grab(RParen) {
                Ok(_) => Ok(self.ast.exprs.push(Expr {
                    kind: ExprKind::Tuple(Vec::new()),
                    span: self.spanned_since(cp),
                })),
                Err(_) => {
                    let expr = self.parse_expr()?;

                    // TODO: Parse tuples

                    self.grab(RParen).map_err(|token| Error {
                        kind: ErrorKind::UnexpectedToken {
                            wanted: &[RParen],
                            got: token,
                        },
                        span: self.spanned_since(cp),
                    })?;

                    Ok(self.ast.exprs.push(Expr {
                        kind: ExprKind::Paren(expr),
                        span: self.spanned_since(cp),
                    }))
                }
            },
            _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
        }
    }

    fn parse_block(&mut self, checkpoint: usize) -> ErrorOr<Vec<Id<Stmt>>> {
        let mut stmts = Vec::new();

        while let Err(token) = self.grab(RBrace) {
            if token.kind == Eof {
                return Err(Error {
                    kind: ErrorKind::UnbalancedBlock,
                    span: self.spanned_since(checkpoint),
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

        Ok(stmts)
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

    fn chase_with(&mut self, f: impl std::ops::Fn(TokenKind) -> bool) -> Option<Token> {
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
