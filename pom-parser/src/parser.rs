use pom_lexer::token::{
    Token,
    TokenKind::{self, *},
    Tokens,
};
use pom_utils::{arena::Id, span::Span};
use smallvec::smallvec;

use crate::{
    Errors,
    ast::{
        Ast,
        expr::{BinaryOp, Expr, ExprKind, Literal},
        stmt::{Bind, BindKind, Stmt, StmtKind},
    },
    error::{Error, ErrorKind},
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
            let stmt = self.parse_stmt();
            self.ast.items.push(stmt);
        }
        (self.ast, self.errors)
    }

    fn parse_stmt(&mut self) -> Id<Stmt> {
        let checkpoint = self.checkpoint();

        match self.grab(LBrace) {
            Ok(_) => {
                let stmts = self.parse_block(checkpoint);
                self.ast.stmts.push(Stmt {
                    kind: StmtKind::Block(stmts),
                    span: self.spanned_since(checkpoint),
                })
            }
            Err(_) => self.parse_expr_or_bind(),
        }
    }

    fn parse_expr_or_bind(&mut self) -> Id<Stmt> {
        let checkpoint = self.checkpoint();

        let expr = self.parse_expr();

        let stmt = match self.grab(Colon) {
            Ok(_) => {
                // We're in a binding, the expr we just parsed becomes the lhs.
                let lhs = expr;
                let bind = self.parse_bind_with_lhs(checkpoint, lhs, &[Semicolon]);
                StmtKind::Bind(bind)
            }
            Err(_) => StmtKind::Expr(expr),
        };

        // Don't bother looking for a semicolon if the expr wasn't successfully parsed.
        if !matches!(self.ast.exprs[expr].kind, ExprKind::Invalid(_)) {
            if let Err(token) = self.grab(Semicolon) {
                _ = self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken {
                        wanted: smallvec![Semicolon],
                        got: token,
                    },
                    span: self.spanned_since(checkpoint),
                });
            }
        }

        self.ast.stmts.push(Stmt {
            kind: stmt,
            span: self.spanned_since(checkpoint),
        })
    }

    fn parse_bind(&mut self, sentinels: &[TokenKind]) -> Bind {
        let checkpoint = self.checkpoint();

        let lhs = self.parse_expr();

        if let Err(token) = self.grab(Colon) {
            _ = self.errors.push(Error {
                kind: ErrorKind::UnexpectedToken {
                    wanted: smallvec![Colon],
                    got: token,
                },
                span: self.spanned_since(checkpoint),
            })
        }

        self.parse_bind_with_lhs(checkpoint, lhs, sentinels)
    }

    fn parse_bind_with_lhs(
        &mut self,
        checkpoint: usize,
        lhs: Id<Expr>,
        sentinels: &[TokenKind],
    ) -> Bind {
        if self.grab(Equal).is_ok() {
            let rhs = self.parse_expr();
            return Bind {
                lhs,
                kind: BindKind::Infer,
                rhs: Some(rhs),
            };
        }

        let kind = match self.grab_any(&[Fn, Type]).map(|token| token.kind) {
            Ok(Fn) => {
                let mut params = Vec::new();

                // Generate a new checkpoint when parsing the parameter list for better error reporting.
                let checkpoint = self.checkpoint();

                if self.grab(LParen).is_ok() && self.grab(RParen).is_err() {
                    loop {
                        params.push(self.parse_bind(&[Comma, RParen]));

                        match self.grab_any(&[Comma, RParen]).map(|token| token.kind) {
                            Ok(Comma) => continue,
                            Ok(RParen) => break,
                            Err(token) => {
                                _ = self.errors.push(Error {
                                    kind: ErrorKind::UnexpectedToken {
                                        wanted: smallvec![Comma, RParen],
                                        got: token,
                                    },
                                    span: self.spanned_since(checkpoint),
                                });
                                break;
                            }
                            Ok(_) => {
                                unreachable!(
                                    "The possible patterns are constrained by a previous `grab`."
                                )
                            }
                        }
                    }
                }

                BindKind::Fn { params }
            }
            Ok(Type) => BindKind::Type,
            // Infer the kind of bindings when prematurely reaching a sentinel.
            // This helps reconstruct a valid AST in weird cases such as:
            // `f: fn (a:) = a;`
            // `f: fn (a) = a;`
            Err(token) if sentinels.contains(&token.kind) => BindKind::Infer,
            Err(_) => BindKind::Expr(self.parse_expr()),
            Ok(_) => {
                unreachable!("The possible patterns are constrained by a previous `grab`.")
            }
        };

        match self.grab(Equal) {
            Ok(_) => {
                let rhs = self.parse_expr();
                Bind {
                    lhs,
                    kind,
                    rhs: Some(rhs),
                }
            }
            // Finding a sentinel after successfully parsing the kind indicates an uninitialized binding, that is, a binding with no rhs.
            Err(token) if sentinels.contains(&token.kind) => Bind {
                lhs,
                kind,
                rhs: None,
            },
            Err(token) => {
                let mut wanted = smallvec![Equal];
                wanted.extend_from_slice(sentinels);

                _ = self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken { wanted, got: token },
                    span: self.spanned_since(checkpoint),
                });

                Bind {
                    lhs,
                    kind,
                    rhs: None,
                }
            }
        }
    }

    fn parse_expr(&mut self) -> Id<Expr> {
        self.parse_expr_precedence0()
    }

    fn parse_expr_precedence0(&mut self) -> Id<Expr> {
        let checkpoint = self.checkpoint();

        let mut lhs = self.parse_expr_precedence1();

        while let Ok(token) = self.grab_any(&[Plus, Minus]) {
            let rhs = self.parse_expr_precedence1();

            let op = match token.kind {
                Plus => BinaryOp::Add,
                Minus => BinaryOp::Sub,
                _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.spanned_since(checkpoint),
            });
        }

        lhs
    }

    fn parse_expr_precedence1(&mut self) -> Id<Expr> {
        let checkpoint = self.checkpoint();

        let mut lhs = self.parse_expr_precedence2();

        while let Ok(token) = self.grab_any(&[Star, Slash]) {
            let rhs = self.parse_expr_precedence2();

            let op = match token.kind {
                Star => BinaryOp::Mul,
                Slash => BinaryOp::Div,
                _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
            };

            lhs = self.ast.exprs.push(Expr {
                kind: ExprKind::Binary { lhs, op, rhs },
                span: self.spanned_since(checkpoint),
            });
        }

        lhs
    }

    fn parse_expr_precedence2(&mut self) -> Id<Expr> {
        let checkpoint = self.checkpoint();

        let mut callable = self.parse_expr_precedence3();

        while self.grab(LParen).is_ok() {
            let mut args = Vec::new();

            while self.grab(RParen).is_err() {
                let arg = self.parse_expr();
                args.push(arg);

                match self.grab_any(&[Comma, RParen]).map(|token| token.kind) {
                    Ok(Comma) => continue,
                    Ok(RParen) => break,
                    Err(token) => {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::UnexpectedToken {
                                wanted: smallvec![Comma, RParen],
                                got: token,
                            },
                            span: self.spanned_since(checkpoint),
                        });
                        break;
                    }
                    _ => {
                        unreachable!("The possible patterns are constrained by a previous `grab`.")
                    }
                }
            }

            callable = self.ast.exprs.push(Expr {
                kind: ExprKind::Call { callable, args },
                span: self.spanned_since(checkpoint),
            });
        }

        callable
    }

    fn parse_expr_precedence3(&mut self) -> Id<Expr> {
        let checkpoint = self.checkpoint();

        let wanted = smallvec![Bool, Float, Ident, Int, LBrace, LParen];

        let token = match self.grab_any(&wanted) {
            Ok(token) => token,
            Err(token) => {
                return self.invalid_expr(Error {
                    kind: ErrorKind::UnexpectedToken { wanted, got: token },
                    span: self.spanned_since(checkpoint),
                });
            }
        };

        match token.kind {
            Bool => {
                let bool = match token.span.text(self.src) {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(
                        "The lexer should never produce `Bool` for anything other than 'true' or 'false'"
                    ),
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Literal(Literal::Bool(bool)),
                    span: self.spanned_since(checkpoint),
                })
            }
            Float => {
                let float = match token.span.text(self.src).parse() {
                    Ok(float) => float,
                    Err(err) => {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::InvalidFloatLiteral(err),
                            span: self.spanned_since(checkpoint),
                        });
                        // Use a dummy literal in the case of a parse error.
                        0.0
                    }
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Literal(Literal::Float(float)),
                    span: self.spanned_since(checkpoint),
                })
            }
            Ident => self.ast.exprs.push(Expr {
                kind: ExprKind::Ident,
                span: self.spanned_since(checkpoint),
            }),
            Int => {
                let int = match token.span.text(self.src).parse() {
                    Ok(int) => int,
                    Err(err) => {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::InvalidIntLiteral(err),
                            span: self.spanned_since(checkpoint),
                        });
                        // Use a dummy literal in the case of a parse error.
                        0
                    }
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Literal(Literal::Int(int)),
                    span: self.spanned_since(checkpoint),
                })
            }
            LBrace => {
                let stmts = self.parse_block(checkpoint);
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Block(stmts),
                    span: self.spanned_since(checkpoint),
                })
            }
            LParen => match self.grab(RParen) {
                Ok(_) => self.ast.exprs.push(Expr {
                    kind: ExprKind::Tuple(Vec::new()),
                    span: self.spanned_since(checkpoint),
                }),
                Err(_) => {
                    let expr = self.parse_expr();

                    // TODO: Parse tuples.

                    if let Err(token) = self.grab(RParen) {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::UnexpectedToken {
                                wanted: smallvec![RParen],
                                got: token,
                            },
                            span: self.spanned_since(checkpoint),
                        })
                    }

                    self.ast.exprs.push(Expr {
                        kind: ExprKind::Paren(expr),
                        span: self.spanned_since(checkpoint),
                    })
                }
            },
            _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
        }
    }

    fn parse_block(&mut self, checkpoint: usize) -> Vec<Id<Stmt>> {
        let mut stmts = Vec::new();

        while let Err(token) = self.grab(RBrace) {
            if token.kind == Eof {
                _ = self.errors.push(Error {
                    kind: ErrorKind::UnbalancedBlock,
                    span: self.spanned_since(checkpoint),
                });
                break;
            }

            stmts.push(self.parse_stmt());
        }

        stmts
    }

    fn invalid_expr(&mut self, err: Error) -> Id<Expr> {
        // Consume the invalid token if we haven't reached the end of the token stream.
        if self.tokens[self.cursor].kind != Eof {
            self.cursor += 1;
        }

        let span = err.span;
        let err = self.errors.push(err);
        self.ast.exprs.push(Expr {
            kind: ExprKind::Invalid(err),
            span,
        })
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
