use pom_lexer::token::{
    Token, TokenExt,
    TokenKind::{self, *},
    Tokens,
};
use pom_utils::{arena::Id, span::Span};

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
                let bind = self.parse_bind_with_lhs(lhs, &[Semicolon], checkpoint);
                StmtKind::Bind(bind)
            }
            Err(_) => StmtKind::Expr(expr),
        };

        // Don't bother looking for a semicolon if the expr wasn't successfully parsed.
        if !matches!(self.ast.exprs[expr].kind, ExprKind::Invalid(_)) {
            self.expect(Semicolon, checkpoint);
        }

        self.ast.stmts.push(Stmt {
            kind: stmt,
            span: self.spanned_since(checkpoint),
        })
    }

    fn parse_bind(&mut self, sentinels: &[TokenKind]) -> Bind {
        let checkpoint = self.checkpoint();

        let lhs = self.parse_expr();

        self.expect(Colon, checkpoint);

        self.parse_bind_with_lhs(lhs, sentinels, checkpoint)
    }

    fn parse_bind_with_lhs(
        &mut self,
        lhs: Id<Expr>,
        sentinels: &[TokenKind],
        checkpoint: usize,
    ) -> Bind {
        if self.grab(Equal).is_ok() {
            let rhs = self.parse_expr();
            return Bind {
                lhs,
                kind: BindKind::Infer,
                rhs: Some(rhs),
            };
        }

        let kind = match self.grab_any(&[Fn, Type]).kind() {
            Ok(Fn) => {
                let mut params = Vec::new();

                // Make a new checkpoint when parsing the parameter list for better error reporting.
                let checkpoint = self.checkpoint();

                if self.grab(LParen).is_ok() {
                    while self.grab(RParen).is_err() {
                        let bind = self.parse_bind(&[Comma, RParen]);
                        params.push(bind);

                        match self.expect_any(&[Comma, RParen], checkpoint).kind() {
                            Ok(Comma) => continue,
                            Ok(RParen) => break,

                            // Break on error to avoid getting potentially stuck in a loop finding the delimiter pair.
                            Err(_) => break,

                            _ => {
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
            Err(token) if sentinels.contains(&token.kind) => {
                _ = self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken {
                        // NOTE: This is a best effort list. The actual valid tokens here are [Fn, Type, {arbitrary expression}],
                        //       but it doesn't seem very useful to dump out all of the expression tokens.
                        wanted: vec![Fn, Type, Ident],
                        got: token,
                    },
                    span: self.spanned_since(checkpoint),
                });
                BindKind::Infer
            }

            Err(_) => {
                let expr = self.parse_expr();
                BindKind::Expr(expr)
            }

            _ => {
                unreachable!("The possible patterns are constrained by a previous `grab`.")
            }
        };

        let wanted = [&[Equal], sentinels].concat();

        match self.expect_any(&wanted, checkpoint).kind() {
            Ok(Equal) => {
                let rhs = self.parse_expr();
                Bind {
                    lhs,
                    kind,
                    rhs: Some(rhs),
                }
            }
            // Finding a sentinel after successfully parsing the kind indicates an uninitialized binding, that is, a binding with no rhs.
            Ok(_) => {
                // Give back the sentinel we just consumed. The callers of this function expect it to still be there when it returns.
                self.cursor -= 1;
                Bind {
                    lhs,
                    kind,
                    rhs: None,
                }
            }
            Err(err) => {
                // To differentiate between an uninitialized binding and one with a parsing error, set the rhs to an invalid expression instead of None.
                let rhs = self.ast.exprs.push(Expr {
                    kind: ExprKind::Invalid(err),
                    span: self.spanned_since(checkpoint),
                });
                Bind {
                    lhs,
                    kind,
                    rhs: Some(rhs),
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

                match self.expect_any(&[Comma, RParen], checkpoint).kind() {
                    Ok(Comma) => continue,
                    Ok(RParen) => break,

                    // Break on error to avoid getting potentially stuck in a loop finding the delimiter pair.
                    Err(_) => break,

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

        let token = match self.expect_any(&[Bool, Float, Ident, Int, LBrace, LParen], checkpoint) {
            Ok(token) => token,
            Err(err) => {
                return self.ast.exprs.push(Expr {
                    kind: ExprKind::Invalid(err),
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

                    self.expect(RParen, checkpoint);

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

            let stmt = self.parse_stmt();
            stmts.push(stmt);
        }

        stmts
    }

    fn grab(&mut self, wanted: TokenKind) -> Result<Token, Token> {
        self.grab_with(|token| token == wanted)
    }

    fn grab_any(&mut self, wanted: &[TokenKind]) -> Result<Token, Token> {
        self.grab_with(|token| wanted.contains(&token))
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

    fn expect(&mut self, wanted: TokenKind, checkpoint: usize) -> Option<Id<Error>> {
        match self.grab(wanted) {
            Ok(_) => None,
            Err(token) => Some(self.errors.push(Error {
                kind: ErrorKind::UnexpectedToken {
                    wanted: vec![wanted],
                    got: token,
                },
                span: self.spanned_since(checkpoint),
            })),
        }
    }

    fn expect_any(&mut self, wanted: &[TokenKind], checkpoint: usize) -> Result<Token, Id<Error>> {
        match self.grab_any(wanted) {
            Ok(token) => Ok(token),
            Err(token) => {
                let err = self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken {
                        wanted: wanted.to_vec(),
                        got: token,
                    },
                    span: self.spanned_since(checkpoint),
                });

                // Consume the invalid token if we haven't reached the end of the token stream.
                if self.tokens[self.cursor].kind != Eof {
                    self.cursor += 1;
                }

                Err(err)
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
