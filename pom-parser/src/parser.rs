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
        expr::{BinaryOp, Expr, ExprKind, FnKind},
        stmt::{Bind, Stmt, StmtKind, TypeAnnotation},
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
        self.parse_expr_or_bind()
    }

    fn parse_expr_or_bind(&mut self) -> Id<Stmt> {
        let mark = self.mark();

        let expr = self.parse_expr();

        let stmt = match self.grab(Colon) {
            Ok(_) => {
                // We're in a binding, the expr we just parsed becomes the lhs.
                let lhs = expr;
                let bind = self.parse_bind_with_lhs(lhs, &[Semicolon]);
                StmtKind::Bind(bind)
            }
            Err(_) => StmtKind::Expr(expr),
        };

        // Don't bother looking for a semicolon if the expr wasn't successfully parsed.
        if !matches!(self.ast.exprs[expr].kind, ExprKind::Invalid(_)) {
            // Allow tail expressions in blocks: skip `;` if next is `}`
            if !self.peek_is(RBrace) {
                self.expect(Semicolon, mark);
            }
        }

        self.ast.stmts.push(Stmt {
            kind: stmt,
            span: self.spanned_since(mark),
        })
    }

    fn parse_bind_with_lhs(&mut self, lhs: Id<Expr>, sentinels: &[TokenKind]) -> Bind {
        if self.grab(Equal).is_ok() {
            let rhs = self.parse_expr();
            return Bind {
                lhs,
                ty: None,
                rhs: Some(rhs),
            };
        }

        let ty = match self.grab(Type) {
            Ok(_) => Some(TypeAnnotation::Type),
            Err(_) => {
                let expr = self.parse_expr();
                Some(TypeAnnotation::Expr(expr))
            }
        };

        let wanted = [&[Equal], sentinels].concat();

        let mark = self.mark();

        match self.expect_any(&wanted, mark).kind() {
            Ok(Equal) => {
                let rhs = self.parse_expr();
                Bind {
                    lhs,
                    ty,
                    rhs: Some(rhs),
                }
            }
            // Finding a sentinel after successfully parsing the kind indicates an uninitialized binding, that is, a binding with no rhs.
            Ok(_) => {
                // Give back the sentinel we just consumed. The callers of this function expect it to still be there when it returns.
                self.cursor -= 1;
                Bind { lhs, ty, rhs: None }
            }
            Err(err) => {
                // To differentiate between an uninitialized binding and one with a parsing error, set the rhs to an invalid expression instead of None.
                let rhs = self.ast.exprs.push(Expr {
                    kind: ExprKind::Invalid(err),
                    span: self.spanned_since(mark),
                });
                Bind {
                    lhs,
                    ty,
                    rhs: Some(rhs),
                }
            }
        }
    }

    fn parse_expr(&mut self) -> Id<Expr> {
        self.parse_expr_fn_type()
    }

    fn parse_expr_fn_type(&mut self) -> Id<Expr> {
        let mark = self.mark();

        let lhs = self.parse_expr_precedence0();

        if self.grab(Arrow).is_ok() {
            let rhs = self.parse_expr_fn_type(); // right-associative

            self.ast.exprs.push(Expr {
                kind: ExprKind::FnType { lhs, rhs },
                span: self.spanned_since(mark),
            })
        } else {
            lhs
        }
    }

    fn parse_expr_precedence0(&mut self) -> Id<Expr> {
        let mark = self.mark();

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
                span: self.spanned_since(mark),
            });
        }

        lhs
    }

    fn parse_expr_precedence1(&mut self) -> Id<Expr> {
        let mark = self.mark();

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
                span: self.spanned_since(mark),
            });
        }

        lhs
    }

    fn parse_expr_precedence2(&mut self) -> Id<Expr> {
        let mark = self.mark();

        let mut callable = self.parse_expr_precedence3();

        while self.grab(LParen).is_ok() {
            let mut args = Vec::new();

            while self.grab(RParen).is_err() {
                let arg = self.parse_expr();
                args.push(arg);

                match self.expect_any(&[Comma, RParen], mark).kind() {
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
                span: self.spanned_since(mark),
            });
        }

        callable
    }

    fn parse_expr_precedence3(&mut self) -> Id<Expr> {
        let mark = self.mark();

        let token =
            match self.expect_any(&[At, Bool, Dollar, Float, Ident, Int, LBrace, LParen], mark) {
                Ok(token) => token,
                Err(err) => {
                    return self.ast.exprs.push(Expr {
                        kind: ExprKind::Invalid(err),
                        span: self.spanned_since(mark),
                    });
                }
            };

        match token.kind {
            At => self.parse_fn_expr(FnKind::Static, mark),
            Bool => {
                let bool = match token.span.text(self.src) {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(
                        "The lexer should never produce `Bool` for anything other than 'true' or 'false'"
                    ),
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Bool(bool),
                    span: self.spanned_since(mark),
                })
            }
            Dollar => self.parse_fn_expr(FnKind::Closure, mark),
            Float => {
                let float = match token.span.text(self.src).parse() {
                    Ok(float) => float,
                    Err(err) => {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::InvalidFloatLiteral(err),
                            span: self.spanned_since(mark),
                        });
                        // Use a dummy literal in the case of a parse error.
                        0.0
                    }
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Float(float),
                    span: self.spanned_since(mark),
                })
            }
            Ident => self.ast.exprs.push(Expr {
                kind: ExprKind::Ident,
                span: self.spanned_since(mark),
            }),
            Int => {
                let int = match token.span.text(self.src).parse() {
                    Ok(int) => int,
                    Err(err) => {
                        _ = self.errors.push(Error {
                            kind: ErrorKind::InvalidIntLiteral(err),
                            span: self.spanned_since(mark),
                        });
                        // Use a dummy literal in the case of a parse error.
                        0
                    }
                };
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Int(int),
                    span: self.spanned_since(mark),
                })
            }
            LBrace => {
                let stmts = self.parse_block(mark);
                self.ast.exprs.push(Expr {
                    kind: ExprKind::Block(stmts),
                    span: self.spanned_since(mark),
                })
            }
            LParen => self.parse_tuple(mark),
            _ => unreachable!("The possible patterns are constrained by a previous `grab`."),
        }
    }

    fn parse_fn_expr(&mut self, kind: FnKind, mark: usize) -> Id<Expr> {
        self.expect(LParen, mark);

        let params = self.parse_param_list(mark);

        let ret = if self.grab(Arrow).is_ok() {
            Some(self.parse_expr())
        } else {
            None
        };

        let body = self.parse_expr();

        self.ast.exprs.push(Expr {
            kind: ExprKind::Fn {
                kind,
                params,
                ret,
                body,
            },
            span: self.spanned_since(mark),
        })
    }

    fn parse_param_list(&mut self, mark: usize) -> Vec<Bind> {
        let mut params = Vec::new();

        while self.grab(RParen).is_err() {
            let param = self.parse_param();
            params.push(param);

            match self.expect_any(&[Comma, RParen], mark).kind() {
                Ok(Comma) => continue,
                Ok(RParen) => break,

                // Break on error to avoid getting potentially stuck in a loop finding the delimiter pair.
                Err(_) => break,

                _ => {
                    unreachable!("The possible patterns are constrained by a previous `grab`.")
                }
            }
        }

        params
    }

    fn parse_param(&mut self) -> Bind {
        let expr = self.parse_expr();

        // If we see a colon, this is `name: type [= default]`
        if self.grab(Colon).is_ok() {
            let lhs = expr;
            return self.parse_bind_with_lhs(lhs, &[Comma, RParen]);
        }

        // Bare param: just an expression (could be a name or a type, disambiguation deferred to sema)
        Bind {
            lhs: expr,
            ty: None,
            rhs: None,
        }
    }

    fn parse_tuple(&mut self, mark: usize) -> Id<Expr> {
        // `()` -> empty tuple
        if self.grab(RParen).is_ok() {
            return self.ast.exprs.push(Expr {
                kind: ExprKind::Tuple(Vec::new()),
                span: self.spanned_since(mark),
            });
        }

        // Parse the first expression
        let first_expr = self.parse_expr();

        // `:` -> named tuple element, parse as bind
        if self.grab(Colon).is_ok() {
            let first_bind = self.parse_bind_with_lhs(first_expr, &[Comma, RParen]);
            let mut elements = vec![first_bind];

            // If comma follows, parse more params
            if self.grab(Comma).is_ok() {
                while self.grab(RParen).is_err() {
                    let param = self.parse_param();
                    elements.push(param);

                    match self.expect_any(&[Comma, RParen], mark).kind() {
                        Ok(Comma) => continue,
                        Ok(RParen) => break,
                        Err(_) => break,
                        _ => unreachable!(
                            "The possible patterns are constrained by a previous `grab`."
                        ),
                    }
                }
            } else {
                self.expect(RParen, mark);
            }

            return self.ast.exprs.push(Expr {
                kind: ExprKind::Tuple(elements),
                span: self.spanned_since(mark),
            });
        }

        // `,` -> positional tuple with multiple elements (or trailing comma)
        if self.grab(Comma).is_ok() {
            let first_bind = Bind {
                lhs: first_expr,
                ty: None,
                rhs: None,
            };
            let mut elements = vec![first_bind];

            while self.grab(RParen).is_err() {
                let param = self.parse_param();
                elements.push(param);

                match self.expect_any(&[Comma, RParen], mark).kind() {
                    Ok(Comma) => continue,
                    Ok(RParen) => break,
                    Err(_) => break,
                    _ => {
                        unreachable!("The possible patterns are constrained by a previous `grab`.")
                    }
                }
            }

            return self.ast.exprs.push(Expr {
                kind: ExprKind::Tuple(elements),
                span: self.spanned_since(mark),
            });
        }

        // `)` -> 1-element tuple
        self.expect(RParen, mark);

        let bind = Bind {
            lhs: first_expr,
            ty: None,
            rhs: None,
        };

        self.ast.exprs.push(Expr {
            kind: ExprKind::Tuple(vec![bind]),
            span: self.spanned_since(mark),
        })
    }

    fn parse_block(&mut self, mark: usize) -> Vec<Id<Stmt>> {
        let mut stmts = Vec::new();

        while let Err(token) = self.grab(RBrace) {
            if token.kind == Eof {
                _ = self.errors.push(Error {
                    kind: ErrorKind::UnbalancedBlock,
                    span: self.spanned_since(mark),
                });
                break;
            }

            let stmt = self.parse_stmt();
            stmts.push(stmt);
        }

        stmts
    }

    fn peek_is(&self, wanted: TokenKind) -> bool {
        self.tokens[self.cursor].kind == wanted
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

    fn expect(&mut self, wanted: TokenKind, mark: usize) -> Option<Id<Error>> {
        match self.grab(wanted) {
            Ok(_) => None,
            Err(token) => Some(self.errors.push(Error {
                kind: ErrorKind::UnexpectedToken {
                    wanted: vec![wanted],
                    got: token,
                },
                span: self.spanned_since(mark),
            })),
        }
    }

    fn expect_any(&mut self, wanted: &[TokenKind], mark: usize) -> Result<Token, Id<Error>> {
        match self.grab_any(wanted) {
            Ok(token) => Ok(token),
            Err(token) => {
                let err = self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken {
                        wanted: wanted.to_vec(),
                        got: token,
                    },
                    span: self.spanned_since(mark),
                });

                // Consume the invalid token if we haven't reached the end of the token stream.
                if self.tokens[self.cursor].kind != Eof {
                    self.cursor += 1;
                }

                Err(err)
            }
        }
    }

    fn mark(&self) -> usize {
        self.cursor
    }

    fn spanned_since(&self, mark: usize) -> Span {
        let previous = self.cursor.saturating_sub(1);
        Span {
            start: self.tokens[usize::min(mark, previous)].span.start,
            end: self.tokens[previous].span.end,
        }
    }
}
