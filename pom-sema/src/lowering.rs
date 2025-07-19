use pom_parser::ast::Ast;
use pom_utils::arena::Id;

use crate::{
    ast,
    ir::{
        Ir, Sym, SymKind,
        expr::{Expr, ExprKind},
        stmt::{Bind, Stmt, StmtKind},
    },
    lowering::error::{Error, ErrorKind, Errors},
};

pub mod error;
#[cfg(test)]
mod tests;

pub struct Lowering<'src> {
    src: &'src str,

    ir: Ir,
    errors: Errors,

    scope: Vec<(&'src str, Id<Sym>)>,
}

impl<'src> Lowering<'src> {
    pub fn new(src: &'src str) -> Self {
        let mut s = Self {
            src,

            ir: Ir::default(),
            errors: Errors::new(),

            scope: Vec::new(),
        };

        // Builtin symbols:
        _ = s.decl_sym(
            "i32",
            Sym {
                kind: SymKind::Type,
            },
        );

        s
    }

    pub fn lower(mut self, ast: Ast) -> (Ir, Errors) {
        for stmt in &ast.items {
            let stmt = self.lower_stmt(&ast, *stmt);
            self.ir.items.push(stmt);
        }
        (self.ir, self.errors)
    }

    fn lower_stmt(&mut self, ast: &Ast, stmt: Id<ast::Stmt>) -> Id<Stmt> {
        let span = ast.stmts[stmt].span;
        match ast.stmts[stmt].kind {
            ast::StmtKind::Bind(ref bind) => {
                let bind = self.lower_bind(ast, bind);
                self.ir.stmts.push(Stmt {
                    kind: StmtKind::Bind(bind),
                    span,
                })
            }
            ast::StmtKind::Block(ref stmts) => {
                let stmts = self.lower_block(ast, stmts);
                self.ir.stmts.push(Stmt {
                    kind: StmtKind::Block(stmts),
                    span,
                })
            }
            ast::StmtKind::Expr(expr) => {
                let expr = self.lower_expr(ast, expr);
                self.ir.stmts.push(Stmt {
                    kind: StmtKind::Expr(expr),
                    span,
                })
            }
        }
    }

    fn lower_bind(&mut self, ast: &Ast, bind: &ast::Bind) -> Bind {
        // Symbols generated while lowering the "right" (the kind and rhs) part of a bind (e.g: function parameters)
        // are scoped to the bind itself and do not affect it's outer scope.
        // For example, in the following snippet:
        // `f: (b: i32) = b;`
        // `b` is pushed to scope when lowering the function's params and made available to the it's rhs.
        // After the rhs is lowered, the scope is restored to where it was before, so `b` is no longer available.
        let scope_checkpoint = self.scope.len();

        let kind = match bind.kind {
            ast::BindKind::Expr(expr) => SymKind::Expr(self.lower_expr(ast, expr)),
            ast::BindKind::Fn { ref params } => {
                let params = params
                    .iter()
                    .map(|param| self.lower_bind(ast, param))
                    .collect();
                SymKind::Fn { params }
            }
            ast::BindKind::Infer => SymKind::Infer,
            ast::BindKind::Type => SymKind::Type,
        };

        let rhs = bind.rhs.map(|rhs| self.lower_expr(ast, rhs));

        self.scope.truncate(scope_checkpoint);

        match ast.exprs[bind.lhs].kind {
            ast::ExprKind::Ident => {
                let name = ast.exprs[bind.lhs].span.text(self.src);
                let sym = self.decl_sym(name, Sym { kind });
                let lhs = self.lower_expr(ast, bind.lhs);
                Bind { lhs, sym, rhs }
            }
            ref expr => unimplemented!("Only identifiers can appear in binds. ({:?})", expr),
        }
    }

    fn lower_block(&mut self, ast: &Ast, stmts: &[Id<ast::Stmt>]) -> Vec<Id<Stmt>> {
        let scope_checkpoint = self.scope.len();

        let stmts = stmts
            .iter()
            .map(|stmt| self.lower_stmt(ast, *stmt))
            .collect();

        self.scope.truncate(scope_checkpoint);

        stmts
    }

    fn lower_expr(&mut self, ast: &Ast, expr: Id<ast::Expr>) -> Id<Expr> {
        let span = ast.exprs[expr].span;
        match ast.exprs[expr].kind {
            ast::ExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.lower_expr(ast, lhs);
                let rhs = self.lower_expr(ast, rhs);
                self.ir.exprs.push(Expr {
                    kind: ExprKind::Binary { lhs, op, rhs },
                    span,
                })
            }
            ast::ExprKind::Block(ref stmts) => {
                let stmts = self.lower_block(ast, stmts);
                self.ir.exprs.push(Expr {
                    kind: ExprKind::Block(stmts),
                    span,
                })
            }
            ast::ExprKind::Call { callable, ref args } => {
                let callable = self.lower_expr(ast, callable);
                let args = args.iter().map(|arg| self.lower_expr(ast, *arg)).collect();
                self.ir.exprs.push(Expr {
                    kind: ExprKind::Call { callable, args },
                    span,
                })
            }
            ast::ExprKind::Ident => {
                let ident = span.text(self.src);
                match self.scope.iter().rfind(|(str, _)| ident == *str) {
                    Some((_, id)) => self.ir.exprs.push(Expr {
                        kind: ExprKind::Ident(*id),
                        span,
                    }),
                    None => self.invalid_expr(Error {
                        kind: ErrorKind::UnknownSymbol,
                        span,
                    }),
                }
            }
            ast::ExprKind::Literal(literal) => self.ir.exprs.push(Expr {
                kind: ExprKind::Literal(literal),
                span,
            }),
            ast::ExprKind::Paren(expr) => self.lower_expr(ast, expr),
            ast::ExprKind::Tuple(ref exprs) => {
                unimplemented!("Tuples aren't lowered yet. ({:?})", exprs)
            }
            ast::ExprKind::Invalid(err) => self.invalid_expr(Error {
                kind: ErrorKind::ParserError(err),
                span,
            }),
        }
    }

    fn decl_sym(&mut self, name: &'src str, sym: Sym) -> Id<Sym> {
        let sym = self.ir.symbols.push(sym);
        self.scope.push((name, sym));
        sym
    }

    fn invalid_expr(&mut self, err: Error) -> Id<Expr> {
        let span = err.span;
        let err = self.errors.push(err);
        self.ir.exprs.push(Expr {
            kind: ExprKind::Invalid(err),
            span,
        })
    }
}
