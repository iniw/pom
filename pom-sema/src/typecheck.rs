use pom_utils::arena::{Arena, Id};

use crate::{
    ast,
    ir::{
        Builtins, Ir, Sym, Type,
        expr::{Expr, ExprKind},
        stmt::{Bind, Stmt, StmtKind},
    },
    typecheck::error::{Error, ErrorKind, Errors},
};

pub fn typecheck(ir: &mut Ir) -> Errors {
    TypeChecker::new().check(ir)
}

pub mod error;

struct TypeChecker {
    errors: Errors,
}

impl TypeChecker {
    fn new() -> Self {
        Self {
            errors: Errors::new(),
        }
    }

    fn check(mut self, ir: &mut Ir) -> Errors {
        let Ir {
            items,
            stmts,
            exprs,

            symbols,

            types: _,
            builtins,
        } = ir;

        for stmt in items {
            self.check_stmt(stmts, exprs, symbols, builtins, *stmt);
        }

        self.errors
    }

    fn check_stmt(
        &mut self,
        stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &mut Arena<Sym>,
        builtins: &Builtins,
        stmt: Id<Stmt>,
    ) {
        match stmts[stmt].kind {
            StmtKind::Bind(bind) => self.check_bind(stmts, exprs, symbols, builtins, bind),
            StmtKind::Expr(expr) => _ = self.check_expr(stmts, exprs, symbols, builtins, expr),
            _ => todo!(),
        }
    }

    fn check_bind(
        &mut self,
        stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &mut Arena<Sym>,
        builtins: &Builtins,
        bind: Bind,
    ) {
        let rhs = bind
            .rhs
            .expect("Uninitialized expressions aren't supported yet.");

        let rhs_ty = self.check_expr(stmts, exprs, symbols, builtins, rhs);

        match symbols[bind.sym] {
            Sym::Expr(expr) => {
                let kind_ty = self.check_expr(stmts, exprs, symbols, builtins, expr);
                self.check_type(kind_ty, rhs_ty);
            }
            Sym::Type(ref mut ty) => *ty = self.check_type(*ty, rhs_ty),
            Sym::Infer(ref mut ty) => *ty = rhs_ty,
            _ => todo!(),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_expr(
        &mut self,
        stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &Arena<Sym>,
        builtins: &Builtins,
        expr: Id<Expr>,
    ) -> Option<Id<Type>> {
        match exprs[expr].kind {
            ExprKind::Ident(sym) => match symbols[sym] {
                Sym::Type(ty) => ty,
                Sym::Infer(ty) => ty,
                Sym::Expr(expr) => self.check_expr(stmts, exprs, symbols, builtins, expr),
                _ => todo!(),
            },
            ExprKind::Literal(literal) => match literal {
                ast::Literal::Bool(_) => Some(builtins.bool),
                ast::Literal::Int(_) => Some(builtins.i32),
                ast::Literal::Float(_) => Some(builtins.f32),
            },
            _ => todo!(),
        }
    }

    fn check_type(&mut self, checking: Option<Id<Type>>, ty: Option<Id<Type>>) -> Option<Id<Type>> {
        match (checking, ty) {
            (Some(checking), Some(ty)) => {
                if checking == ty {
                    Some(ty)
                } else {
                    self.errors.push(Error {
                        kind: ErrorKind::MismatchedTypes {
                            wanted: ty,
                            got: checking,
                        },
                    });
                    None
                }
            }
            (None, ty) => ty,
            (Some(_), None) => None,
        }
    }
}
