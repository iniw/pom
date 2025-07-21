use pom_utils::arena::{Arena, Id};

use crate::{
    ast,
    ir::{
        Builtins, Ir, Sym, SymKind, Type,
        expr::{Expr, ExprKind},
        stmt::{Bind, Stmt, StmtKind},
    },
};

pub fn typecheck(ir: &mut Ir) {
    TypeChecker::new().check(ir);
}

struct TypeChecker {}

impl TypeChecker {
    fn new() -> Self {
        Self {}
    }

    fn check(mut self, ir: &mut Ir) {
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

        match symbols[bind.sym].kind {
            SymKind::Type(ref mut ty) => Self::check_type(ty, rhs_ty),
            SymKind::Expr(expr) => {
                let mut kind_ty = self.check_expr(stmts, exprs, symbols, builtins, expr);
                Self::check_type(&mut kind_ty, rhs_ty);
            }
            _ => todo!(),
        }
    }

    fn check_expr(
        &mut self,
        _stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &Arena<Sym>,
        builtins: &Builtins,
        expr: Id<Expr>,
    ) -> Option<Id<Type>> {
        match exprs[expr].kind {
            ExprKind::Ident(sym) => match symbols[sym].kind {
                SymKind::Type(ty) => ty,
                _ => todo!(),
            },
            ExprKind::Literal(literal) => match literal {
                ast::Literal::Int(_) => Some(builtins.i32),
                ast::Literal::Bool(_) => Some(builtins.bool),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    fn check_type(checked: &mut Option<Id<Type>>, ty: Option<Id<Type>>) {
        match (&checked, ty) {
            (Some(checked), Some(ty)) => assert_eq!(*checked, ty),
            (None, ty) => *checked = ty,
            (Some(_), None) => (),
        }
    }
}
