#![expect(dead_code, unused_variables)]

use pom_utils::arena::Id;

use crate::ir::{
    Ir, Sym,
    expr::Expr,
    stmt::{Bind, Stmt, StmtKind},
};

pub fn typecheck(ir: &mut Ir) {
    TypeChecking::new().check(ir);
}

struct TypeChecking {}

impl TypeChecking {
    fn new() -> Self {
        Self {}
    }

    fn check(mut self, ir: &mut Ir) {
        for stmt in ir.items.clone() {
            self.check_stmt(ir, stmt);
        }
    }

    fn check_stmt(&mut self, ir: &mut Ir, stmt: Id<Stmt>) {
        match ir.stmts[stmt].kind {
            StmtKind::Bind(ref bind) => {
                todo!();
            }
            StmtKind::Expr(expr) => self.check_expr(ir, expr),
            _ => todo!(),
        }
    }

    fn check_bind(&mut self, ir: &mut Ir, bind: &Bind) {
        todo!()
    }

    fn check_expr(&mut self, ir: &mut Ir, expr: Id<Expr>) {
        todo!()
    }

    fn infer_type(&mut self, ir: &mut Ir, expr: Id<Expr>) -> Option<Id<Sym>> {
        todo!()
    }

    fn check_type(&mut self, ir: &mut Ir, expr: Id<Expr>, ty: Id<Sym>) {
        todo!()
    }
}
