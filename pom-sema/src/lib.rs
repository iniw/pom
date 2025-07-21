use pom_parser::ast::Ast;

use crate::{
    ir::Ir,
    lowering::{error::Errors, lower},
    typecheck::typecheck,
};

pub mod ir;
mod lowering;
mod typecheck;

pub fn analyse(src: &str, ast: Ast) -> (Ir, Errors) {
    let (mut ir, errors) = lower(src, ast);

    typecheck(&mut ir);

    (ir, errors)
}

mod ast {
    use pom_parser::ast::{expr, stmt};

    pub type Bind = stmt::Bind;
    pub type BindKind = stmt::BindKind;
    pub type Stmt = stmt::Stmt;
    pub type StmtKind = stmt::StmtKind;

    pub type BinaryOp = expr::BinaryOp;
    pub type Expr = expr::Expr;
    pub type ExprKind = expr::ExprKind;
    pub type Literal = expr::Literal;
}
