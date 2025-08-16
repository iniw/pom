use pom_parser::ast::Ast;

use crate::{
    ir::Ir,
    lowering::{error::Errors as LoweringErrors, lower},
    typecheck::{error::Errors as TypecheckErrors, typecheck},
};

pub mod ir;
mod lowering;
mod typecheck;

pub fn analyse(src: &str, ast: Ast) -> (Ir, LoweringErrors, TypecheckErrors) {
    let (mut ir, lowering_errors) = lower(src, ast);

    let typecheck_errors = typecheck(&mut ir);

    (ir, lowering_errors, typecheck_errors)
}

mod ast {
    use pom_parser::ast::{expr, stmt};

    pub type Bind = stmt::Bind;
    pub type BindKind = stmt::TypeAnnotation;
    pub type Stmt = stmt::Stmt;
    pub type StmtKind = stmt::StmtKind;

    pub type BinaryOp = expr::BinaryOp;
    pub type Expr = expr::Expr;
    pub type ExprKind = expr::ExprKind;
    pub type Literal = expr::Literal;
}
