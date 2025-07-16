use pom_parser::ast::Ast;
use pom_utils::arena::Id;

use crate::{
    ast,
    ir::{
        Ir, Sym,
        expr::{Expr, ExprKind},
        stmt::{BindKind, Stmt, StmtKind},
    },
    lowering::error::{Error, ErrorKind, ErrorOr, Errors},
};

pub mod error;

pub struct Lowering<'src> {
    src: &'src str,

    ir: Ir,
    errors: Errors,

    scope: Vec<(&'src str, Id<Sym>)>,
}

impl<'src> Lowering<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,

            ir: Ir::default(),
            errors: Vec::new(),

            scope: Vec::new(),
        }
    }

    pub fn lower(mut self, ast: Ast) -> (Ir, Errors) {
        for stmt in &ast.items {
            match self.lower_stmt(&ast, *stmt) {
                Ok(stmt) => self.ir.items.push(stmt),
                Err(err) => self.errors.push(err),
            }
        }
        (self.ir, self.errors)
    }

    fn lower_stmt(&mut self, ast: &Ast, stmt: Id<ast::Stmt>) -> ErrorOr<Id<Stmt>> {
        let span = ast.stmts[stmt].span;
        match ast.stmts[stmt].kind {
            ast::StmtKind::Bind { lhs, kind, rhs } => self.lower_bind(ast, stmt, lhs, kind, rhs),
            ast::StmtKind::Block(ref stmts) => {
                let stmts = self.lower_block(ast, stmts);
                Ok(self.ir.stmts.push(Stmt {
                    kind: StmtKind::Block(stmts),
                    span,
                }))
            }
            ast::StmtKind::Expr(expr) => {
                let expr = self.lower_expr(ast, expr)?;
                Ok(self.ir.stmts.push(Stmt {
                    kind: StmtKind::Expr(expr),
                    span,
                }))
            }
        }
    }

    fn lower_bind(
        &mut self,
        ast: &Ast,
        stmt: Id<ast::Stmt>,
        lhs: Id<ast::Expr>,
        kind: Option<ast::BindKind>,
        rhs: Option<Id<ast::Expr>>,
    ) -> ErrorOr<Id<Stmt>> {
        let kind = kind
            .map(|kind| match kind {
                ast::BindKind::Fn => Ok(BindKind::Fn),
                ast::BindKind::Expr(expr) => {
                    let expr = self.lower_expr(ast, expr)?;
                    Ok(BindKind::Expr(expr))
                }
                ast::BindKind::Type => Ok(BindKind::Type),
            })
            .transpose()?;

        let rhs = rhs.map(|rhs| self.lower_expr(ast, rhs)).transpose()?;

        let sym = self.ir.symbols.push(Sym { kind, init: rhs });

        if ast.exprs[lhs].kind != ast::ExprKind::Ident {
            unimplemented!("Only identifiers can appear in binds.");
        }

        self.scope.push((ast.exprs[lhs].span.text(self.src), sym));

        let lhs = self.lower_expr(ast, lhs)?;

        Ok(self.ir.stmts.push(Stmt {
            kind: StmtKind::Bind { lhs, kind, rhs },
            span: ast.stmts[stmt].span,
        }))
    }

    fn lower_block(&mut self, ast: &Ast, ast_stmts: &Vec<Id<ast::Stmt>>) -> Vec<Id<Stmt>> {
        let scope_checkpoint = self.scope.len();

        let mut stmts = Vec::new();

        for stmt in ast_stmts {
            match self.lower_stmt(ast, *stmt) {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => self.errors.push(err),
            }
        }

        self.scope.truncate(scope_checkpoint);

        stmts
    }

    fn lower_expr(&mut self, ast: &Ast, expr: Id<ast::Expr>) -> ErrorOr<Id<Expr>> {
        let span = ast.exprs[expr].span;
        match ast.exprs[expr].kind {
            ast::ExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.lower_expr(ast, lhs)?;
                let rhs = self.lower_expr(ast, rhs)?;
                Ok(self.ir.exprs.push(Expr {
                    kind: ExprKind::Binary { lhs, op, rhs },
                    span,
                }))
            }
            ast::ExprKind::Block(ref stmts) => {
                let stmts = self.lower_block(ast, stmts);
                Ok(self.ir.exprs.push(Expr {
                    kind: ExprKind::Block(stmts),
                    span,
                }))
            }
            ast::ExprKind::Ident => {
                let ident = span.text(self.src);
                match self.scope.iter().rfind(|(str, _)| ident == *str) {
                    Some((_, id)) => Ok(self.ir.exprs.push(Expr {
                        kind: ExprKind::Ident(*id),
                        span,
                    })),
                    None => Err(Error {
                        kind: ErrorKind::UnknownSymbol,
                        span,
                    }),
                }
            }
            ast::ExprKind::Literal(literal) => Ok(self.ir.exprs.push(Expr {
                kind: ExprKind::Literal(literal),
                span,
            })),
            ast::ExprKind::Paren(expr) => self.lower_expr(ast, expr),
            ast::ExprKind::Tuple(ref exprs) => {
                unimplemented!("Tuples aren't lowered yet. ({:?})", exprs)
            }
        }
    }
}
