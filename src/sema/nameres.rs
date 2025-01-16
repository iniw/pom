#![expect(dead_code, unused_variables)]
pub mod ast;
mod env;
pub mod error;

use ast::{DeclarationInfo, Expr, Stmt};
use error::Error;

use crate::{
    lex::span::Spanned,
    pool::{Handle, Pool},
    syn::ast::{self as syn},
};

pub struct NameResolution {
    result: NameResolutionResult,
}

pub struct NameResolutionResult {
    symbols: Pool<DeclarationInfo>,

    global_stmts: Vec<Handle<Spanned<Stmt>>>,

    stmts: Pool<Spanned<Stmt>>,
    exprs: Pool<Spanned<Expr>>,

    errors: Vec<Error>,
}

impl NameResolution {
    fn resolve_names(
        mut self,
        global_stmts: Vec<Handle<Spanned<syn::Stmt>>>,
        stmts: Pool<Spanned<syn::Stmt>>,
        exprs: Pool<Spanned<syn::Expr>>,
    ) -> NameResolutionResult {
        self.result.global_stmts.reserve(global_stmts.len());
        self.result.stmts.reserve(stmts.len());
        self.result.exprs.reserve(exprs.len());

        for stmt in global_stmts {
            match self.resolve_statement(&stmts, &exprs, stmt) {
                Ok(stmt) => self.result.global_stmts.push(stmt),
                Err(error) => self.result.errors.push(error),
            }
        }

        todo!()
    }

    fn resolve_statement(
        &mut self,
        stmts: &Pool<Spanned<syn::Stmt>>,
        exprs: &Pool<Spanned<syn::Expr>>,
        stmt: Handle<Spanned<syn::Stmt>>,
    ) -> Result<Handle<Spanned<Stmt>>, Error> {
        let Spanned(stmt, span) = &stmts[stmt];
        match stmt {
            syn::Stmt::Declaration { identifier, info } => {
                todo!()
            }
            syn::Stmt::Expr(expr) => {
                let expr = self.resolve_expression(stmts, exprs, *expr)?;
                Ok(self
                    .result
                    .stmts
                    .push(Spanned(ast::Stmt::Expr(expr), *span)))
            }
        }
    }

    fn resolve_declaration(
        &mut self,
        stmts: &Pool<Spanned<syn::Stmt>>,
        exprs: &Pool<Spanned<syn::Expr>>,
        identifier: &str,
        info: &syn::DeclarationInfo,
    ) -> Result<Handle<Spanned<Stmt>>, Error> {
        todo!()
    }

    fn resolve_expression(
        &mut self,
        stmts: &Pool<Spanned<syn::Stmt>>,
        exprs: &Pool<Spanned<syn::Expr>>,
        expr: Handle<Spanned<syn::Expr>>,
    ) -> Result<Handle<Spanned<Expr>>, Error> {
        todo!()
    }
}
