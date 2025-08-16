use pom_utils::arena::{Arena, Id};

use crate::{
    ast,
    ir::{
        Data, Ir, Kind, Sym, Type, TypeCtor,
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
        } = ir;

        for stmt in items {
            self.check_stmt(stmts, exprs, symbols, *stmt);
        }

        self.errors
    }

    fn check_stmt(
        &mut self,
        stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &mut Arena<Sym>,
        stmt: Id<Stmt>,
    ) {
        match stmts[stmt].kind {
            StmtKind::Bind(bind) => self.check_bind(stmts, exprs, symbols, bind),
            StmtKind::Expr(expr) => _ = self.check_expr(stmts, exprs, symbols, expr),
            _ => todo!(),
        }
    }

    fn check_bind(
        &mut self,
        stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &mut Arena<Sym>,
        bind: Bind,
    ) {
        let rhs = bind
            .rhs
            .expect("Uninitialized expressions aren't supported yet.");

        let rhs = self.check_expr(stmts, exprs, symbols, rhs);

        match symbols[bind.sym] {
            Sym::Resolved(ref ty) => _ = self.unify_types(ty, &rhs),
            Sym::Infer => symbols[bind.sym] = Sym::Resolved(rhs),
            Sym::Compute(expr) => {
                let ty = self.check_expr(stmts, exprs, symbols, expr);
                match (&ty, &rhs) {
                    (Type::Kind(kind), Type::Data(data)) => match (kind, data) {
                        (Kind::Type(kind_ty), Data::Type(data_ty)) => {
                            if self.unify_type_ctors(kind_ty, data_ty) {
                                symbols[bind.sym] = Sym::Resolved(rhs);
                            }
                        }

                        (Kind::Fn(_), Data::Fn(_)) => todo!(),

                        _ => self.errors.push(Error {
                            kind: ErrorKind::MismatchedTypes {
                                wanted: ty,
                                got: rhs,
                            },
                        }),
                    },

                    (_, _) => self.errors.push(Error {
                        kind: ErrorKind::InvalidSymbolKind { expr },
                    }),
                }
            }
            // This essentially produces an alias, is that right?
            Sym::NewType => {
                if matches!(rhs, Type::Kind(Kind::Type(_))) {
                    symbols[bind.sym] = Sym::Resolved(rhs);
                } else {
                    self.errors.push(Error {
                        kind: ErrorKind::InvalidNewTypeRhs { rhs },
                    });
                }
            }
        }
    }

    fn check_expr(
        &mut self,
        _stmts: &mut Arena<Stmt>,
        exprs: &mut Arena<Expr>,
        symbols: &Arena<Sym>,
        expr: Id<Expr>,
    ) -> Type {
        match exprs[expr].kind {
            ExprKind::Ident(sym) => match symbols[sym] {
                Sym::Resolved(ref ty) => ty.clone(),
                ref sym => panic!("BUG: Symbol should've already been resolved - {sym:#?}"),
            },
            ExprKind::Literal(literal) => match literal {
                ast::Literal::Bool(_) => Type::Data(Data::Type(TypeCtor::Bool)),
                ast::Literal::Int(_) => Type::Data(Data::Type(TypeCtor::I32)),
                ast::Literal::Float(_) => Type::Data(Data::Type(TypeCtor::F32)),
            },
            _ => todo!(),
        }
    }

    fn unify_types(&mut self, a: &Type, b: &Type) -> bool {
        let eq = a == b;
        if !eq {
            self.errors.push(Error {
                kind: ErrorKind::MismatchedTypes {
                    wanted: a.clone(),
                    got: b.clone(),
                },
            });
        }
        eq
    }

    fn unify_type_ctors(&mut self, a: &TypeCtor, b: &TypeCtor) -> bool {
        let eq = a == b;
        if !eq {
            self.errors.push(Error {
                kind: ErrorKind::MismatchedTypeCtors {
                    wanted: a.clone(),
                    got: b.clone(),
                },
            });
        }
        eq
    }
}
