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
        if let Sym::Resolved(Type::Kind(Kind::Fn(ref sig))) = symbols[bind.sym] {
            for bind in sig.params.clone() {
                self.check_bind(stmts, exprs, symbols, bind);
            }

            bind.rhs
                .map(|rhs| self.check_expr(stmts, exprs, symbols, rhs));

            return;
        }

        let rhs = bind
            .rhs
            .map(|rhs| self.check_expr(stmts, exprs, symbols, rhs));

        match symbols[bind.sym] {
            Sym::Resolved(ref ty) => {
                if let Some(rhs) = rhs {
                    self.unify_types(ty, &rhs);
                };
            }
            Sym::Infer => {
                symbols[bind.sym] =
                    Sym::Resolved(rhs.expect("BUG: Can't infer type of symbol without RHS"))
            }
            Sym::Compute(expr) => {
                let ty = self.check_expr(stmts, exprs, symbols, expr);
                match (ty, rhs) {
                    (Type::Kind(kind), Some(Type::Data(data))) => match (kind, data) {
                        (Kind::Type(kind_ty), Data::Type(data_ty)) => {
                            if self.unify_type_ctors(&kind_ty, &data_ty) {
                                symbols[bind.sym] = Sym::Resolved(Type::Data(Data::Type(data_ty)));
                            }
                        }

                        (Kind::Fn(_), Data::Fn(_)) => todo!(),

                        (kind, data) => self.errors.push(Error {
                            kind: ErrorKind::MismatchedDataBind {
                                wanted: kind,
                                got: data,
                            },
                        }),
                    },

                    (Type::Kind(kind), None) => {
                        symbols[bind.sym] = Sym::Resolved(match kind {
                            Kind::Fn(sig) => Type::Data(Data::Fn(sig)),
                            Kind::Type(ty) => Type::Data(Data::Type(ty)),
                        })
                    }

                    (Type::Kind(kind), Some(other)) => self.errors.push(Error {
                        kind: ErrorKind::MismatchedBind {
                            wanted: kind,
                            got: other,
                        },
                    }),

                    (ty, _) => self.errors.push(Error {
                        kind: ErrorKind::InvalidSymbolType { ty },
                    }),
                }
            }
            // This essentially produces an alias, is that right?
            Sym::NewType => {
                let Some(rhs) = rhs else {
                    return;
                };

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
                ref sym => {
                    panic!("BUG: Symbol should've already been resolved - {sym:#?} - {expr:#?}")
                }
            },
            ExprKind::Literal(literal) => match literal {
                ast::Literal::Bool(_) => Type::Data(Data::Type(TypeCtor::Bool)),
                ast::Literal::Int(_) => Type::Data(Data::Type(TypeCtor::I32)),
                ast::Literal::Float(_) => Type::Data(Data::Type(TypeCtor::F32)),
            },
            ref expr => todo!("{expr:#?}"),
        }
    }

    fn unify_types(&mut self, a: &Type, b: &Type) -> bool {
        let eq = a == b;
        if !eq {
            eprintln!("Failed to unify '{a:#?}' and '{b:#?}'");
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
            eprintln!("Failed to unify '{a:#?}' and '{b:#?}'");
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
