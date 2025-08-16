use pom_utils::arena::Id;

use crate::ir::{Type, TypeCtor, expr::Expr};

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidSymbolKind { expr: Id<Expr> },

    InvalidNewTypeRhs { rhs: Type },

    MismatchedTypes { wanted: Type, got: Type },
    MismatchedTypeCtors { wanted: TypeCtor, got: TypeCtor },
}

pub type Errors = Vec<Error>;
