use crate::ir::{Data, Kind, Type, TypeCtor};

/// A typecheck error (no span — source location comes from the expression being checked).
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
}

/// The kind of typecheck error.
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    /// Symbol resolved to an invalid type.
    InvalidSymbolType { ty: Type },

    /// Right-hand side of a `NewType` binding has an invalid type.
    InvalidNewTypeRhs { rhs: Type },

    /// Binding type annotation (kind-level) doesn't match the initializer's type.
    MismatchedBind { wanted: Kind, got: Type },
    /// Binding type annotation (kind-level) doesn't match the initializer's data-level type.
    MismatchedDataBind { wanted: Kind, got: Data },

    /// Two types that should match don't.
    MismatchedTypes { wanted: Type, got: Type },
    /// Two concrete type constructors that should match don't.
    MismatchedTypeCtors { wanted: TypeCtor, got: TypeCtor },
}

/// Alias for a collection of typecheck errors.
pub type Errors = Vec<Error>;
