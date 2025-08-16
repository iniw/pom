use crate::ir::{Data, Kind, Type, TypeCtor};

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidSymbolType { ty: Type },

    InvalidNewTypeRhs { rhs: Type },

    MismatchedBind { wanted: Kind, got: Type },
    MismatchedDataBind { wanted: Kind, got: Data },

    MismatchedTypes { wanted: Type, got: Type },
    MismatchedTypeCtors { wanted: TypeCtor, got: TypeCtor },
}

pub type Errors = Vec<Error>;
