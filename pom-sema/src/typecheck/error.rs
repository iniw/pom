use pom_utils::arena::Id;

use crate::ir::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Error {
    pub kind: ErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    MismatchedTypes { wanted: Id<Type>, got: Id<Type> },
}

pub type Errors = Vec<Error>;
