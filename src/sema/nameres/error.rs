use crate::lex::span::Spanned;

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidMainDeclaration,
    InvalidShadowing,
    NotInsideFunction,
    UndeclaredSymbol,
    SymbolIsNotVariable,
}

#[derive(Debug, Copy, Clone)]
pub struct Error(pub Spanned<ErrorKind>);

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        let Spanned(kind, span) = &self.0;
        match kind {
            ErrorKind::InvalidMainDeclaration => {
                format!(
                    "The 'main' symbol in the outer scope must be a function instead of a {}.",
                    span.render(source_code)
                )
            }
            ErrorKind::InvalidShadowing => {
                format!(
                    "Shadowing of symbol with {} is not allowed at this scope.",
                    span.render(source_code)
                )
            }
            ErrorKind::NotInsideFunction => {
                format!(
                    "Operation {} is only valid inside of a function.",
                    span.render(source_code)
                )
            }
            ErrorKind::UndeclaredSymbol => {
                format!(
                    "Symbol {} is undeclared or not available at that scope.",
                    span.render(source_code)
                )
            }
            ErrorKind::SymbolIsNotVariable => {
                format!(
                    "Only variables are allowed in the place of {}.",
                    span.render(source_code)
                )
            }
        }
    }
}
