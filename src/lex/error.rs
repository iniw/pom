use super::span::Spanned;

#[derive(Debug, Copy, Clone)]
pub struct Error(pub Spanned<ErrorKind>);

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    UnknownToken,
    UnterminatedNumericLiteral,
}

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        let Spanned(kind, span) = &self.0;
        match kind {
            ErrorKind::UnknownToken => {
                format!("Unknown token {}", span.render(source_code))
            }
            ErrorKind::UnterminatedNumericLiteral => {
                format!("Unterminated numeric literal {}", span.render(source_code))
            }
        }
    }
}
