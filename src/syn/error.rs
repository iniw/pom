use crate::lex::span::{Span, Spanned};

#[derive(Debug, Copy, Clone)]
pub struct Error(pub Spanned<ErrorKind>);

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        let Spanned(kind, span) = &self.0;
        match kind {
            ErrorKind::InvalidNumericLiteral => {
                format!("Invalid numeric literal {}.", span.render(source_code))
            }
            ErrorKind::ExpectedExpression => {
                format!(
                    "Expected expression instead of {}.",
                    span.render(source_code)
                )
            }
            ErrorKind::ExpectedSemicolon => {
                format!(
                    "Expected semicolon to finalize statement, got {} instead.",
                    span.render(source_code)
                )
            }
            ErrorKind::UnclosedBraces => {
                format!(
                    "Unclosed braces beginning @ {}:{}",
                    span.line(source_code),
                    span.column(source_code),
                )
            }
            ErrorKind::UnclosedFunctionParenthesis(opening) => {
                format!(
                    "Unclosed parenthesis for function call beginning @ {}:{}, got {} instead.",
                    opening.line(source_code),
                    opening.column(source_code),
                    span.render(source_code)
                )
            }
            ErrorKind::UnclosedExpressionParenthesis(opening) => {
                format!(
                    "Unclosed parenthesis for parenthesized expression beginning @ {}:{}, got {} instead.",
                    opening.line(source_code),
                    opening.column(source_code),
                    span.render(source_code)
                )
            }
            ErrorKind::UnexpectedKind => {
                format!("Unexpected symbol kind {}.", span.render(source_code))
            }
            ErrorKind::UnexpectedToken => {
                format!("Unexpected token {}.", span.render(source_code))
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumericLiteral,

    ExpectedExpression,
    ExpectedSemicolon,

    UnclosedBraces,
    UnclosedFunctionParenthesis(Span),
    UnclosedExpressionParenthesis(Span),

    UnexpectedKind,
    UnexpectedToken,
}
