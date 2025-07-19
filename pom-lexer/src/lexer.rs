use logos::Logos;
use pom_utils::span::Span;

use crate::{
    Errors, Tokens,
    error::Error,
    token::{Token, TokenKind},
};

pub struct Lexer<'src> {
    tokens: Tokens,
    errors: Errors,

    lexer: logos::SpannedIter<'src, TokenKind>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            tokens: Tokens::new(),
            errors: Errors::new(),

            lexer: TokenKind::lexer(src).spanned(),
        }
    }

    pub fn lex(mut self) -> (Tokens, Errors) {
        for (token, span) in self.lexer {
            let span = Span {
                start: span.start as u32,
                end: span.end as u32,
            };
            match token {
                Ok(token) => self.tokens.push(Token { kind: token, span }),
                Err(err) => {
                    let err = self.errors.push(Error { kind: err, span });
                    self.tokens.push(Token {
                        kind: TokenKind::Invalid(err),
                        span,
                    })
                }
            }
        }

        self.tokens.push(Token::eof());

        (self.tokens, self.errors)
    }
}
