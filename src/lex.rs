use std::{iter::Peekable, str::CharIndices};

#[derive(Debug)]
pub struct Lexer<'src> {
    source_data: &'src str,
    source: Peekable<CharIndices<'src>>,
    line: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source_data: source,
            source: source.char_indices().peekable(),
            line: 0,
        }
    }

    pub fn lex(&mut self) -> (Vec<ContextualizedToken<'src>>, Vec<LexingError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some((start, character)) = self.source.next() {
            match self.interpret_token(character, start) {
                Ok(Some(token)) => {
                    let lexeme = self.current_lexeme_starting_from(start);
                    tokens.push(ContextualizedToken {
                        token,
                        lexeme,
                        span: Span { line: self.line },
                    });
                }
                Ok(None) => {}
                Err(e) => {
                    errors.push(e);
                }
            }
        }

        tokens.push(ContextualizedToken {
            token: Token::EndOfFile,
            lexeme: "",
            span: Span { line: self.line },
        });

        (tokens, errors)
    }

    fn interpret_token(
        &mut self,
        character: char,
        start: usize,
    ) -> Result<Option<Token<'src>>, LexingError> {
        match character {
            '{' => Ok(Some(Token::LeftBrace)),
            '}' => Ok(Some(Token::RightBrace)),

            ':' => Ok(Some(Token::Colon)),
            ';' => Ok(Some(Token::Semicolon)),

            '=' => Ok(Some(Token::Equal)),

            '\n' => {
                self.line += 1;
                Ok(None)
            }

            c if c.is_whitespace() => Ok(None),

            c if Self::is_valid_for_identifier_or_keyword(c) => {
                match self.consume_while(start, Self::is_valid_for_identifier_or_keyword) {
                    "fn" => Ok(Some(Token::Fn)),
                    "type" => Ok(Some(Token::Type)),
                    identifier => Ok(Some(Token::Identifier(identifier))),
                }
            }

            _ => Err(LexingError::UnknownToken(character)),
        }
    }

    fn consume_while(&mut self, start: usize, f: impl Fn(char) -> bool) -> &'src str {
        while self.source.next_if(|(_, next)| f(*next)).is_some() {}
        self.current_lexeme_starting_from(start)
    }

    fn current_lexeme_starting_from(&mut self, start: usize) -> &'src str {
        if let Some((end, _)) = self.source.peek() {
            &self.source_data[start..*end]
        } else {
            &self.source_data[start..]
        }
    }

    fn is_valid_for_identifier_or_keyword(c: char) -> bool {
        c.is_ascii_alphabetic() || c.is_ascii_alphanumeric() || c == '_'
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Token<'src> {
    Colon,
    Equal,
    Identifier(&'src str),
    Fn,
    LeftBrace,
    RightBrace,
    Return,
    Semicolon,
    Type,

    EndOfFile,
}

#[derive(Debug, Copy, Clone)]
pub struct ContextualizedToken<'src> {
    lexeme: &'src str,
    token: Token<'src>,
    span: Span,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    line: u32,
}

#[derive(Debug, Copy, Clone)]
pub enum LexingError {
    UnknownToken(char),
}
