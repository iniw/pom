use std::{iter::Peekable, str::CharIndices};

#[derive(Debug)]
pub struct Lexer<'src> {
    source_code: &'src str,
    source: Peekable<CharIndices<'src>>,
    current_lexeme_start: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        Lexer {
            source_code: source,
            source: source.char_indices().peekable(),
            current_lexeme_start: 0,
        }
    }
    pub fn lex(&mut self) -> (Vec<Spanned<Token>>, Vec<LexingError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some((offset, character)) = self.source.next() {
            self.current_lexeme_start = offset;
            match self.interpret_character(character) {
                Ok(Some(token)) => {
                    tokens.push(Spanned::<Token> {
                        data: token,
                        span: self.current_lexeme_span(),
                    });
                }
                Ok(None) => {}
                Err(e) => {
                    errors.push(e);
                }
            }
        }

        (tokens, errors)
    }

    fn interpret_character(&mut self, character: char) -> Result<Option<Token<'src>>, LexingError> {
        match character {
            // Ampersands
            '&' if self.next_if_eq('&') => Ok(Some(Token::AmpersandAmpersand)),
            '&' => Ok(Some(Token::Ampersand)),

            // Braces
            '{' => Ok(Some(Token::LeftBrace)),
            '}' => Ok(Some(Token::RightBrace)),

            // Colons
            ':' => Ok(Some(Token::Colon)),
            ';' => Ok(Some(Token::Semicolon)),

            // Equals
            '=' if self.next_if_eq('=') => Ok(Some(Token::EqualEqual)),
            '=' => Ok(Some(Token::Equal)),

            // Keywords and symbols
            'a'..='z' | 'A'..='Z' | '_' => {
                match self.extend_current_lexeme_while(
                    |c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
                ) {
                    "fn" => Ok(Some(Token::Fn)),
                    "type" => Ok(Some(Token::Type)),
                    "return" => Ok(Some(Token::Return)),
                    symbol => Ok(Some(Token::Symbol(symbol))),
                }
            }

            // Numbers
            '0'..='9' => Ok(Some(Token::Number(
                self.extend_current_lexeme_while(char::is_ascii_digit),
            ))),

            // Others
            '!' => Ok(Some(Token::Bang)),
            '.' => Ok(Some(Token::Dot)),

            // Pipes
            '|' if self.next_if_eq('|') => Ok(Some(Token::PipePipe)),
            '|' => Ok(Some(Token::Pipe)),

            // Whitespace
            // FIXME: Throw `UnterminatedNumericLiteral` when reaching whitespace after lexing a
            // number followed by a dot.
            ' ' | '\t' | '\n' | '\r' | '\x0C' => Ok(None),

            _ => Err(LexingError::UnknownToken(self.current_lexeme_span())),
        }
    }

    fn extend_current_lexeme_while(&mut self, f: impl Fn(&char) -> bool) -> &'src str {
        while self.source.next_if(|(_, next)| f(next)).is_some() {}
        self.current_lexeme()
    }

    fn current_lexeme_end(&mut self) -> usize {
        if let Some((end, _)) = self.source.peek() {
            *end
        } else {
            self.source_code.len()
        }
    }

    fn current_lexeme_span(&mut self) -> Span {
        Span {
            start: self.current_lexeme_start,
            end: self.current_lexeme_end(),
        }
    }

    fn current_lexeme(&mut self) -> &'src str {
        &self.source_code[self.current_lexeme_start..self.current_lexeme_end()]
    }

    #[expect(dead_code)]
    fn next_if(&mut self, f: impl Fn(&char) -> bool) -> bool {
        self.source.next_if(|(_, next)| f(next)).is_some()
    }

    fn next_if_eq(&mut self, c: char) -> bool {
        self.source.next_if(|(_, next)| *next == c).is_some()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Token<'src> {
    // Ampersands
    Ampersand,
    AmpersandAmpersand,

    // Braces
    LeftBrace,
    RightBrace,

    // Colons
    Colon,
    Semicolon,

    // Equals
    Equal,
    EqualEqual,

    // Keywords and symbols
    Fn,
    Return,
    Symbol(&'src str),
    Type,

    // Numbers
    Number(&'src str),

    // Others
    Bang,
    Dot,

    // Pipes
    Pipe,
    PipePipe,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    fn line(&self, source_code: &str) -> usize {
        source_code[..=self.start].lines().count()
    }

    fn column(&self, source_code: &str) -> usize {
        if let Some(line) = source_code[..self.start].rfind("\n") {
            self.start - line
        } else {
            self.start + 1
        }
    }

    fn lexeme<'src>(&self, source_code: &'src str) -> &'src str {
        &source_code[self.start..self.end]
    }

    pub fn render(&self, source_code: &str) -> String {
        format!(
            "'{}' @ {}:{}",
            self.lexeme(source_code),
            self.line(source_code),
            self.column(source_code)
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Spanned<T> {
    data: T,
    span: Span,
}

impl<T> Spanned<T> {
    pub fn render(&self, source_code: &str) -> String
    where
        T: std::fmt::Debug,
    {
        format!("{:?} -> {}", self.data, self.span.render(source_code))
    }
}

#[derive(Debug, Clone)]
pub enum LexingError {
    UnknownToken(Span),
    UnterminatedNumericLiteral(Span),
    InvalidNumericLiteral(Span, <u64 as std::str::FromStr>::Err),
}

impl LexingError {
    pub fn render(&self, source_code: &str) -> String {
        match self {
            LexingError::UnknownToken(span) => {
                format!("Unknown token {}", span.render(source_code))
            }
            LexingError::UnterminatedNumericLiteral(span) => {
                format!("Unterminated numeric literal {}", span.render(source_code))
            }
            LexingError::InvalidNumericLiteral(span, err) => {
                format!(
                    "Invalid numeric literal {} ({})",
                    span.render(source_code),
                    err
                )
            }
        }
    }
}
