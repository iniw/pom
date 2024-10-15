use std::{iter::Peekable, str::CharIndices};

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

    pub fn lex(mut self) -> (Vec<Spanned<Token<'src>>>, Vec<Error>) {
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

    fn interpret_character(&mut self, character: char) -> Result<Option<Token<'src>>, Error> {
        match character {
            // Ampersands
            '&' if self.next_if_eq('&') => Ok(Some(Token::AmpersandAmpersand)),
            '&' => Ok(Some(Token::Ampersand)),

            // Arithmetic
            '+' => Ok(Some(Token::Add)),
            '-' => Ok(Some(Token::Sub)),
            '/' => Ok(Some(Token::Div)),
            '*' => Ok(Some(Token::Mul)),

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
            '0'..='9' => {
                self.extend_current_lexeme_while(char::is_ascii_digit);
                if self.next_if_eq('.') {
                    if !self.next_if(char::is_ascii_digit) {
                        return Err(Error {
                            kind: ErrorKind::UnterminatedNumericLiteral,
                            span: self.current_lexeme_span(),
                        });
                    }
                    self.extend_current_lexeme_while(char::is_ascii_digit);
                }
                Ok(Some(Token::Number(self.current_lexeme())))
            }

            // Others
            '!' => Ok(Some(Token::Bang)),
            '.' => Ok(Some(Token::Dot)),

            // Parenthesis
            '(' => Ok(Some(Token::LeftParenthesis)),
            ')' => Ok(Some(Token::RightParenthesis)),

            // Pipes
            '|' if self.next_if_eq('|') => Ok(Some(Token::PipePipe)),
            '|' => Ok(Some(Token::Pipe)),

            // Whitespace
            ' ' | '\t' | '\n' | '\r' | '\x0C' => Ok(None),

            _ => Err(Error {
                kind: ErrorKind::UnknownToken,
                span: self.current_lexeme_span(),
            }),
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
            start: self.current_lexeme_start as u32,
            end: self.current_lexeme_end() as u32,
        }
    }

    fn current_lexeme(&mut self) -> &'src str {
        &self.source_code[self.current_lexeme_start..self.current_lexeme_end()]
    }

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

    // Arithmetic
    Add,
    Sub,
    Div,
    Mul,

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

    // Parenthesis
    LeftParenthesis,
    RightParenthesis,

    // Pipes
    Pipe,
    PipePipe,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    fn line(&self, source_code: &str) -> u32 {
        source_code[..=self.start as usize].lines().count() as u32
    }

    fn column(&self, source_code: &str) -> u32 {
        if let Some(line) = source_code[..self.start as usize].rfind("\n") {
            self.start - (line as u32)
        } else {
            self.start + 1
        }
    }

    fn lexeme<'src>(&self, source_code: &'src str) -> &'src str {
        &source_code[self.start as usize..self.end as usize]
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
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn render(&self, source_code: &str) -> String
    where
        T: std::fmt::Debug,
    {
        format!("{:?} -> {}", self.data, self.span.render(source_code))
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

#[derive(Debug, Copy, Clone)]
pub enum ErrorKind {
    UnknownToken,
    UnterminatedNumericLiteral,
}

impl Error {
    pub fn render(&self, source_code: &str) -> String {
        match self.kind {
            ErrorKind::UnknownToken => {
                format!("Unknown token {}", self.span.render(source_code))
            }
            ErrorKind::UnterminatedNumericLiteral => {
                format!(
                    "Unterminated numeric literal {}",
                    self.span.render(source_code)
                )
            }
        }
    }
}
