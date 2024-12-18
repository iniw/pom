#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn line(&self, source_code: &str) -> usize {
        source_code[..=self.start as usize].lines().count()
    }

    pub fn column(&self, source_code: &str) -> u32 {
        if let Some(line) = source_code[..self.start as usize].rfind("\n") {
            self.start - (line as u32)
        } else {
            self.start + 1
        }
    }

    pub fn render(&self, source_code: &str) -> String {
        if *self == Self::eof_for(source_code) {
            "<EOF>".to_owned()
        } else {
            format!(
                "'{}' @ {}:{}",
                self.lexeme(source_code).trim(),
                self.line(source_code),
                self.column(source_code)
            )
        }
    }

    pub(super) fn eof_for(source_code: &str) -> Self {
        Self {
            start: source_code.len() as u32,
            end: source_code.len() as u32,
        }
    }

    fn lexeme<'src>(&self, source_code: &'src str) -> &'src str {
        &source_code[self.start as usize..self.end as usize]
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub fn render(&self, source_code: &str) -> String
    where
        T: std::fmt::Debug,
    {
        format!("{:?} -> {}", self.0, self.1.render(source_code))
    }

    pub fn data(&self) -> &T {
        &self.0
    }

    pub fn span(&self) -> Span {
        self.1
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
