#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn line(&self, src: &str) -> u32 {
        src[..=self.start as usize].lines().count() as u32
    }

    pub fn column(&self, src: &str) -> u32 {
        if let Some(line) = src[..self.start as usize].rfind("\n") {
            self.start - (line as u32)
        } else {
            self.start + 1
        }
    }

    pub fn render(&self, src: &str) -> String {
        if self.is_eof() {
            "<EOF>".to_owned()
        } else {
            format!(
                "'{}' @ {}:{}",
                self.text(src),
                self.line(src),
                self.column(src)
            )
        }
    }

    pub fn eof() -> Self {
        Self {
            start: u32::MAX,
            end: u32::MAX,
        }
    }

    pub fn is_eof(&self) -> bool {
        *self == Self::eof()
    }

    pub fn text<'src>(&self, src: &'src str) -> &'src str {
        &src[self.start as usize..self.end as usize]
    }
}
