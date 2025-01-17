#[derive(Debug)]
pub struct Env<'lex> {
    symbols: Vec<Entry<'lex>>,
    checkpoints: Vec<usize>,
}

impl<'lex> Env<'lex> {
    pub fn with_global_env() -> Self {
        Self {
            symbols: vec![],
            checkpoints: vec![],
        }
    }

    #[inline(always)]
    pub fn push(&mut self) {
        self.checkpoints.push(self.symbols.len())
    }

    #[inline(always)]
    pub fn pop(&mut self) {
        let checkpoint = self
            .checkpoints
            .pop()
            .expect("ICE: An environment should've been pushed.");

        self.symbols.truncate(checkpoint);
    }

    pub fn find_symbol(&mut self, identifier: &'lex str) -> Option<&Symbol> {
        self.symbols
            .iter()
            .rfind(|entry| entry.identifier == identifier)
            .map(|entry| &entry.symbol)
    }

    pub fn declare_symbol(&mut self, identifier: &'lex str, symbol: Symbol) -> Option<()> {
        // TODO: Disallow declaring symbols with the same name as the current function, if we are inside one.
        // TODO: Handle shadowing.
        self.symbols.push(Entry {
            identifier,
            symbol,
            depth: self.checkpoints.last().copied().unwrap_or(0) as u32,
        });
        Some(())
    }
}

#[derive(Debug)]
struct Entry<'lex> {
    identifier: &'lex str,
    symbol: Symbol,
    depth: u32,
}

#[derive(Debug)]
pub struct Symbol;
