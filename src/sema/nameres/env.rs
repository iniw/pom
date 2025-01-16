#[derive(Debug)]
pub struct Env<'lex> {
    envs: Vec<Entry<'lex>>,
    envs_restore: Vec<usize>,
}

impl<'lex> Env<'lex> {
    pub fn with_global_env() -> Self {
        Self {
            envs: vec![],
            envs_restore: vec![],
        }
    }

    #[inline(always)]
    pub fn push(&mut self) {
        self.envs_restore.push(self.envs.len())
    }

    #[inline(always)]
    pub fn pop(&mut self) {
        let len = self
            .envs_restore
            .pop()
            .expect("ICE: An environment should've been pushed.");

        self.envs.truncate(len);
    }

    pub fn find_symbol(&mut self, identifier: &'lex str) -> Option<&Symbol> {
        self.envs
            .iter()
            .rfind(|entry| entry.identifier == identifier)
            .map(|entry| &entry.symbol)
    }

    pub fn declare_symbol(&mut self, identifier: &'lex str, symbol: Symbol) -> Option<()> {
        // TODO: Disallow declaring symbols with the same name as the current function, if we are inside one.
        // TODO: Handle shadowing.
        self.envs.push(Entry { identifier, symbol });
        Some(())
    }

    #[inline(always)]
    fn active(&mut self) -> &[Entry] {
        let beginning = self
            .envs_restore
            .last()
            .expect("ICE: There should be at least one active env.");

        &self.envs[*beginning..]
    }
}

#[derive(Debug)]
struct Entry<'lex> {
    identifier: &'lex str,
    symbol: Symbol,
}

#[derive(Debug)]
pub struct Symbol;
