#[derive(Debug, Clone)]
pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn new() -> Self {
        // FIXME: Use a hint from the source code somehow?
        Self(Vec::with_capacity(1000))
    }

    pub fn add(&mut self, entry: T) -> Handle<T> {
        self.0.push(entry);
        Handle::new((self.0.len() - 1) as u32)
    }

    pub fn get(&self, handle: Handle<T>) -> &T {
        &self.0[handle.idx as usize]
    }

    pub fn next_handle(&self) -> Handle<T> {
        Handle::new(self.0.len() as u32)
    }
}

impl<'a, T> IntoIterator for &'a Pool<T> {
    type Item = Handle<T>;
    type IntoIter = PoolHandleIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        PoolHandleIter {
            pool: self,
            index: 0,
        }
    }
}

pub struct PoolHandleIter<'a, T> {
    pool: &'a Pool<T>,
    index: usize,
}

impl<'a, T> Iterator for PoolHandleIter<'a, T> {
    type Item = Handle<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.pool.0.len() {
            let handle = Handle::new(self.index as u32);
            self.index += 1;
            Some(handle)
        } else {
            None
        }
    }
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Handle<T> {
    idx: u32,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> Handle<T> {
    fn new(idx: u32) -> Self {
        Self {
            idx,
            _phantom: std::marker::PhantomData {},
        }
    }
}
