#[derive(Debug, Clone)]
pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn new() -> Self {
        // FIXME: Use a hint from the source code somehow?
        Self(Vec::new())
    }

    #[inline]
    pub fn add(&mut self, entry: T) -> Handle<T> {
        self.0.push(entry);
        Handle::new((self.0.len() - 1) as u32)
    }

    #[inline]
    pub fn get(&self, handle: Handle<T>) -> &T {
        unsafe { self.0.get_unchecked(handle.idx as usize) }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn handles(&self) -> PoolHandleIter<T> {
        PoolHandleIter::new(self)
    }
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IntoIterator for Pool<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Pool<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

pub struct PoolHandleIter<'a, T> {
    inner: std::ops::Range<u32>,
    _phantom: std::marker::PhantomData<&'a Pool<T>>,
}

impl<'a, T> PoolHandleIter<'a, T> {
    pub fn new(pool: &'a Pool<T>) -> Self {
        Self {
            inner: 0..pool.0.len() as u32,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T> Iterator for PoolHandleIter<'a, T> {
    type Item = Handle<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Handle::new)
    }
}

#[derive(PartialEq, Eq)]
pub struct Handle<T> {
    idx: u32,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

impl<T> Handle<T> {
    #[inline]
    fn new(idx: u32) -> Self {
        Self {
            idx,
            _phantom: std::marker::PhantomData {},
        }
    }
}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Handle({})", self.idx)
    }
}
