#[derive(Debug, Clone)]
pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    #[inline(always)]
    pub fn push(&mut self, entry: T) -> Handle<T> {
        let entry_handle = self.0.len() as u32;
        self.0.push(entry);
        Handle::new(entry_handle)
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> std::ops::Index<Handle<T>> for Pool<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, handle: Handle<T>) -> &Self::Output {
        unsafe { self.0.get_unchecked(handle.idx as usize) }
    }
}

impl<T> std::ops::IndexMut<Handle<T>> for Pool<T> {
    #[inline(always)]
    fn index_mut(&mut self, handle: Handle<T>) -> &mut Self::Output {
        unsafe { self.0.get_unchecked_mut(handle.idx as usize) }
    }
}

impl<T> IntoIterator for Pool<T> {
    type Item = (Handle<T>, T);
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .into_iter()
            .enumerate()
            .map(|(idx, item)| (Handle::new(idx as u32), item))
    }
}

impl<'pool, T> IntoIterator for &'pool Pool<T> {
    type Item = (Handle<T>, &'pool T);
    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter()
            .enumerate()
            .map(|(idx, item)| (Handle::new(idx as u32), item))
    }
}

#[derive(PartialEq, Eq)]
pub struct Handle<T> {
    idx: u32,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Handle<T> {
    #[inline(always)]
    fn new(idx: u32) -> Self {
        Self {
            idx,
            phantom: std::marker::PhantomData {},
        }
    }
}

impl<T> Clone for Handle<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Handle<T> {}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Handle({})", self.idx)
    }
}
