use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct Pool<T>(Vec<T>);

impl<T> Pool<T> {
    pub fn new() -> Self {
        // FIXME: Use a hint from the source code somehow?
        Self(Vec::new())
    }

    #[inline(always)]
    pub fn push(&mut self, entry: T) -> Handle<T> {
        let entry_handle = self.next_handle();
        self.0.push(entry);
        Handle::new(entry_handle)
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline(always)]
    pub fn next_handle(&self) -> u32 {
        self.0.len() as u32
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

impl<T> Index<Handle<T>> for Pool<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, handle: Handle<T>) -> &Self::Output {
        unsafe { self.0.get_unchecked(handle.idx as usize) }
    }
}

impl<T> IndexMut<Handle<T>> for Pool<T> {
    #[inline(always)]
    fn index_mut(&mut self, handle: Handle<T>) -> &mut Self::Output {
        unsafe { self.0.get_unchecked_mut(handle.idx as usize) }
    }
}

impl<T> IntoIterator for Pool<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Pool<T> {
    type Item = &'a T;
    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

pub struct PoolHandleIter<'a, T> {
    inner: std::ops::Range<u32>,
    _phantom: std::marker::PhantomData<&'a Pool<T>>,
}

impl<'a, T> PoolHandleIter<'a, T> {
    #[inline(always)]
    pub fn new(pool: &'a Pool<T>) -> Self {
        Self {
            inner: 0..pool.0.len() as u32,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T> Iterator for PoolHandleIter<'a, T> {
    type Item = Handle<T>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Handle::new)
    }
}

#[derive(PartialEq, Eq)]
pub struct Handle<T> {
    idx: u32,
    _phantom: std::marker::PhantomData<T>,
}

impl<T> Handle<T> {
    #[inline(always)]
    fn new(idx: u32) -> Self {
        Self {
            idx,
            _phantom: std::marker::PhantomData {},
        }
    }

    /// # Safety
    /// `idx` must be a valid index into a `Pool<T>`
    #[inline(always)]
    pub unsafe fn from_raw(idx: u32) -> Self {
        Self {
            idx,
            _phantom: std::marker::PhantomData {},
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
        let mut type_name = std::any::type_name::<T>().to_owned();
        type_name.drain(..type_name.rfind(':').map(|i| i + 1).unwrap_or(0));
        write!(f, "Handle<{}>({})", type_name, self.idx)
    }
}
