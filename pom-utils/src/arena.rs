use std::{
    iter::{Enumerate, Map},
    slice::Iter,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arena<T>(Vec<T>);

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    #[inline]
    pub fn push(&mut self, entry: T) -> Id<T> {
        let entry_id = self.0.len() as u32;
        self.0.push(entry);
        Id::new(entry_id)
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> std::ops::Index<Id<T>> for Arena<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, id: Id<T>) -> &Self::Output {
        unsafe { self.0.get_unchecked(id.0 as usize) }
    }
}

impl<T> std::ops::IndexMut<Id<T>> for Arena<T> {
    #[inline(always)]
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        unsafe { self.0.get_unchecked_mut(id.0 as usize) }
    }
}

impl<T> IntoIterator for Arena<T> {
    type Item = (Id<T>, T);
    type IntoIter =
        Map<Enumerate<<Vec<T> as IntoIterator>::IntoIter>, fn((usize, T)) -> Self::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0
            .into_iter()
            .enumerate()
            .map(|(idx, item)| (Id::new(idx as u32), item))
    }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type Item = (Id<T>, &'a T);
    type IntoIter = Map<Enumerate<Iter<'a, T>>, fn((usize, &'a T)) -> Self::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter()
            .enumerate()
            .map(|(idx, item)| (Id::new(idx as u32), item))
    }
}

impl<T> From<Vec<T>> for Arena<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

#[macro_export]
macro_rules! arena {
    () => (
        $crate::arena::Arena::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::arena::Arena::from(vec![$elem; $n])
    );
    ($($x:expr),+ $(,)?) => (
        $crate::arena::Arena::from(vec![$($x),+])
    );
}

#[must_use = "Ids cannot be created manually and so should not be ignored when given to you."]
#[repr(transparent)]
pub struct Id<T>(u32, std::marker::PhantomData<T>);

impl<T> Id<T> {
    #[inline(always)]
    fn new(idx: u32) -> Self {
        Self(idx, std::marker::PhantomData)
    }
}

impl<T> PartialEq for Id<T> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Clone for Id<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id({})", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push() {
        let mut arena = Arena::new();
        let id1 = arena.push(55);
        let id2 = arena.push(47);

        assert_eq!(id1, Id::new(0));
        assert_eq!(id2, Id::new(1));
    }

    #[test]
    fn index() {
        let mut arena = Arena::new();
        let id1 = arena.push(55);
        let id2 = arena.push(47);

        assert_eq!(arena[id1], 55);
        assert_eq!(arena[id2], 47);
    }

    #[test]
    fn iter() {
        let arena = arena![55, 47];
        assert_eq!(
            arena.into_iter().collect::<Vec<_>>(),
            &[(Id::new(0), 55), (Id::new(1), 47)]
        )
    }
}
