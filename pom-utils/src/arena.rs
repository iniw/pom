use std::{
    iter::{Enumerate, Map},
    num::NonZeroU32,
    slice::Iter,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Arena<T>(Vec<T>);

impl<T> Arena<T> {
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self(Vec::with_capacity(cap))
    }

    #[inline]
    pub fn push(&mut self, entry: T) -> Id<T> {
        self.0.push(entry);
        Id::new(self.0.len() as u32)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Default for Arena<T> {
    #[inline]
    fn default() -> Self {
        Self(Vec::default())
    }
}

impl<T> std::ops::Index<Id<T>> for Arena<T> {
    type Output = T;

    #[inline]
    fn index(&self, id: Id<T>) -> &Self::Output {
        unsafe { self.0.get_unchecked(id.0.get() as usize - 1) }
    }
}

impl<T> std::ops::IndexMut<Id<T>> for Arena<T> {
    #[inline]
    fn index_mut(&mut self, id: Id<T>) -> &mut Self::Output {
        unsafe { self.0.get_unchecked_mut(id.0.get() as usize - 1) }
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
            .map(|(idx, item)| (Id::new(idx as u32 + 1), item))
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
            .map(|(idx, item)| (Id::new(idx as u32 + 1), item))
    }
}

impl<T> From<Vec<T>> for Arena<T> {
    #[inline]
    fn from(value: Vec<T>) -> Self {
        Self(value)
    }
}

#[macro_export]
macro_rules! arena {
    [] => (
        $crate::arena::Arena::new()
    );
    [$elem:expr; $n:expr] => (
        $crate::arena::Arena::from(vec![$elem; $n])
    );
    [$($x:expr),+ $(,)?] => (
        $crate::arena::Arena::from(vec![$($x),+])
    );
}

#[must_use = "Ids cannot be created manually and so should not be ignored when given to you."]
#[repr(transparent)]
pub struct Id<T>(NonZeroU32, std::marker::PhantomData<T>);

impl<T> Id<T> {
    #[inline]
    fn new(idx: u32) -> Self {
        Self(
            unsafe { NonZeroU32::new_unchecked(idx) },
            std::marker::PhantomData,
        )
    }
}

impl<T> PartialEq for Id<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> Clone for Id<T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Id<T> {}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id<{}>({})", tynm::type_name::<T>(), self.0)
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

        assert_eq!(id1, Id::new(1));
        assert_eq!(id2, Id::new(2));
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
            &[(Id::new(1), 55), (Id::new(2), 47)]
        )
    }

    #[test]
    fn cmp() {
        let arena1 = arena![55, 47];
        let arena2 = arena![47, 55];
        assert_ne!(arena1, arena2);
        assert_eq!(arena1, arena1);
    }
}
