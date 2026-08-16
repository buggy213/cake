use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops::{Index, IndexMut}};

/// Idx is implemented by newtyped index types
trait Idx : Clone + Copy + Hash + PartialEq + Eq + PartialOrd + Ord + Debug + Into<usize> {}

struct IndexVec<I: Idx, T> {
    inner: Vec<T>,

    // needed to satisfy the compiler, which insists the index type 
    // shows up in one of the fields
    _unused: PhantomData<I>,   
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    pub fn push_mut(&mut self, value: T) -> &mut T {
        self.inner.push(value);
        self.inner.last_mut().unwrap()
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.into()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.into()]
    }
}

struct IndexSlice<I: Idx, T: ?Sized> {
    _unused: PhantomData<I>,
    inner: T,
}

impl<I: Idx, T> Index<I> for IndexSlice<I, [T]> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.into()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexSlice<I, [T]> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.into()]
    }
}
