use std::{array::IntoIter, fmt::Debug, hash::Hash, marker::PhantomData, ops::{Deref, DerefMut, Index, IndexMut}};

/// Idx is implemented by newtyped index types
pub trait Idx : Clone + Copy + Hash + PartialEq + Eq + PartialOrd + Ord + Debug + Into<usize> {}

pub struct IndexVec<I: Idx, T> {
    inner: Vec<T>,

    // needed to satisfy the compiler, which insists the index type 
    // shows up in one of the fields
    _unused: PhantomData<I>,   
}

impl<I: Idx, T> IndexVec<I, T> {
    pub fn new() -> Self {
        Self { inner: Vec::new(), _unused: PhantomData }
    }

    // TODO: from_elem needs work

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.inner.iter_mut()
    }

    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    pub fn push_mut(&mut self, value: T) -> &mut T {
        self.inner.push(value);
        self.inner.last_mut().unwrap()
    }

    /// Helper function for the `index_vec!` macro
    pub fn from_vec(vec: Vec<T>) -> Self {
        Self { inner: vec, _unused: PhantomData }
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

/// IndexVec's `Debug` implementation passes through to the underlying vector
impl<I: Idx, T: Debug> Debug for IndexVec<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Vec<T> as Debug>::fmt(&self.inner, f)
    }
}

impl<I: Idx, T> AsRef<IndexSlice<I, [T]>> for IndexVec<I, T> {
    fn as_ref(&self) -> &IndexSlice<I, [T]> {
        IndexSlice::from_slice(&self.inner)
    }
}

impl<I: Idx, T> AsMut<IndexSlice<I, [T]>> for IndexVec<I, T> {
    fn as_mut(&mut self) -> &mut IndexSlice<I, [T]> {
        IndexSlice::from_slice_mut(&mut self.inner)
    }
}

impl<I: Idx, T> Deref for IndexVec<I, T> {
    type Target = IndexSlice<I, [T]>;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<I: Idx, T> DerefMut for IndexVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a IndexVec<I, T> {
    type Item = &'a T;

    type IntoIter = <&'a Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, I: Idx, T> IntoIterator for &'a mut IndexVec<I, T> {
    type Item = &'a mut T;

    type IntoIter = <&'a mut Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[macro_export]
macro_rules! index_vec {
    () => (
        $crate::IndexVec::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::IndexVec::from_elem($elem, $n)
    );
    ($($x:expr),+ $(,)?) => (
        $crate::IndexVec::from_vec(vec![$($x),+])
    );
}

#[repr(transparent)]
pub struct IndexSlice<I: Idx, T: ?Sized> {
    _unused: PhantomData<fn(I)>,
    inner: T,
}

impl<I: Idx, T> IndexSlice<I, [T]> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.inner.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.inner.iter_mut()
    }

    fn from_slice(slice: &[T]) -> &Self {
        // SAFETY: IndexSlice is repr(transparent) over [T], and PhantomData is a ZST,
        // so &[T] and &IndexSlice<I, [T]> have the same layout
        unsafe { &*(slice as *const [T] as *const Self) }
    }

    fn from_slice_mut(slice_mut: &mut [T]) -> &mut Self {
        // SAFETY: IndexSlice is repr(transparent) over [T], and PhantomData is a ZST,
        // so &mut [T] and &mut IndexSlice<I, [T]> have the same layout
        unsafe { &mut *(slice_mut as *mut [T] as *mut Self) }
    }
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

impl<'a, I: Idx, T> IntoIterator for &'a IndexSlice<I, [T]> {
    type Item = &'a T;

    type IntoIter = <&'a [T] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
