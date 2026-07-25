//! String interning pool for use within the compiler
//! 
//! Uses `hashbrown::HashTable` and `rustc_hash::FxHasher` for fast lookup of strings

use std::{cell::RefCell, hash::{Hash, Hasher}, rc::Rc};

use hashbrown::HashTable;
use rustc_hash::FxHasher;

pub(crate) struct StringPoolImpl {
    backing_mem: Vec<u8>,
    hash_table: HashTable<StringPoolRef>,
    ends: Vec<u32>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct StringPoolRef(u32);
impl StringPoolRef {
    pub(crate) fn invalid() -> StringPoolRef {
        StringPoolRef(u32::MAX)
    }
}

impl StringPoolImpl {
    pub(crate) fn new() -> StringPoolImpl {
        StringPoolImpl { 
            backing_mem: Vec::with_capacity(2 << 16), 
            hash_table: HashTable::new(),
            ends: Vec::new()
        }
    }

    pub(crate) fn intern_string(&mut self, s: &str) -> StringPoolRef {
        let mut fx_hasher = FxHasher::default();
        s.hash(&mut fx_hasher);

        let eq = |&r: &StringPoolRef| -> bool {
            StringPoolImpl::get_string_impl(&self.backing_mem, &self.ends, r) == s
        };

        let hasher = |&r: &StringPoolRef| -> u64 {
            let mut fx_hasher = FxHasher::default();
            let s = StringPoolImpl::get_string_impl(&self.backing_mem, &self.ends, r);
            s.hash(&mut fx_hasher);
            fx_hasher.finish()
        };

        let entry = self.hash_table.entry(
            fx_hasher.finish(),
            eq, 
            hasher
        );

        use hashbrown::hash_table::Entry;
        match entry {
            Entry::Occupied(occupied_entry) => return *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let string_ref = StringPoolRef(self.ends.len() as u32);
                self.backing_mem.extend_from_slice(s.as_bytes());      
                self.ends.push(self.backing_mem.len() as u32);
                vacant_entry.insert(string_ref);
                string_ref
            },
        }
    }

    pub(crate) fn lookup(&self, s: &str) -> Option<StringPoolRef> {
        let mut fx_hasher = FxHasher::default();
        s.hash(&mut fx_hasher);

        let eq = |&r: &StringPoolRef| -> bool {
            StringPoolImpl::get_string_impl(&self.backing_mem, &self.ends, r) == s
        };

        self.hash_table.find(
            fx_hasher.finish(), 
            eq
        ).copied()
    }

    fn get_string_impl<'pool>(backing_mem: &'pool [u8], ends: &'pool [u32], r: StringPoolRef) -> &'pool str {
        let start = if r.0 == 0 { 0usize } else {
            ends[(r.0 - 1) as usize] as usize
        };
        let end = ends[r.0 as usize] as usize;

        let slice = &backing_mem[start..end];

        // SAFETY: ranges encoded by ends correspond to valid utf-8 sequences 
        // by construction
        unsafe { str::from_utf8_unchecked(slice) }
    }

    pub(crate) fn get_string(&self, r: StringPoolRef) -> &str {
        StringPoolImpl::get_string_impl(&self.backing_mem, &self.ends, r)
    }

    fn iter_strings(&self) -> impl Iterator<Item = (StringPoolRef, &str)> {
        (0..self.ends.len() as u32)
            .map(StringPoolRef)
            .map(|r| (r, self.get_string(r)))
    }
}

impl std::fmt::Debug for StringPoolImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter_strings()).finish()
    }
}

#[derive(Debug)]
pub(crate) struct StringPoolProxy {
    pub(crate) inner: Rc<RefCell<StringPoolImpl>>
}

impl StringPoolProxy {
    pub(crate) fn new() -> StringPoolProxy {
        StringPoolProxy { inner: Rc::new(RefCell::new(StringPoolImpl::new())) }
    }
    pub(crate) fn intern_string(&mut self, s: &str) -> StringPoolRef {
        self.inner.borrow_mut().intern_string(s)
    }
    pub(crate) fn lookup(&self, s: &str) -> Option<StringPoolRef> {
        self.inner.borrow().lookup(s)
    }
    pub(crate) fn get_string(&self, r: StringPoolRef) -> &str {
        let inner_string_pool = self.inner.borrow();
        let s = inner_string_pool.get_string(r);

        // SAFETY: the only thing which can invalidate s is reallocation due to additional calls to
        // `intern_string`. However, test code does not call get_string, and the parser code is already
        // borrow checked in the non-test configuration
        unsafe { std::mem::transmute::<&str, &'static str>(s) }
    }
}

#[cfg(not(test))]
pub(crate) type StringPool = StringPoolImpl;
#[cfg(test)]
pub(crate) type StringPool = StringPoolProxy;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_pool_deduplicates_while_preserving_identity() {
        let mut string_pool = StringPool::new();
        let foo = string_pool.intern_string("foo");
        let bar = string_pool.intern_string("bar");
        let foo_copy = string_pool.intern_string("foo");
        let foobar = string_pool.intern_string("foobar");

        assert_ne!(foo, bar);
        assert_eq!(foo, foo_copy);
        assert_ne!(foo, foobar);
        assert_ne!(bar, foobar);

        assert_eq!(string_pool.get_string(foo), "foo");
        assert_eq!(string_pool.get_string(bar), "bar");
        assert_eq!(string_pool.get_string(foo_copy), "foo");
        assert_eq!(string_pool.get_string(foobar), "foobar")
    }
}