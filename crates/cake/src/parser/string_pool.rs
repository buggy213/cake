//! String interning pool for use within the compiler
//! 
//! Uses `hashbrown::HashTable` and `rustc_hash::FxHasher` for fast lookup of strings

use std::hash::{Hash, Hasher};

use hashbrown::HashTable;
use rustc_hash::FxHasher;

pub(crate) struct StringPool {
    backing_mem: Vec<u8>,
    hash_table: HashTable<StringPoolRef>,
    ends: Vec<u32>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct StringPoolRef(u32);

impl StringPool {
    pub(crate) fn new() -> StringPool {
        StringPool { 
            backing_mem: Vec::with_capacity(2 << 16), 
            hash_table: HashTable::new(),
            ends: Vec::new()
        }
    }

    pub(crate) fn intern_string(&mut self, s: &str) -> StringPoolRef {
        let mut fx_hasher = FxHasher::default();
        s.hash(&mut fx_hasher);

        let eq = |&r: &StringPoolRef| -> bool {
            StringPool::get_string_impl(&self.backing_mem, &self.ends, r) == s
        };

        let hasher = |&r: &StringPoolRef| -> u64 {
            let mut fx_hasher = FxHasher::default();
            let s = StringPool::get_string_impl(&self.backing_mem, &self.ends, r);
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
        StringPool::get_string_impl(&self.backing_mem, &self.ends, r)
    }
}

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