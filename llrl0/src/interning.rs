use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

/// An interned symbol representation.
///
/// Symbols are interned in a `InternTable`.
#[derive(Debug)]
pub struct Interned<T> {
    pub index: u32,
    _marker: PhantomData<fn() -> T>,
}

// Due to rust-lang/rust#26925 we need some manual impls

impl<T> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Interned<T> {}

impl<T> PartialOrd for Interned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Interned<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Interned<T> {}

impl<T> Hash for Interned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}

#[derive(Debug, Clone)]
pub struct InternTable<T: Hash + Eq> {
    ids: Vec<T>,
    symbols: HashMap<T, Interned<T>>,
}

impl<T: Hash + Eq> InternTable<T> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        InternTable {
            ids: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn get_interned<Q: ?Sized>(&self, id: &Q) -> Option<Interned<T>>
    where
        Q: Hash + Eq,
        T: Borrow<Q>,
    {
        self.symbols.get(id).copied()
    }

    pub fn is_interned<Q: ?Sized>(&self, id: &Q) -> bool
    where
        Q: Hash + Eq,
        T: Borrow<Q>,
    {
        self.get_interned(id).is_some()
    }

    pub fn intern<Q: ?Sized>(&mut self, id: &Q) -> Interned<T>
    where
        Q: Hash + Eq + ToOwned<Owned = T>,
        T: Borrow<Q>,
    {
        match self.get_interned(id) {
            Some(existing_symbol) => existing_symbol,
            None => {
                let new_symbol = Interned {
                    index: self.ids.len() as u32,
                    _marker: PhantomData,
                };
                self.ids.push(id.to_owned());
                self.symbols.insert(id.to_owned(), new_symbol);
                new_symbol
            }
        }
    }

    pub fn review(&self, symbol: Interned<T>) -> &T {
        &self.ids[symbol.index as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intern() {
        let mut tbl = InternTable::new();
        assert!(!tbl.is_interned("hello"));
        assert!(!tbl.is_interned("world"));
        let hello = tbl.intern("hello");
        assert!(tbl.is_interned("hello"));
        assert!(!tbl.is_interned("world"));
        let world = tbl.intern("world");
        assert_eq!(tbl.intern("hello"), hello);
        assert_eq!(tbl.intern("world"), world);
        assert_ne!(hello, world);
        assert_eq!(tbl.review(hello), "hello");
        assert_eq!(tbl.review(world), "world");
    }
}
