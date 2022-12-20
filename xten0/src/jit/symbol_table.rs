use super::Error;
use std::collections::{hash_map::Entry as HashMapEntry, HashMap};

#[derive(Debug)]
pub struct SymbolTable<R> {
    entries: HashMap<String, *const u8>,
    resolver: R,
}

impl<R: Resolver> SymbolTable<R> {
    pub fn new(resolver: R) -> Self {
        Self {
            entries: HashMap::new(),
            resolver,
        }
    }

    pub fn bind(&mut self, symbol: &str, ptr: Option<*const u8>) -> Result<(), Error> {
        match (self.entries.entry(symbol.to_owned()), ptr) {
            (HashMapEntry::Vacant(e), Some(ptr)) => {
                e.insert(ptr);
                Ok(())
            }
            // NOTE: Should we defer use of the `resolver` until `SymbolTable::resolve`?
            // The current implementation has the assumption that any symbols that are not known
            // at bind time are given by the `resolver`.
            (HashMapEntry::Vacant(e), None) => match (self.resolver)(symbol) {
                ptr if !ptr.is_null() => {
                    e.insert(ptr);
                    Ok(())
                }
                _ => Err(Error::UndefinedSymbol(symbol.to_owned())),
            },
            (_, Some(_)) => Err(Error::DuplicateSymbol(symbol.to_owned())),
            // Use of known symbols
            (_, None) => Ok(()),
        }
    }

    pub fn resolve(&self, symbol: &str) -> Option<*const u8> {
        self.entries.get(symbol).copied()
    }
}

pub type ResolverFn = fn(&str) -> *const u8;

/// Alias of `FnMut(&str) -> Option<*const u8>`. This is used for resolving undefined symbols.
pub trait Resolver: FnMut(&str) -> *const u8 {}

impl<T: FnMut(&str) -> *const u8> Resolver for T {}

pub mod resolver {
    pub fn none(_: &str) -> *const u8 {
        std::ptr::null()
    }

    #[cfg(all(unix, feature = "dl"))]
    pub mod dl {
        use libc::{c_char, c_void};
        use std::ffi::CString;

        pub fn default(sym: &str) -> *const u8 {
            let name = CString::new(sym).unwrap();
            (unsafe { dlsym(RTLD_DEFAULT, name.as_ptr()) }) as *const u8
        }

        const RTLD_DEFAULT: *mut c_void = 0 as *mut c_void;

        #[link(name = "dl")]
        extern "C" {
            fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
        }
    }
}
