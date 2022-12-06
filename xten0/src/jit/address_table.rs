use super::mmap::Error;
use super::table::Table;
use std::collections::{hash_map::Entry as HashMapEntry, HashMap};

#[derive(Debug)]
pub struct AddressTable {
    entries: HashMap<String, *const *const u8>,
    payload: Table<*const u8>,
}

impl AddressTable {
    pub fn new(hint_addr: *const u8) -> Self {
        Self {
            entries: HashMap::new(),
            payload: Table::new(hint_addr),
        }
    }

    pub fn prepare(
        &mut self,
        symbol: &str,
        resolve_symbol: impl FnOnce() -> Option<*const u8>,
    ) -> Result<Option<*const *const u8>, Error> {
        match self.entries.entry(symbol.to_owned()) {
            HashMapEntry::Occupied(e) => Ok(Some(*e.get())),
            HashMapEntry::Vacant(e) => match resolve_symbol() {
                Some(ptr) => Ok(Some(*e.insert(self.payload.put(ptr)?))),
                None => Ok(None),
            },
        }
    }
}
