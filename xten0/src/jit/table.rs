use super::mmap::{Error, Protect};
use super::segment::Segment;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ptr;

/// Sequence of fixed-size entries built on a Segment.
#[derive(Debug)]
pub struct Table<T: Copy> {
    segment: Segment,
    _entries: PhantomData<T>,
}

impl<T: Copy> Table<T> {
    pub fn new(map_32bit: bool) -> Self {
        Self {
            segment: Segment::new(Protect::ReadOnly, map_32bit),
            _entries: PhantomData,
        }
    }

    /// Add an entry. Returns the address corresponding to the entry.
    pub fn put(&mut self, entry: T) -> Result<*const T, Error> {
        let mut part = self.segment.allocate(size_of::<T>())?;
        part.set_protect_temporarily(Protect::ReadWrite)?; // TODO: Reduce mprotect calls
        let ptr = part.as_ptr() as *mut T;
        unsafe { ptr::write(ptr, entry) };
        drop(part);
        Ok(ptr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table() {
        let mut table = Table::new(false);

        for i in 0u64..522 {
            let entry = table.put(i).unwrap();
            assert_eq!(unsafe { ptr::read(entry) }, i);
        }
    }
}
