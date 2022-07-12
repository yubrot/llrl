use super::{Mmap, MmapError, Protect};
use derive_new::new;
use std::ptr;

/// A collection of non-contiguous `Mmap`s with the same `Protect`.
#[derive(Debug)]
pub struct Segment {
    hint_addr: *const u8,
    protect: Protect,
    chunks: Vec<Mmap>,
    offset: usize,
}

impl Segment {
    pub fn new(hint_addr: *const u8, protect: Protect) -> Self {
        Self {
            hint_addr,
            protect,
            chunks: Vec::new(),
            offset: 0,
        }
    }

    pub fn protect(&self) -> Protect {
        self.protect
    }

    fn last_mmap_capacity(&self) -> usize {
        if let Some(last_mmap) = self.chunks.last() {
            last_mmap.size() - self.offset
        } else {
            0
        }
    }

    /// Allocates free space for the specified size.
    /// If there is not enough space, a new mmap is created.
    pub fn allocate(&mut self, size: usize) -> Result<SegmentPart, MmapError> {
        if self.last_mmap_capacity() < size {
            let pages = (size + Mmap::PAGE_SIZE - 1) / Mmap::PAGE_SIZE;
            let mmap = Mmap::new_near(self.hint_addr, pages, self.protect)?;
            self.chunks.push(mmap);
            self.offset = 0;
        }
        let part = SegmentPart::new(self.protect, self.chunks.last_mut(), self.offset, size);
        self.offset += size;
        Ok(part)
    }
}

/// Free space allocated from the segment.
#[derive(Debug, new)]
pub struct SegmentPart<'a> {
    protect: Protect,
    mmap: Option<&'a mut Mmap>,
    offset: usize,
    size: usize,
}

impl<'a> SegmentPart<'a> {
    /// Access protection set for the segment.
    pub fn segment_protect(&self) -> Protect {
        self.protect
    }

    /// To initialize this area, temporarily set the access protection of the `Mmap` to which
    /// this `SegmentPart` belongs to a different value.
    ///
    /// The temporarily changed access protection setting is automatically restored when the
    /// `SegmentPart` is dropped.
    pub fn set_protect_temporarily(&mut self, protect: Protect) -> Result<(), MmapError> {
        match self.mmap {
            Some(ref mut mmap) => mmap.set_protect(protect),
            None => Ok(()),
        }
    }

    pub fn as_ptr(&self) -> *mut u8 {
        match self.mmap {
            Some(ref mmap) => mmap.as_ptr().wrapping_add(self.offset),
            None => ptr::null_mut(),
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

impl<'a> Drop for SegmentPart<'a> {
    fn drop(&mut self) {
        if let Some(ref mut mmap) = self.mmap {
            mmap.set_protect(self.protect).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    #[test]
    fn segment() {
        let some_heap_space = Box::new(0u8);
        let some_heap_ptr = &*some_heap_space as *const u8;
        let mut segment = Segment::new(some_heap_ptr, Protect::ReadOnly);

        let mut part = segment.allocate(1024 * 5).unwrap();
        assert_eq!(part.size(), 1024 * 5);

        // no protection error
        assert!(part.set_protect_temporarily(Protect::ReadWrite).is_ok());
        let ptr1 = part.as_ptr();
        unsafe { ptr::copy(vec![0u8; 1024 * 5].as_ptr(), ptr1, 1024 * 5) };
        drop(part);

        // protection setting is restored
        let part = segment.allocate(1024).unwrap();
        assert_eq!(part.size(), 1024);
        assert_eq!(
            part.mmap.as_ref().map(|m| m.protect()),
            Some(Protect::ReadOnly)
        );

        // existing mmap is used
        let ptr2 = part.as_ptr();
        assert_eq!(ptr1.wrapping_add(1024 * 5), ptr2);
        drop(part);

        // new mmap is created
        let part = segment.allocate(1024 * 3).unwrap();
        assert_eq!(part.size(), 1024 * 3);
        let ptr3 = part.as_ptr();
        assert!(
            unsafe { ptr3.offset_from(ptr1) } <= -1024 * 4
                || 1024 * 2 <= unsafe { ptr3.offset_from(ptr2) }
        );
    }
}
