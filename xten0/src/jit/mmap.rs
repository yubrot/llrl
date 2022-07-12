use libc::{
    __errno_location, c_void, mmap, mprotect, munmap, EINVAL, ENOMEM, MAP_ANONYMOUS, MAP_PRIVATE,
    PROT_EXEC, PROT_NONE, PROT_READ, PROT_WRITE,
};

/// Memory area mapped by `mmap(2)`.
#[derive(Debug)]
pub struct Mmap {
    ptr: *mut c_void,
    pages: usize,
    protect: Protect,
}

impl Mmap {
    /// Create a new memory map near the specified address.
    pub fn new_near(hint_addr: *const u8, pages: usize, protect: Protect) -> Result<Self, Error> {
        let ptr = unsafe {
            mmap(
                hint_addr as *mut c_void,
                pages * Self::PAGE_SIZE,
                protect.value(),
                MAP_PRIVATE | MAP_ANONYMOUS,
                -1,
                0,
            )
        };
        if ptr == usize::MAX as *mut c_void {
            Err(unsafe { Error::from_errno() })
        } else {
            Ok(Self {
                ptr,
                pages,
                protect,
            })
        }
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.ptr as *mut u8
    }

    pub fn pages(&self) -> usize {
        self.pages
    }

    pub fn size(&self) -> usize {
        self.pages * Self::PAGE_SIZE
    }

    pub fn protect(&self) -> Protect {
        self.protect
    }

    /// Set the access protections of this map.
    pub fn set_protect(&mut self, protect: Protect) -> Result<(), Error> {
        if self.protect != protect {
            let prev_protect = self.protect;
            self.protect = protect;
            match unsafe { mprotect(self.ptr, self.pages * Self::PAGE_SIZE, protect.value()) } {
                0 => Ok(()),
                _ => {
                    self.protect = prev_protect;
                    Err(unsafe { Error::from_errno() })
                }
            }
        } else {
            Ok(())
        }
    }

    pub const PAGE_SIZE: usize = 4096;
}

impl Drop for Mmap {
    fn drop(&mut self) {
        if unsafe { munmap(self.ptr, self.pages * Self::PAGE_SIZE) } == -1 {
            let err = unsafe { Error::from_errno() };
            panic!("{:?}", err);
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, thiserror::Error)]
pub enum Error {
    #[error("Invalid argument")]
    InvalidArgument,
    #[error("Not enough memory")]
    NotEnoughMemory,
    #[error("Unknown error")]
    Unknown,
}

impl Error {
    /// Create a MmapError from `errno`.
    ///
    /// # Safety
    /// The caller must ensure that the error was caused by the previous mmap-related call.
    unsafe fn from_errno() -> Self {
        match *__errno_location() {
            EINVAL => Self::InvalidArgument,
            ENOMEM => Self::NotEnoughMemory,
            _ => Self::Unknown,
        }
    }
}

/// An access protection of `Mmap`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Protect {
    None,
    ReadOnly,
    ReadWrite,
    ReadExec,
}

impl Protect {
    fn value(self) -> i32 {
        match self {
            Self::None => PROT_NONE,
            Self::ReadOnly => PROT_READ,
            Self::ReadWrite => PROT_READ | PROT_WRITE,
            Self::ReadExec => PROT_READ | PROT_EXEC,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mmap_munmap() {
        let some_heap_space = Box::new(0u8);
        let some_heap_ptr = &*some_heap_space as *const u8;
        let mmap = Mmap::new_near(some_heap_ptr, 2, Protect::ReadWrite);
        assert!(mmap.is_ok());
        let mut mmap = mmap.unwrap();
        assert!(unsafe { mmap.as_ptr().offset_from(some_heap_ptr) }.abs() < 2 * 1024 * 1024 * 1024);
        assert_eq!(mmap.pages(), 2);
        assert_eq!(mmap.size(), 2 * Mmap::PAGE_SIZE);
        assert_eq!(mmap.protect(), Protect::ReadWrite);
        assert!(mmap.set_protect(Protect::ReadOnly).is_ok());
        assert_eq!(mmap.protect(), Protect::ReadOnly);
        drop(mmap);
    }
}
