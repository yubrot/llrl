/// Alias of `FnMut(&str) -> Option<*const u8>`. This is used for resolving undefined symbols.
pub trait SymbolResolver: FnMut(&str) -> Option<*const u8> {}

impl<T: FnMut(&str) -> Option<*const u8>> SymbolResolver for T {}

pub fn none(_: &str) -> Option<*const u8> {
    None
}

#[cfg(all(unix, feature = "dl"))]
pub mod dl {
    use libc::{c_char, c_void};
    use std::ffi::CString;

    pub fn default(sym: &str) -> Option<*const u8> {
        let name = CString::new(sym).unwrap();
        let ptr = unsafe { dlsym(RTLD_DEFAULT, name.as_ptr()) };

        if !ptr.is_null() {
            Some(ptr as *const u8)
        } else {
            None
        }
    }

    const RTLD_DEFAULT: *mut c_void = 0 as *mut c_void;

    #[link(name = "dl")]
    extern "C" {
        fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    }
}
