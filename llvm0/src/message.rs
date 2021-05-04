use llvm_sys::core::*;
use std::ffi::{CStr, CString, NulError};
use std::fmt;
use std::ops::Deref;
use std::os::raw::c_char;

pub struct Message(*mut c_char);

impl Message {
    pub fn new<T: Into<Vec<u8>>>(t: T) -> Result<Self, NulError> {
        let t = CString::new(t)?;
        Ok(Message(unsafe { LLVMCreateMessage(t.as_ptr()) }))
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn from_ptr(p: *mut c_char) -> Self {
        Message(p)
    }
}

impl Deref for Message {
    type Target = CStr;

    fn deref(&self) -> &Self::Target {
        unsafe { CStr::from_ptr(self.0) }
    }
}

impl Drop for Message {
    fn drop(&mut self) {
        unsafe { LLVMDisposeMessage(self.0) };
    }
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Message::new({:?})", self.to_string_lossy())
    }
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string_lossy(), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lifecycle() {
        let msg = Message::new("hello");
        assert!(msg.is_ok());
        assert_eq!(msg.unwrap().to_string_lossy(), "hello");
    }
}
