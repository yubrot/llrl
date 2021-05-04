use std::ffi::CStr;
use std::fmt;
use std::ops::Deref;
use std::os::raw::c_char;

use super::*;
use llvm_sys::error::*;

pub struct Error {
    _refs: LLVMOpaqueError,
}

impl Error {
    pub unsafe fn from_ptr(ptr: LLVMErrorRef) -> Option<Oo<Self>> {
        if ptr.is_null() {
            None
        } else {
            Some(Oo::from_ptr(ptr))
        }
    }
}

impl_opaque_eq!(Error);

impl_opaque_debug!(Error);

unsafe_impl_opaque!(Error(LLVMOpaqueError));

impl OpaqueOwn for Error {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMConsumeError(a);
    }
}

pub struct ErrorMessage(*mut c_char);

impl ErrorMessage {
    pub unsafe fn from_ptr(p: *mut c_char) -> Self {
        Self(p)
    }
}

impl Deref for ErrorMessage {
    type Target = CStr;

    fn deref(&self) -> &Self::Target {
        unsafe { CStr::from_ptr(self.0) }
    }
}

impl Drop for ErrorMessage {
    fn drop(&mut self) {
        unsafe { LLVMDisposeErrorMessage(self.0) };
    }
}

impl From<Oo<Error>> for ErrorMessage {
    fn from(e: Oo<Error>) -> Self {
        unsafe { Self::from_ptr(LLVMGetErrorMessage(Oo::into_ptr(e))) }
    }
}

impl fmt::Debug for ErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErrorMessage({:?})", self.to_string_lossy())
    }
}

impl fmt::Display for ErrorMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.to_string_lossy(), f)
    }
}
