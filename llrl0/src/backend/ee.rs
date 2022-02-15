//! Execution environment compatible types.

use crate::emitter::ir::{CapturedUse, Syntax, SyntaxBody, SyntaxMetadata};
use derive_new::new;
use std::mem::{size_of, ManuallyDrop};

mod sexp;

pub use sexp::*;

/// Interconversion with the equivalent value representation in the host environment.
pub trait EeValue: Sized {
    type HostValue;

    fn into_host(self) -> Self::HostValue;

    fn from_host(host_value: Self::HostValue) -> Self;
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeString {
    pub ptr: *const u8,
    pub len: u64,
}

impl EeString {
    pub fn as_str(&self) -> &str {
        unsafe {
            if self.len == 0 {
                &""
            } else {
                let slice = std::slice::from_raw_parts(self.ptr, self.len as usize);
                std::str::from_utf8_unchecked(slice)
            }
        }
    }
}

impl EeValue for EeString {
    type HostValue = String;

    fn into_host(self) -> Self::HostValue {
        self.as_str().to_string()
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        unsafe {
            if host_value.is_empty() {
                EeString {
                    ptr: std::ptr::null(),
                    len: 0,
                }
            } else {
                let ptr = llrt::GC_malloc(host_value.len()) as *mut u8;
                std::ptr::copy_nonoverlapping(host_value.as_ptr(), ptr, host_value.len());
                EeString {
                    ptr: ptr as *const u8,
                    len: host_value.len() as u64,
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct EeChar(pub char);

impl EeValue for EeChar {
    type HostValue = char;

    fn into_host(self) -> Self::HostValue {
        self.0
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        Self(host_value)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeArray<T> {
    pub ptr: *mut T,
    pub len: u64,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeCapturedUse {
    pub tag: u64,
    pub node_id: u64,
}

impl EeValue for EeCapturedUse {
    // NOTE: Since Construct is just a Rust enum, this compatibility can be broken by Rust changes.
    type HostValue = CapturedUse; // ~ Construct

    fn into_host(self) -> Self::HostValue {
        unsafe { std::mem::transmute(self) }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        unsafe { std::mem::transmute(host_value) }
    }
}

#[repr(C)]
pub struct EeResult<T, E> {
    pub tag: u8,
    pub body: EeResultBody<T, E>,
}

impl<T, E> EeResult<T, E> {
    pub fn err(err: E) -> Self {
        let err = ManuallyDrop::new(err);
        Self {
            tag: 0,
            body: EeResultBody { err },
        }
    }

    pub fn ok(ok: T) -> Self {
        let ok = ManuallyDrop::new(ok);
        Self {
            tag: 1,
            body: EeResultBody { ok },
        }
    }

    pub fn into_inner(self) -> Result<T, E> {
        match self.tag {
            0 => Err(ManuallyDrop::into_inner(unsafe { self.body.err })),
            1 => Ok(ManuallyDrop::into_inner(unsafe { self.body.ok })),
            tag => panic!("Wrong tag: {}", tag),
        }
    }
}

impl<T: EeValue, E: EeValue> EeValue for EeResult<T, E> {
    type HostValue = Result<T::HostValue, E::HostValue>;

    fn into_host(self) -> Self::HostValue {
        match self.into_inner() {
            Err(err) => Err(err.into_host()),
            Ok(ok) => Ok(ok.into_host()),
        }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        match host_value {
            Err(e) => EeResult::err(E::from_host(e)),
            Ok(t) => EeResult::ok(T::from_host(t)),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union EeResultBody<T, E> {
    pub err: ManuallyDrop<E>, // for tag=0
    pub ok: ManuallyDrop<T>,  // for tag=1
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct EeSyntax<T>(pub *const EeSyntaxBuffer<T>);

impl<T: EeValue> EeValue for EeSyntax<T>
where
    T::HostValue: SyntaxBody,
{
    type HostValue = Syntax<T::HostValue>;

    fn into_host(self) -> Self::HostValue {
        let buffer = unsafe { std::ptr::read(self.0) };
        let meta = buffer.metadata.into_host();
        let body = ManuallyDrop::into_inner(buffer.body).into_host();
        body.pack(meta)
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        let (meta, body) = T::HostValue::unpack(host_value);
        let meta = EeSyntaxMetadata::from_host(meta);
        let body = T::from_host(body);
        let ptr =
            unsafe { llrt::GC_malloc(size_of::<EeSyntaxBuffer<T>>()) } as *mut EeSyntaxBuffer<T>;
        unsafe { std::ptr::write(ptr, EeSyntaxBuffer::new(meta, ManuallyDrop::new(body))) };
        Self(ptr as *const _)
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct EeSyntaxBuffer<T> {
    pub metadata: EeSyntaxMetadata,
    pub body: ManuallyDrop<T>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeSyntaxMetadata {
    pub ip: u32,
    pub ir: u32,
}

impl EeValue for EeSyntaxMetadata {
    type HostValue = SyntaxMetadata; // ~ SourceLocation ~ (Interned<Path>, Interned<SourceLocation>)

    fn into_host(self) -> Self::HostValue {
        unsafe { std::mem::transmute(self) }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        unsafe { std::mem::transmute(host_value) }
    }
}
