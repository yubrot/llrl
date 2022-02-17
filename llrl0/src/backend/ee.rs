//! Execution environment compatible types.

use crate::emitter::ir::{CapturedUse, Syntax, SyntaxBody, SyntaxMetadata};
use derive_new::new;
use memoffset::offset_of;
use std::mem::{size_of, ManuallyDrop};

pub mod ffi;
mod sexp;

pub use sexp::*;

/// Interconversion with the equivalent value representation in the host environment.
pub trait EeValue: Sized {
    type HostValue;

    fn into_host(self) -> Self::HostValue;

    fn from_host(host_value: Self::HostValue) -> Self;
}

/// Conversion to static data of the execution environment.
pub trait EeData {
    /// Treat the value as data, without considering indirect references.
    fn direct_data(&self) -> &[u8];

    fn has_indirect_data(&self) -> bool {
        false
    }

    /// Traverse indirect references of the value. Each indirect reference is represented as
    /// a pair of EdData and an offset on the data where the indirect reference is placed.
    /// Therefore, each offset must be aligned with the alignment of pointers.
    fn traverse_indirect_data(&self, f: &mut dyn FnMut(usize, &dyn EeData)) {
        self.traverse_indirect_data_(EeIndirectDataHandler::new(0, f))
    }

    fn traverse_indirect_data_(&self, _handler: EeIndirectDataHandler) {}
}

#[derive(new)]
pub struct EeIndirectDataHandler<'a> {
    offset: usize,
    handler: &'a mut dyn FnMut(usize, &dyn EeData),
}

impl<'a> EeIndirectDataHandler<'a> {
    pub fn offset(&mut self, offset: usize) -> EeIndirectDataHandler {
        EeIndirectDataHandler {
            offset: self.offset + offset,
            handler: self.handler,
        }
    }

    pub fn handle(&mut self, data: &dyn EeData) {
        (self.handler)(self.offset, data);
    }
}

impl<T: EeData + ?Sized> EeData for &'_ T {
    fn direct_data(&self) -> &[u8] {
        T::direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        T::has_indirect_data(self)
    }

    fn traverse_indirect_data_(&self, handler: EeIndirectDataHandler) {
        T::traverse_indirect_data_(self, handler)
    }
}

impl EeData for str {
    fn direct_data(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl EeData for [u8] {
    fn direct_data(&self) -> &[u8] {
        self
    }
}

fn sized_direct_data<T>(value: &T) -> &[u8] {
    unsafe { std::slice::from_raw_parts(value as *const T as *const u8, size_of::<T>()) }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeString {
    ptr: *const u8,
    len: u64,
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

    pub fn from_str(s: &str) -> Self {
        if s.is_empty() {
            Self {
                ptr: std::ptr::null(),
                len: 0,
            }
        } else {
            let ptr = unsafe { llrt::GC_malloc(s.len()) } as *mut u8;
            unsafe { std::ptr::copy_nonoverlapping(s.as_ptr(), ptr, s.len()) };
            Self {
                ptr: ptr as *const u8,
                len: s.len() as u64,
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
        Self::from_str(host_value.as_str())
    }
}

impl EeData for EeString {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        true
    }

    fn traverse_indirect_data_(&self, mut handler: EeIndirectDataHandler) {
        handler.offset(offset_of!(Self, ptr)).handle(&self.as_str());
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct EeChar(char);

impl EeValue for EeChar {
    type HostValue = char;

    fn into_host(self) -> Self::HostValue {
        self.0
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        Self(host_value)
    }
}

impl EeData for EeChar {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct EeArray<T> {
    ptr: *mut T,
    len: u64,
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

impl EeData for EeCapturedUse {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}

#[repr(C)]
pub struct EeResult<T, E> {
    tag: u8,
    body: EeResultBody<T, E>,
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

    pub fn as_inner(&self) -> Result<&T, &E> {
        match self.tag {
            0 => Err(unsafe { &self.body.err }),
            1 => Ok(unsafe { &self.body.ok }),
            tag => panic!("Wrong tag: {}", tag),
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

impl<T: EeData, E: EeData> EeData for EeResult<T, E> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        match self.as_inner() {
            Ok(a) => a.has_indirect_data(),
            Err(a) => a.has_indirect_data(),
        }
    }

    fn traverse_indirect_data_(&self, mut handler: EeIndirectDataHandler) {
        let handler = handler.offset(offset_of!(Self, body));
        match self.as_inner() {
            Ok(a) => a.traverse_indirect_data_(handler),
            Err(a) => a.traverse_indirect_data_(handler),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
union EeResultBody<T, E> {
    err: ManuallyDrop<E>, // for tag=0
    ok: ManuallyDrop<T>,  // for tag=1
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct EeSyntax<T>(*const EeSyntaxBuffer<T>);

impl<T: EeValue + Clone> EeValue for EeSyntax<T>
where
    T::HostValue: SyntaxBody,
{
    type HostValue = Syntax<T::HostValue>;

    fn into_host(self) -> Self::HostValue {
        // We need to clone *self.0 since it is managed by GC
        unsafe { &*self.0 }.clone().into_host()
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        let buffer = EeSyntaxBuffer::<T>::from_host(host_value);
        // NOTE: Since the buffer is managed by GC, buffer.body: T is never dropped
        let ptr =
            unsafe { llrt::GC_malloc(size_of::<EeSyntaxBuffer<T>>()) } as *mut EeSyntaxBuffer<T>;
        unsafe { std::ptr::write(ptr, buffer) };
        Self(ptr as *const _)
    }
}

impl<T: EeData> EeData for EeSyntax<T> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        true
    }

    fn traverse_indirect_data_(&self, mut handler: EeIndirectDataHandler) {
        handler.handle(unsafe { &*self.0 });
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct EeSyntaxBuffer<T> {
    metadata: EeSyntaxMetadata,
    body: T,
}

impl<T: EeValue> EeValue for EeSyntaxBuffer<T>
where
    T::HostValue: SyntaxBody,
{
    type HostValue = Syntax<T::HostValue>;

    fn into_host(self) -> Self::HostValue {
        let meta = self.metadata.into_host();
        let body = self.body.into_host();
        body.pack(meta)
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        let (meta, body) = T::HostValue::unpack(host_value);
        let meta = EeSyntaxMetadata::from_host(meta);
        let body = T::from_host(body);
        Self::new(meta, body)
    }
}

impl<T: EeData> EeData for EeSyntaxBuffer<T> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        self.body.has_indirect_data()
    }

    fn traverse_indirect_data_(&self, mut handler: EeIndirectDataHandler) {
        self.body
            .traverse_indirect_data_(handler.offset(offset_of!(Self, body)));
    }
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

impl EeData for EeSyntaxMetadata {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}
