//! A set of definitions ABI-compatible with the native targeting backend's data layout.

use crate::lowering::ir::{CapturedUse, Syntax, SyntaxBody, SyntaxMetadata};
use derive_new::new;
use memoffset::offset_of;
use std::mem::{size_of, ManuallyDrop};

pub mod context;
mod function;
mod sexp;
mod size_align;

pub use function::*;
pub use sexp::*;
pub use size_align::{SizeAlign, SizeAlignResolver};

/// Interconversion with the equivalent value representation in the host environment.
pub trait NativeValue: Sized {
    type HostValue;

    fn into_host(self) -> Self::HostValue;

    fn from_host(host_value: Self::HostValue) -> Self;
}

/// Conversion to static data of the native environment.
pub trait NativeData {
    /// Treat the value as data, without considering indirect references.
    fn direct_data(&self) -> &[u8];

    fn has_indirect_data(&self) -> bool {
        false
    }

    /// Traverse indirect references of the value. Each indirect reference is represented as
    /// a pair of NativeData and an offset on the data where the indirect reference is placed.
    /// Therefore, each offset must be aligned with the alignment of pointers.
    fn traverse_indirect_data(&self, f: &mut dyn FnMut(usize, &dyn NativeData)) {
        self.traverse_indirect_data_(NativeIndirectDataHandler::new(0, f))
    }

    fn traverse_indirect_data_(&self, _handler: NativeIndirectDataHandler) {}
}

#[derive(new)]
pub struct NativeIndirectDataHandler<'a> {
    offset: usize,
    handler: &'a mut dyn FnMut(usize, &dyn NativeData),
}

impl<'a> NativeIndirectDataHandler<'a> {
    pub fn offset(&mut self, offset: usize) -> NativeIndirectDataHandler {
        NativeIndirectDataHandler {
            offset: self.offset + offset,
            handler: self.handler,
        }
    }

    pub fn handle(&mut self, data: &dyn NativeData) {
        (self.handler)(self.offset, data);
    }
}

impl<T: NativeData + ?Sized> NativeData for &'_ T {
    fn direct_data(&self) -> &[u8] {
        T::direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        T::has_indirect_data(self)
    }

    fn traverse_indirect_data_(&self, handler: NativeIndirectDataHandler) {
        T::traverse_indirect_data_(self, handler)
    }
}

impl NativeData for str {
    fn direct_data(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl NativeData for [u8] {
    fn direct_data(&self) -> &[u8] {
        self
    }
}

fn sized_direct_data<T>(value: &T) -> &[u8] {
    unsafe { std::slice::from_raw_parts(value as *const T as *const u8, size_of::<T>()) }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NativeString {
    ptr: *const u8,
    len: u64,
}

impl NativeString {
    pub fn as_str(&self) -> &str {
        unsafe {
            if self.len == 0 {
                ""
            } else {
                let slice = std::slice::from_raw_parts(self.ptr, self.len as usize);
                std::str::from_utf8_unchecked(slice)
            }
        }
    }

    #[allow(clippy::should_implement_trait)]
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

impl NativeValue for NativeString {
    type HostValue = String;

    fn into_host(self) -> Self::HostValue {
        self.as_str().to_string()
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        Self::from_str(host_value.as_str())
    }
}

impl NativeData for NativeString {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        true
    }

    fn traverse_indirect_data_(&self, mut handler: NativeIndirectDataHandler) {
        handler.offset(offset_of!(Self, ptr)).handle(&self.as_str());
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct NativeChar(char);

impl NativeValue for NativeChar {
    type HostValue = char;

    fn into_host(self) -> Self::HostValue {
        self.0
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        Self(host_value)
    }
}

impl NativeData for NativeChar {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NativeArray<T> {
    ptr: *mut T,
    len: u64,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NativeCapturedUse {
    pub tag: u64,
    pub node_id: u64,
}

impl NativeValue for NativeCapturedUse {
    // NOTE: Since Construct is just a Rust enum, this compatibility can be broken by Rust changes.
    type HostValue = CapturedUse; // ~ Construct

    fn into_host(self) -> Self::HostValue {
        unsafe { std::mem::transmute(self) }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        unsafe { std::mem::transmute(host_value) }
    }
}

impl NativeData for NativeCapturedUse {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}

#[repr(C)]
pub struct NativeResult<T, E> {
    tag: u8,
    body: NativeResultBody<T, E>,
}

impl<T, E> NativeResult<T, E> {
    pub fn err(err: E) -> Self {
        let err = ManuallyDrop::new(err);
        Self {
            tag: 0,
            body: NativeResultBody { err },
        }
    }

    pub fn ok(ok: T) -> Self {
        let ok = ManuallyDrop::new(ok);
        Self {
            tag: 1,
            body: NativeResultBody { ok },
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

impl<T: NativeValue, E: NativeValue> NativeValue for NativeResult<T, E> {
    type HostValue = Result<T::HostValue, E::HostValue>;

    fn into_host(self) -> Self::HostValue {
        match self.into_inner() {
            Err(err) => Err(err.into_host()),
            Ok(ok) => Ok(ok.into_host()),
        }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        match host_value {
            Err(e) => NativeResult::err(E::from_host(e)),
            Ok(t) => NativeResult::ok(T::from_host(t)),
        }
    }
}

impl<T: NativeData, E: NativeData> NativeData for NativeResult<T, E> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        match self.as_inner() {
            Ok(a) => a.has_indirect_data(),
            Err(a) => a.has_indirect_data(),
        }
    }

    fn traverse_indirect_data_(&self, mut handler: NativeIndirectDataHandler) {
        let handler = handler.offset(offset_of!(Self, body));
        match self.as_inner() {
            Ok(a) => a.traverse_indirect_data_(handler),
            Err(a) => a.traverse_indirect_data_(handler),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
union NativeResultBody<T, E> {
    err: ManuallyDrop<E>, // for tag=0
    ok: ManuallyDrop<T>,  // for tag=1
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct NativeSyntax<T>(*const NativeSyntaxBuffer<T>);

impl<T: NativeValue + Clone> NativeValue for NativeSyntax<T>
where
    T::HostValue: SyntaxBody,
{
    type HostValue = Syntax<T::HostValue>;

    fn into_host(self) -> Self::HostValue {
        // We need to clone *self.0 since it is managed by GC
        unsafe { &*self.0 }.clone().into_host()
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        let buffer = NativeSyntaxBuffer::<T>::from_host(host_value);
        // NOTE: Since the buffer is managed by GC, buffer.body: T is never dropped
        let ptr = unsafe { llrt::GC_malloc(size_of::<NativeSyntaxBuffer<T>>()) }
            as *mut NativeSyntaxBuffer<T>;
        unsafe { std::ptr::write(ptr, buffer) };
        Self(ptr as *const _)
    }
}

impl<T: NativeData> NativeData for NativeSyntax<T> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        true
    }

    fn traverse_indirect_data_(&self, mut handler: NativeIndirectDataHandler) {
        handler.handle(unsafe { &*self.0 });
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
pub struct NativeSyntaxBuffer<T> {
    metadata: NativeSyntaxMetadata,
    body: T,
}

impl<T: NativeValue> NativeValue for NativeSyntaxBuffer<T>
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
        let meta = NativeSyntaxMetadata::from_host(meta);
        let body = T::from_host(body);
        Self::new(meta, body)
    }
}

impl<T: NativeData> NativeData for NativeSyntaxBuffer<T> {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }

    fn has_indirect_data(&self) -> bool {
        self.body.has_indirect_data()
    }

    fn traverse_indirect_data_(&self, mut handler: NativeIndirectDataHandler) {
        self.body
            .traverse_indirect_data_(handler.offset(offset_of!(Self, body)));
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct NativeSyntaxMetadata {
    pub ip: u32,
    pub ir: u32,
}

impl NativeValue for NativeSyntaxMetadata {
    type HostValue = SyntaxMetadata; // ~ SourceLocation ~ (Interned<Path>, Interned<SourceLocation>)

    fn into_host(self) -> Self::HostValue {
        unsafe { std::mem::transmute(self) }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        unsafe { std::mem::transmute(host_value) }
    }
}

impl NativeData for NativeSyntaxMetadata {
    fn direct_data(&self) -> &[u8] {
        sized_direct_data(self)
    }
}
