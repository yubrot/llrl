use crate::lowering::ir::{CapturedUse, Sexp, Syntax, SyntaxBody, SyntaxMetadata};
use derive_new::new;
use memoffset::offset_of;
use std::mem::{align_of, size_of, ManuallyDrop};

/// Interconversion with the equivalent value representation in the host environment.
pub trait NativeValue: Sized {
    type HostValue;

    fn into_host(self) -> Self::HostValue;

    fn from_host(host_value: Self::HostValue) -> Self;
}

/// Data represented by bytes and indirect references.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct Rep {
    pub align: usize,
    pub direct: Vec<u8>,
    pub indirect: Vec<(usize, Rep)>,
}

impl Rep {
    pub fn zero(size: usize, align: usize) -> Self {
        Self::new(align, vec![0; size], Vec::new())
    }

    pub fn bytes(bytes: &[u8]) -> Self {
        Self::new(1, bytes.to_vec(), Vec::new())
    }

    pub fn of<T: Embed>(value: &T) -> Self {
        let mut rep = Self::zero(size_of::<T>(), align_of::<T>());
        value.embed(RepWriter::new(&mut rep, 0));
        rep
    }
}

pub trait Embed {
    fn embed(&self, w: RepWriter<'_>);
}

macro_rules! impl_embed_primitive {
    ($ty:ty, |$self:ident, $w:ident| $bytes:expr) => {
        impl Embed for $ty {
            fn embed(&$self, mut $w: RepWriter<'_>) {
                $w.write(&$bytes);
            }
        }
    };
}

impl_embed_primitive!(bool, |self, w| [*self as u8]);
impl_embed_primitive!(u8, |self, w| [*self]);
impl_embed_primitive!(u16, |self, w| self.to_le_bytes());
impl_embed_primitive!(u32, |self, w| self.to_le_bytes());
impl_embed_primitive!(u64, |self, w| self.to_le_bytes());
impl_embed_primitive!(f32, |self, w| self.to_bits().to_le_bytes());
impl_embed_primitive!(f64, |self, w| self.to_bits().to_le_bytes());
impl_embed_primitive!(char, |self, w| (*self as u32).to_le_bytes());

/// An utility to build `Rep`.
#[derive(Debug, new)]
pub struct RepWriter<'a> {
    rep: &'a mut Rep,
    offset: usize,
}

impl<'a> RepWriter<'a> {
    pub fn write(&mut self, src: &[u8]) {
        self.rep.direct[self.offset..self.offset + src.len()].copy_from_slice(src);
    }

    pub fn offset(&mut self, offset: usize) -> RepWriter {
        RepWriter::new(self.rep, self.offset + offset)
    }

    pub fn put(&mut self, offset: usize, a: &(impl Embed + ?Sized)) {
        a.embed(self.offset(offset));
    }

    pub fn reference(&mut self, offset: usize, rep: Rep) {
        self.rep.indirect.push((offset + self.offset, rep));
    }
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

impl Embed for NativeString {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.reference(offset_of!(Self, ptr), Rep::bytes(self.as_str().as_bytes()));
        w.put(offset_of!(Self, len), &self.len);
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

impl Embed for NativeChar {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(0, &self.0);
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

impl Embed for NativeCapturedUse {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(offset_of!(Self, tag), &self.tag);
        w.put(offset_of!(Self, node_id), &self.node_id);
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

impl<T: Embed, E: Embed> Embed for NativeResult<T, E> {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(offset_of!(Self, tag), &self.tag);
        match self.as_inner() {
            Ok(a) => w.put(offset_of!(Self, body), a),
            Err(a) => w.put(offset_of!(Self, body), a),
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

impl<T: Embed> Embed for NativeSyntax<T> {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.reference(0, Rep::of(unsafe { &*self.0 }));
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

impl<T: Embed> Embed for NativeSyntaxBuffer<T> {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(offset_of!(Self, metadata), &self.metadata);
        w.put(offset_of!(Self, body), &self.body);
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

impl Embed for NativeSyntaxMetadata {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(offset_of!(Self, ip), &self.ip);
        w.put(offset_of!(Self, ir), &self.ir);
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct NativeSexp {
    tag: u8,
    body: NativeSexpBody,
}

impl NativeSexp {
    pub fn integer(signed: bool, value: u64) -> Self {
        let integer = NativeSexpInteger::new(signed, value);
        Self {
            tag: 0,
            body: NativeSexpBody { integer },
        }
    }

    pub fn fpnumber(value: f64) -> Self {
        let fpnumber = NativeSexpFPNumber::new(value);
        Self {
            tag: 1,
            body: NativeSexpBody { fpnumber },
        }
    }

    pub fn bool(value: bool) -> Self {
        let bool = NativeSexpBool::new(value);
        Self {
            tag: 2,
            body: NativeSexpBody { bool },
        }
    }

    pub fn symbol(value: NativeString) -> Self {
        let symbol = NativeSexpSymbol::new(value);
        Self {
            tag: 3,
            body: NativeSexpBody { symbol },
        }
    }

    pub fn string(value: NativeString) -> Self {
        let string = NativeSexpString::new(value);
        Self {
            tag: 4,
            body: NativeSexpBody { string },
        }
    }

    pub fn char(value: NativeChar) -> Self {
        let char = NativeSexpChar::new(value);
        Self {
            tag: 5,
            body: NativeSexpBody { char },
        }
    }

    pub fn cons(car: NativeSyntax<NativeSexp>, cdr: NativeSyntax<NativeSexp>) -> Self {
        let cons = NativeSexpCons::new(car, cdr);
        Self {
            tag: 6,
            body: NativeSexpBody { cons },
        }
    }

    pub fn nil() -> Self {
        let nil = NativeSexpNil;
        Self {
            tag: 7,
            body: NativeSexpBody { nil },
        }
    }

    pub fn use_(value: NativeCapturedUse) -> Self {
        let use_ = NativeSexpUse::new(value);
        Self {
            tag: 8,
            body: NativeSexpBody { use_ },
        }
    }

    fn into_view(self) -> NativeSexpView {
        match self.tag {
            0 => NativeSexpView::Integer(unsafe { self.body.integer }),
            1 => NativeSexpView::FPNumber(unsafe { self.body.fpnumber }),
            2 => NativeSexpView::Bool(unsafe { self.body.bool }),
            3 => NativeSexpView::Symbol(unsafe { self.body.symbol }),
            4 => NativeSexpView::String(unsafe { self.body.string }),
            5 => NativeSexpView::Char(unsafe { self.body.char }),
            6 => NativeSexpView::Cons(unsafe { self.body.cons }),
            7 => NativeSexpView::Nil(unsafe { self.body.nil }),
            8 => NativeSexpView::Use(unsafe { self.body.use_ }),
            _ => panic!("Wrong tag: {}", self.tag),
        }
    }
}

impl NativeValue for NativeSexp {
    type HostValue = Sexp;

    fn into_host(self) -> Self::HostValue {
        match self.into_view() {
            NativeSexpView::Integer(i) => Sexp::Integer(i.signed, i.value),
            NativeSexpView::FPNumber(f) => Sexp::FPNumber(f.value.into()),
            NativeSexpView::Bool(b) => Sexp::Bool(b.value),
            NativeSexpView::Symbol(s) => Sexp::Symbol(s.value.into_host()),
            NativeSexpView::String(s) => Sexp::String(s.value.into_host()),
            NativeSexpView::Char(c) => Sexp::Char(c.value.into_host()),
            NativeSexpView::Cons(c) => Sexp::Cons(c.car.into_host(), c.cdr.into_host()),
            NativeSexpView::Nil(_) => Sexp::Nil,
            NativeSexpView::Use(u) => Sexp::Use(u.value.into_host()),
        }
    }

    fn from_host(host_value: Self::HostValue) -> Self {
        match host_value {
            Sexp::Integer(signed, value) => NativeSexp::integer(signed, value),
            Sexp::FPNumber(value) => NativeSexp::fpnumber(value.into_inner()),
            Sexp::Bool(value) => NativeSexp::bool(value),
            Sexp::Symbol(value) => NativeSexp::symbol(NativeString::from_host(value)),
            Sexp::String(value) => NativeSexp::string(NativeString::from_host(value)),
            Sexp::Char(value) => NativeSexp::char(NativeChar::from_host(value)),
            Sexp::Cons(car, cdr) => NativeSexp::cons(
                NativeSyntax::<NativeSexp>::from_host(car),
                NativeSyntax::<NativeSexp>::from_host(cdr),
            ),
            Sexp::Nil => NativeSexp::nil(),
            Sexp::Use(value) => NativeSexp::use_(NativeCapturedUse::from_host(value)),
        }
    }
}
impl Embed for NativeSexp {
    fn embed(&self, mut w: RepWriter<'_>) {
        w.put(offset_of!(Self, tag), &self.tag);
        let mut w = w.offset(offset_of!(Self, body));
        match &self.into_view() {
            NativeSexpView::Integer(s) => {
                w.put(offset_of!(NativeSexpInteger, signed), &s.signed);
                w.put(offset_of!(NativeSexpInteger, value), &s.value);
            }
            NativeSexpView::FPNumber(s) => {
                w.put(offset_of!(NativeSexpFPNumber, value), &s.value);
            }
            NativeSexpView::Bool(s) => {
                w.put(offset_of!(NativeSexpBool, value), &s.value);
            }
            NativeSexpView::Symbol(s) => {
                w.put(offset_of!(NativeSexpSymbol, value), &s.value);
            }
            NativeSexpView::String(s) => {
                w.put(offset_of!(NativeSexpString, value), &s.value);
            }
            NativeSexpView::Char(s) => {
                w.put(offset_of!(NativeSexpChar, value), &s.value);
            }
            NativeSexpView::Cons(s) => {
                w.put(offset_of!(NativeSexpCons, car), &s.car);
                w.put(offset_of!(NativeSexpCons, cdr), &s.cdr);
            }
            NativeSexpView::Nil(_) => {}
            NativeSexpView::Use(s) => {
                w.put(offset_of!(NativeSexpUse, value), &s.value);
            }
        }
    }
}

enum NativeSexpView {
    Integer(NativeSexpInteger),
    FPNumber(NativeSexpFPNumber),
    Bool(NativeSexpBool),
    Symbol(NativeSexpSymbol),
    String(NativeSexpString),
    Char(NativeSexpChar),
    Cons(NativeSexpCons),
    Nil(NativeSexpNil),
    Use(NativeSexpUse),
}

#[derive(Clone, Copy)]
#[repr(C)]
union NativeSexpBody {
    integer: NativeSexpInteger,   // for tag=0
    fpnumber: NativeSexpFPNumber, // for tag=1
    bool: NativeSexpBool,         // for tag=2
    symbol: NativeSexpSymbol,     // for tag=3
    string: NativeSexpString,     // for tag=4
    char: NativeSexpChar,         // for tag=5
    cons: NativeSexpCons,         // for tag=6
    nil: NativeSexpNil,           // for tag=7
    use_: NativeSexpUse,          // for tag=8
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpInteger {
    signed: bool,
    value: u64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpFPNumber {
    value: f64,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpBool {
    value: bool,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpSymbol {
    value: NativeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpString {
    value: NativeString,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpChar {
    value: NativeChar,
}

#[derive(Clone, Copy, new)]
#[repr(C)]
struct NativeSexpCons {
    car: NativeSyntax<NativeSexp>,
    cdr: NativeSyntax<NativeSexp>,
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpNil;

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct NativeSexpUse {
    value: NativeCapturedUse,
}
