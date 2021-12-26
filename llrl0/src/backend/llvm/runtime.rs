use crate::emitter::ir::{CapturedUse, Syntax, SyntaxBody, SyntaxMetadata};
use derive_new::new;
use llvm::prelude::*;
use memoffset::offset_of;
use std::mem::{align_of, size_of, ManuallyDrop};

/// Interconversion with the equivalent value representation in the execution environment.
pub trait RtValue: Sized {
    type Native;

    fn size_align() -> (usize, usize);

    fn into_native(self) -> Self::Native;

    fn from_native(native: Self::Native) -> Self;
}

/// Types that can be represented as a constant on LLVM-IR.
pub trait RtConstant: RtValue {
    type Src: ?Sized;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx>;

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &Self::Src,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm>;
}

/// Types that can be expanded on LLVM-IR array constants.
/// The element type of an array is an integer of pointer size.
pub trait RtExpand {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    );
}

macro_rules! buf_offset {
    ($parent:path, $field:tt) => {
        offset_of!($parent, $field) / size_of::<usize>()
    };
}

mod library;
mod sexp;

pub use library::RtLibrary;
pub use sexp::RtSexp;

pub fn build_panic<'ctx: 'm, 'm>(
    msg: impl LLVMAnyValue<'ctx, 'm>,
    builder: &LLVMBuilder<'ctx, 'm>,
    ctx: &impl BuildContext<'ctx, 'm>,
) {
    let panic = ctx.library().llrt_panic();
    builder.build_call(panic, &[msg.as_value()]);
    builder.build_unreachable();
}

pub fn build_heap_alloc<'ctx: 'm, 'm>(
    ty: impl LLVMAnyType<'ctx>,
    builder: &LLVMBuilder<'ctx, 'm>,
    ctx: &impl BuildContext<'ctx, 'm>,
) -> LLVMValue<'ctx, 'm> {
    let gc_malloc = ctx.library().gc_malloc();
    let size = llvm_constant!(*ctx, (trunc_or_bit_cast isize (size_of {ty})));
    let ptr = builder.build_call(gc_malloc, &[size.as_value()]);
    builder.build_bit_cast(ptr, LLVMPointerType::get(ty, 0))
}

pub fn build_heap_array_alloc<'ctx: 'm, 'm>(
    ty: impl LLVMAnyType<'ctx>,
    num: impl LLVMAnyValue<'ctx, 'm>,
    builder: &LLVMBuilder<'ctx, 'm>,
    ctx: &impl BuildContext<'ctx, 'm>,
) -> LLVMValue<'ctx, 'm> {
    let gc_malloc = ctx.library().gc_malloc();
    let size = llvm_constant!(*ctx, (trunc_or_bit_cast isize (size_of {ty})));
    let size = builder.build_mul(size, num);
    let ptr = builder.build_call(gc_malloc, &[size.as_value()]);
    builder.build_bit_cast(ptr, LLVMPointerType::get(ty, 0))
}

pub trait BuildContext<'ctx: 'm, 'm>: LLVMTypeBuilder<'ctx> {
    fn module(&self) -> &'m LLVMModule<'ctx>;
    fn library(&self) -> &RtLibrary<'ctx, 'm>;
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtString {
    ptr: *const u8,
    len: u64,
}

impl RtString {
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

    pub fn to_llvm_constant<'ctx: 'm, 'm>(
        &self,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        Self::llvm_constant(self.as_str(), module)
    }

    fn llvm_constant_body<'ctx: 'm, 'm>(
        s: &str,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        if s.is_empty() {
            // TODO: Is it OK? Rust disallows null-pointer even if the length is 0:
            // https://doc.rust-lang.org/std/slice/fn.from_raw_parts.html
            // the llrl currently does not do optimizations like enum layout optimization.
            llvm_constant!(*module, (nullptr (ptr u8)))
        } else {
            let init = llvm_constant!(*module, (str s));
            let var = module.add_global("", init.get_type(), None);
            var.set_linkage(llvm::Linkage::Internal);
            var.set_is_constant(true);
            var.set_initializer(Some(init));
            llvm_constant!(*module, (bit_cast (ptr u8) {var}))
        }
    }

    pub fn build_getptr<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_extract_value(a, 0)
    }

    pub fn build_getlen<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_extract_value(a, 1)
    }

    pub fn build_genid<'ctx: 'm, 'm>(
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let string_genid = ctx.library().llrt_string_genid();
        builder.build_call(string_genid, &[])
    }

    pub fn build_construct<'ctx: 'm, 'm>(
        ptr: impl LLVMAnyValue<'ctx, 'm>,
        len: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let ctx = ptr.context();
        let value = llvm_constant!(ctx, (undef (struct (ptr u8) u64)));
        let value = builder.build_insert_value(value, ptr, 0);
        builder.build_insert_value(value, len, 1)
    }

    pub fn build_eq<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        b: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let string_eq = ctx.library().llrt_string_eq();
        let ret = builder.build_call(string_eq, &[a.as_value(), b.as_value()]);
        builder.build_ne(ret, llvm_constant!(*ctx, (u32 0)))
    }

    pub fn build_cmp<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        b: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let string_cmp = ctx.library().llrt_string_cmp();
        builder.build_call(string_cmp, &[a.as_value(), b.as_value()])
    }

    pub fn build_concat<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        b: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let string_concat = ctx.library().llrt_string_concat();
        builder.build_call(string_concat, &[a.as_value(), b.as_value()])
    }
}

impl RtValue for RtString {
    type Native = String;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        self.as_str().to_string()
    }

    fn from_native(native: Self::Native) -> Self {
        unsafe {
            if native.is_empty() {
                RtString {
                    ptr: std::ptr::null(),
                    len: 0,
                }
            } else {
                let ptr = llrt::GC_malloc(native.len()) as *mut u8;
                std::ptr::copy_nonoverlapping(native.as_ptr(), ptr, native.len());
                RtString {
                    ptr: ptr as *const u8,
                    len: native.len() as u64,
                }
            }
        }
    }
}

impl RtConstant for RtString {
    type Src = str;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, (struct (ptr u8) u64)).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &str,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let body = Self::llvm_constant_body(src, module);
        llvm_constant!(*module, (struct {body} (u64 src.len()))).as_constant()
    }
}

impl RtExpand for RtString {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        let body = Self::llvm_constant_body(self.as_str(), module);
        buf[0] = llvm_constant!(*module, (ptr_to_int isize {body}));
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct RtChar(char);

impl RtChar {
    pub fn build_eq<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        b: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_eq(a, b)
    }
}

impl RtValue for RtChar {
    type Native = char;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        self.0
    }

    fn from_native(native: Self::Native) -> Self {
        Self(native)
    }
}

impl RtConstant for RtChar {
    type Src = char;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, u32).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &char,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        llvm_constant!(*module, (u32 {*src as u32})).as_constant()
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtArray {
    ptr: *const u8,
    len: u64,
}

impl RtArray {
    pub fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    pub fn llvm_type<'ctx>(elem_ty: impl LLVMAnyType<'ctx>) -> LLVMType<'ctx> {
        let ctx = elem_ty.context();
        llvm_type!(ctx, (struct (ptr {elem_ty}) u64)).as_type()
    }

    pub fn build_getptr<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_extract_value(a, 0)
    }

    pub fn build_getlen<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_extract_value(a, 1)
    }

    pub fn build_construct<'ctx: 'm, 'm>(
        ptr: impl LLVMAnyValue<'ctx, 'm>,
        len: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let ctx = ptr.context();
        let ptr_ty = ptr.get_type();
        let value = llvm_constant!(ctx, (undef (struct {ptr_ty} u64)));
        let value = builder.build_insert_value(value, ptr, 0);
        builder.build_insert_value(value, len, 1)
    }

    pub fn build_load<'ctx: 'm, 'm>(
        index: impl LLVMAnyValue<'ctx, 'm>,
        array: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let ptr = builder.build_extract_value(array, 0);
        let ptr = builder.build_gep(ptr, &[index.as_value()]);
        builder.build_load(ptr)
    }

    pub fn build_store<'ctx: 'm, 'm>(
        index: impl LLVMAnyValue<'ctx, 'm>,
        value: impl LLVMAnyValue<'ctx, 'm>,
        array: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) {
        let ptr = builder.build_extract_value(array, 0);
        let ptr = builder.build_gep(ptr, &[index.as_value()]);
        builder.build_store(value, ptr);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtCapturedUse {
    tag: u64,
    node_id: u64,
}

impl RtValue for RtCapturedUse {
    type Native = CapturedUse;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        // NOTE: This compatibility can be broken by Rust changes.
        unsafe { std::mem::transmute(self) }
    }

    fn from_native(native: Self::Native) -> Self {
        unsafe { std::mem::transmute(native) }
    }
}

impl RtConstant for RtCapturedUse {
    type Src = CapturedUse;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, (struct u64 u64)).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &CapturedUse,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let data = Self::from_native(*src);
        llvm_constant!(*module, (struct (u64 {data.tag}) (u64 {data.node_id}))).as_constant()
    }
}

#[repr(C)]
pub struct RtResult<T, E> {
    tag: u8,
    body: RtResultBody<T, E>,
}

impl<T, E> RtResult<T, E> {
    pub fn err(err: E) -> Self {
        let err = ManuallyDrop::new(err);
        Self {
            tag: 0,
            body: RtResultBody { err },
        }
    }

    pub fn ok(ok: T) -> Self {
        let ok = ManuallyDrop::new(ok);
        Self {
            tag: 1,
            body: RtResultBody { ok },
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

impl<T: RtValue, E: RtValue> RtValue for RtResult<T, E> {
    type Native = Result<T::Native, E::Native>;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        match self.into_inner() {
            Err(err) => Err(err.into_native()),
            Ok(ok) => Ok(ok.into_native()),
        }
    }

    fn from_native(native: Self::Native) -> Self {
        match native {
            Err(e) => RtResult::err(E::from_native(e)),
            Ok(t) => RtResult::ok(T::from_native(t)),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
union RtResultBody<T, E> {
    err: ManuallyDrop<E>, // 0
    ok: ManuallyDrop<T>,  // 1
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct RtSyntax<T>(*const RtSyntaxBuffer<T>);

impl RtSyntax<()> {
    pub fn size_align() -> (usize, usize) {
        (size_of::<*const u8>(), align_of::<*const u8>())
    }

    pub fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, (ptr u8)).as_type()
    }

    pub fn build_construct_syntax<'ctx: 'm, 'm>(
        metadata: SyntaxMetadata,
        body: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let body = body.as_value();
        let ty = llvm_type!(*ctx, (struct {RtSyntaxMetadata::llvm_type(ctx.context())} {body.get_type()}));
        let value = llvm_constant!(*ctx, (undef { ty }));
        let meta = RtSyntaxMetadata::llvm_constant(&metadata, ctx.module());
        let value = builder.build_insert_value(value, meta, 0);
        let value = builder.build_insert_value(value, body, 1);
        let ptr = build_heap_alloc(ty, builder, ctx);
        builder.build_store(value, ptr);
        builder.build_bit_cast(ptr, Self::llvm_type(ctx.context()))
    }

    pub fn build_syntax_body<'ctx: 'm, 'm>(
        body_ty: impl LLVMAnyType<'ctx>,
        value: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
        ctx: &impl BuildContext<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let ty = llvm_type!(*ctx, (struct {RtSyntaxMetadata::llvm_type(ctx.context())} {body_ty}));
        let value = builder.build_bit_cast(value, llvm_type!(*ctx, (ptr { ty })));
        let value = builder.build_struct_gep(value, 1);
        builder.build_load(value)
    }
}

impl<T: RtExpand> RtSyntax<T> {
    pub fn to_llvm_constant<'ctx: 'm, 'm>(
        &self,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let psize = size_of::<usize>();
        assert_eq!(psize, align_of::<RtSyntaxBuffer<T>>());
        let len = size_of::<RtSyntaxBuffer<T>>() / psize;
        let data = unsafe { std::slice::from_raw_parts(self.0 as *const usize, len) };

        let mut buf = (0..len)
            .map(|index| llvm_constant!(*module, (isize {data[index]})).as_constant())
            .collect::<Vec<_>>();

        // FIXME:
        // This implementation is too technical. If performance is not a consideration,
        // this implementation can be made unnecessary by building (Syntax Sexp) dynamically.
        unsafe { &*self.0 }.expand_on_buffer(&mut buf[..], module);

        let buf = LLVMConstantArray::get(llvm_type!(*module, isize), &buf);
        let var = module.add_global("", buf.get_type(), None);
        var.set_linkage(llvm::Linkage::Internal);
        var.set_is_constant(true);
        var.set_initializer(Some(buf));
        llvm_constant!(*module, (bit_cast (ptr u8) {var}))
    }
}

impl<T: RtValue + RtExpand> RtValue for RtSyntax<T>
where
    T::Native: SyntaxBody,
{
    type Native = Syntax<T::Native>;

    fn size_align() -> (usize, usize) {
        RtSyntax::size_align()
    }

    fn into_native(self) -> Self::Native {
        let buffer = unsafe { std::ptr::read(self.0) };
        let meta = buffer.metadata.into_native();
        let body = ManuallyDrop::into_inner(buffer.body).into_native();
        body.pack(meta)
    }

    fn from_native(native: Self::Native) -> Self {
        let (meta, body) = T::Native::unpack(native);
        let meta = RtSyntaxMetadata::from_native(meta);
        let body = T::from_native(body);
        let ptr =
            unsafe { llrt::GC_malloc(size_of::<RtSyntaxBuffer<T>>()) } as *mut RtSyntaxBuffer<T>;
        unsafe { std::ptr::write(ptr, RtSyntaxBuffer::new(meta, ManuallyDrop::new(body))) };
        Self(ptr as *const _)
    }
}

impl<T: RtValue + RtExpand> RtConstant for RtSyntax<T>
where
    T::Native: SyntaxBody,
    Syntax<T::Native>: Clone,
{
    type Src = Syntax<T::Native>;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        RtSyntax::llvm_type(ctx)
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &Self::Src,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        // TODO: Reduce allocation (We don't need to allocate memories for temporary RtSyntax)
        Self::from_native(src.clone()).to_llvm_constant(module)
    }
}

impl<T: RtExpand> RtExpand for RtSyntax<T> {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        buf[0] = llvm_constant!(*module, (ptr_to_int isize {self.to_llvm_constant(module)}));
    }
}

#[derive(Debug, Clone, Copy, new)]
#[repr(C)]
struct RtSyntaxBuffer<T> {
    metadata: RtSyntaxMetadata,
    body: ManuallyDrop<T>,
}

impl<T: RtExpand> RtExpand for RtSyntaxBuffer<T> {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.body
            .expand_on_buffer(&mut buf[buf_offset!(Self, body)..], module);
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RtSyntaxMetadata {
    ip: u32,
    ir: u32,
}

impl RtValue for RtSyntaxMetadata {
    type Native = SyntaxMetadata;

    fn size_align() -> (usize, usize) {
        (size_of::<Self>(), align_of::<Self>())
    }

    fn into_native(self) -> Self::Native {
        unsafe { std::mem::transmute(self) }
    }

    fn from_native(native: Self::Native) -> Self {
        unsafe { std::mem::transmute(native) }
    }
}

impl RtConstant for RtSyntaxMetadata {
    type Src = SyntaxMetadata;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, [u32; 2]).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &SyntaxMetadata,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let Self { ip, ir } = Self::from_native(*src);
        llvm_constant!(*module, [(u32 ip) (u32 ir)]).as_constant()
    }
}
