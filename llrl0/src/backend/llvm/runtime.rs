use crate::emitter::ir::{CapturedUse, Syntax, SyntaxBody, SyntaxMetadata};
use llvm::prelude::*;
use memoffset::offset_of;
use once_cell::unsync::OnceCell;
use std::mem::{align_of, size_of};
use std::sync::Once;

macro_rules! buf_offset {
    ($parent:path, $field:tt) => {
        offset_of!($parent, $field) / size_of::<usize>()
    };
}

pub use crate::backend::ee::*;

/// Types that can be represented as a constant on LLVM-IR.
pub trait RtConstant: EeValue {
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

pub trait BuildContext<'ctx: 'm, 'm>: LLVMTypeBuilder<'ctx> {
    fn module(&self) -> &'m LLVMModule<'ctx>;
    fn library(&self) -> &RtLibrary<'ctx, 'm>;
}

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

impl EeString {
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

impl RtConstant for EeString {
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

impl RtExpand for EeString {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        let body = Self::llvm_constant_body(self.as_str(), module);
        buf[0] = llvm_constant!(*module, (ptr_to_int isize {body}));
    }
}

impl EeChar {
    pub fn build_eq<'ctx: 'm, 'm>(
        a: impl LLVMAnyValue<'ctx, 'm>,
        b: impl LLVMAnyValue<'ctx, 'm>,
        builder: &LLVMBuilder<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        builder.build_eq(a, b)
    }
}

impl RtConstant for EeChar {
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

impl EeArray<()> {
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

impl RtConstant for EeCapturedUse {
    type Src = CapturedUse;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, (struct u64 u64)).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &CapturedUse,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let data = Self::from_host(*src);
        llvm_constant!(*module, (struct (u64 {data.tag}) (u64 {data.node_id}))).as_constant()
    }
}

impl EeSyntax<()> {
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
        let ty = llvm_type!(*ctx, (struct {EeSyntaxMetadata::llvm_type(ctx.context())} {body.get_type()}));
        let value = llvm_constant!(*ctx, (undef { ty }));
        let meta = EeSyntaxMetadata::llvm_constant(&metadata, ctx.module());
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
        let ty = llvm_type!(*ctx, (struct {EeSyntaxMetadata::llvm_type(ctx.context())} {body_ty}));
        let value = builder.build_bit_cast(value, llvm_type!(*ctx, (ptr { ty })));
        let value = builder.build_struct_gep(value, 1);
        builder.build_load(value)
    }
}

impl<T: RtExpand> EeSyntax<T> {
    pub fn to_llvm_constant<'ctx: 'm, 'm>(
        &self,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let psize = size_of::<usize>();
        assert_eq!(psize, align_of::<EeSyntaxBuffer<T>>());
        let len = size_of::<EeSyntaxBuffer<T>>() / psize;
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

impl<T: EeValue + RtExpand> RtConstant for EeSyntax<T>
where
    T::HostValue: SyntaxBody,
    Syntax<T::HostValue>: Clone,
{
    type Src = Syntax<T::HostValue>;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        EeSyntax::llvm_type(ctx)
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &Self::Src,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        // TODO: Reduce allocation (We don't need to allocate memories for temporary RtSyntax)
        Self::from_host(src.clone()).to_llvm_constant(module)
    }
}

impl<T: RtExpand> RtExpand for EeSyntax<T> {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        buf[0] = llvm_constant!(*module, (ptr_to_int isize {self.to_llvm_constant(module)}));
    }
}

impl<T: RtExpand> RtExpand for EeSyntaxBuffer<T> {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.body
            .expand_on_buffer(&mut buf[buf_offset!(Self, body)..], module);
    }
}

impl RtConstant for EeSyntaxMetadata {
    type Src = SyntaxMetadata;

    fn llvm_type<'ctx>(ctx: &'ctx LLVMContext) -> LLVMType<'ctx> {
        llvm_type!(ctx, [u32; 2]).as_type()
    }

    fn llvm_constant<'ctx: 'm, 'm>(
        src: &SyntaxMetadata,
        module: &'m LLVMModule<'ctx>,
    ) -> LLVMConstant<'ctx, 'm> {
        let Self { ip, ir } = Self::from_host(*src);
        llvm_constant!(*module, [(u32 ip) (u32 ir)]).as_constant()
    }
}

impl RtExpand for EeSexp {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        let o = buf_offset!(Self, body);
        match self.into_view() {
            EeSexpView::Symbol(s) => s.expand_on_buffer(&mut buf[o..], module),
            EeSexpView::String(s) => s.expand_on_buffer(&mut buf[o..], module),
            EeSexpView::Cons(c) => c.expand_on_buffer(&mut buf[o..], module),
            _ => {}
        }
    }
}

impl RtExpand for EeSexpSymbol {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.value.expand_on_buffer(buf, module);
    }
}

impl RtExpand for EeSexpString {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.value.expand_on_buffer(buf, module);
    }
}

impl RtExpand for EeSexpCons {
    fn expand_on_buffer<'ctx: 'm, 'm>(
        &self,
        buf: &mut [LLVMConstant<'ctx, 'm>],
        module: &'m LLVMModule<'ctx>,
    ) {
        self.car.expand_on_buffer(buf, module);
        self.cdr
            .expand_on_buffer(&mut buf[buf_offset!(Self, cdr)..], module);
    }
}

#[derive(Debug)]
pub struct RtLibrary<'ctx: 'm, 'm> {
    module: &'m LLVMModule<'ctx>,
    gc_malloc: OnceCell<LLVMFunction<'ctx, 'm>>,
    panic: OnceCell<LLVMFunction<'ctx, 'm>>,
    string_genid: OnceCell<LLVMFunction<'ctx, 'm>>,
    string_eq: OnceCell<LLVMFunction<'ctx, 'm>>,
    string_cmp: OnceCell<LLVMFunction<'ctx, 'm>>,
    string_concat: OnceCell<LLVMFunction<'ctx, 'm>>,
}

impl<'ctx: 'm, 'm> RtLibrary<'ctx, 'm> {
    pub fn new(module: &'m LLVMModule<'ctx>) -> Self {
        static INIT_LIBRARY: Once = Once::new();

        INIT_LIBRARY.call_once(|| unsafe {
            use llrt::*;
            llvm::add_symbol("llrt_init", llrt_init as *mut ());
            llvm::add_symbol("llrt_args", llrt_args as *mut ());
            llvm::add_symbol("llrt_panic", llrt_panic as *mut ());
            llvm::add_symbol("llrt_exit", llrt_exit as *mut ());
            llvm::add_symbol("llrt_spawn_process", llrt_spawn_process as *mut ());
            llvm::add_symbol("llrt_execute_process", llrt_execute_process as *mut ());
            llvm::add_symbol("llrt_wait", llrt_wait as *mut ());
            llvm::add_symbol("llrt_time", llrt_time as *mut ());
            llvm::add_symbol("llrt_getcwd", llrt_getcwd as *mut ());
            llvm::add_symbol("llrt_string_genid", llrt_string_genid as *mut ());
            llvm::add_symbol("llrt_string_eq", llrt_string_eq as *mut ());
            llvm::add_symbol("llrt_string_cmp", llrt_string_cmp as *mut ());
            llvm::add_symbol("llrt_string_concat", llrt_string_concat as *mut ());
            llvm::add_symbol("llrt_f32_to_string", llrt_f32_to_string as *mut ());
            llvm::add_symbol("llrt_f64_to_string", llrt_f64_to_string as *mut ());
            llvm::add_symbol("llrt_i64_to_string", llrt_i64_to_string as *mut ());
            llvm::add_symbol("llrt_u64_to_string", llrt_u64_to_string as *mut ());
            llvm::add_symbol("llrt_string_to_i64", llrt_string_to_i64 as *mut ());
            llvm::add_symbol("llrt_string_to_u64", llrt_string_to_u64 as *mut ());
            llvm::add_symbol("llrt_string_to_f32", llrt_string_to_f32 as *mut ());
            llvm::add_symbol("llrt_string_to_f64", llrt_string_to_f64 as *mut ());
            llvm::add_symbol("llrt_readdir", llrt_readdir as *mut ());
            llvm::add_symbol("llrt_stdin", llrt_stdin as *mut ());
            llvm::add_symbol("llrt_stdout", llrt_stdout as *mut ());
            llvm::add_symbol("llrt_stderr", llrt_stderr as *mut ());
            llvm::add_symbol("llrt_current_errno", llrt_current_errno as *mut ());
            llvm::add_symbol("llrt_xxh_seed", llrt_xxh_seed as *mut ());
        });

        Self {
            module,
            gc_malloc: OnceCell::new(),
            panic: OnceCell::new(),
            string_genid: OnceCell::new(),
            string_eq: OnceCell::new(),
            string_cmp: OnceCell::new(),
            string_concat: OnceCell::new(),
        }
    }

    pub fn gc_malloc(&self) -> LLVMFunction<'ctx, 'm> {
        *self.gc_malloc.get_or_init(|| {
            // TODO: The return value should be noalias
            self.module
                .add_function("GC_malloc", llvm_type!(*self, (function(isize) (ptr u8))))
        })
    }

    pub fn llrt_panic(&self) -> LLVMFunction<'ctx, 'm> {
        *self.panic.get_or_init(|| {
            // TODO: put noreturn attribute
            self.module.add_function(
                "llrt_panic",
                llvm_type!(*self, (function((struct (ptr u8) u64)) void)),
            )
        })
    }

    pub fn llrt_string_genid(&self) -> LLVMFunction<'ctx, 'm> {
        *self.string_genid.get_or_init(|| {
            self.module.add_function(
                "llrt_string_genid",
                llvm_type!(*self, (function() (struct (ptr u8) u64))),
            )
        })
    }

    pub fn llrt_string_eq(&self) -> LLVMFunction<'ctx, 'm> {
        *self.string_eq.get_or_init(|| {
            self.module.add_function(
                "llrt_string_eq",
                llvm_type!(*self, (function((struct (ptr u8) u64) (struct (ptr u8) u64)) i32)),
            )
        })
    }

    pub fn llrt_string_cmp(&self) -> LLVMFunction<'ctx, 'm> {
        *self.string_cmp.get_or_init(|| {
            self.module.add_function(
                "llrt_string_cmp",
                llvm_type!(*self, (function((struct (ptr u8) u64) (struct (ptr u8) u64)) i32)),
            )
        })
    }

    pub fn llrt_string_concat(&self) -> LLVMFunction<'ctx, 'm> {
        *self.string_concat.get_or_init(|| {
            self.module.add_function(
                "llrt_string_concat",
                llvm_type!(*self, (function((struct (ptr u8) u64) (struct (ptr u8) u64)) (struct (ptr u8) u64))),
            )
        })
    }
}

impl<'ctx: 'm, 'm> LLVMTypeBuilder<'ctx> for RtLibrary<'ctx, 'm> {
    fn context(&self) -> &'ctx LLVMContext {
        self.module.context()
    }
}
