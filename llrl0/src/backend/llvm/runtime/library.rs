use llvm::prelude::*;
use once_cell::unsync::OnceCell;
use std::sync::Once;

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
