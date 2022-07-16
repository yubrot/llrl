use super::{FunctionSymbol, FunctionSymbolKind};
use crate::backend::native::{native_macro, native_main};
use crate::lowering::ir::*;
use llvm::prelude::*;
use std::sync::Once;

#[derive(Debug)]
pub struct Executor<'ctx> {
    engine: LLVMExecutionEngine<'ctx>,
}

impl<'ctx> Executor<'ctx> {
    pub fn new(
        context: &'ctx LLVMContext,
        opt_level: Option<llvm::OptLevel>,
        code_model: Option<llvm::CodeModel>,
    ) -> Self {
        static INIT_EXECUTION_CONTEXT: Once = Once::new();

        INIT_EXECUTION_CONTEXT.call_once(|| unsafe {
            // NOTE: Only native targets are supported. For more details, see
            // assert_data_layout_matches_native_environment
            llvm::initialize_native_target().expect("Failed to initialize native target");
            llvm::initialize_native_asm_printer().expect("Failed to initialize native ASM printer");
            LLVMExecutionEngine::link_in_mcjit();
        });

        let engine = LLVMExecutionEngine::new_mcjit(context, opt_level, code_model, None, None)
            .unwrap_or_else(|e| panic!("Failed to create execution engine: {}", e));

        assert_data_layout_matches_native_environment(engine.data_layout());

        Self { engine }
    }

    pub fn data_layout(&self) -> &LLVMDataLayout {
        self.engine.data_layout()
    }

    pub fn target_machine(&self) -> &LLVMTargetMachine {
        self.engine
            .target_machine()
            .expect("Failed to get TargetMachine")
    }

    pub fn add_module(&mut self, module: LLVMBox<LLVMModule<'ctx>>) {
        self.engine.add_module(module);
    }

    pub fn remove_modules(&mut self) -> Vec<LLVMBox<LLVMModule<'ctx>>> {
        self.engine.remove_modules()
    }

    pub fn call_macro(&self, f: &FunctionSymbol, s: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        assert!(
            matches!(f.kind, FunctionSymbolKind::Macro),
            "Function kind mismatch: expected macro but got {:?}",
            f.kind
        );
        let address = self.engine.get_function_address(&f.name) as usize;
        assert!(address != 0, "Function not found: {}", f.name);
        let handler = unsafe { native_macro(address as _) };
        handler(s)
    }

    pub fn call_main(&self, f: &FunctionSymbol) -> bool {
        assert!(
            matches!(f.kind, FunctionSymbolKind::Main(Ct::U(1))),
            "Function kind mismatch: expected main(U1) but got {:?}",
            f.kind
        );
        let address = self.engine.get_function_address(&f.name) as usize;
        assert!(address != 0, "Function not found: {}", f.name);
        let handler = unsafe { native_main(address as _) };
        handler()
    }
}

pub fn assert_data_layout_matches_native_environment(dl: &LLVMDataLayout) {
    // NOTE: llrl assumes that the native environment (i.e., at the time of executing
    // the JIT compiled function for macro expansion) and the target environment are the same.
    // The entire implementation of the LLVM backend is based on this assumption, so it is
    // likely that major changes will be required if we wish to support cross-compilation.
    use std::mem::{align_of, size_of};
    let ctx = LLVMContext::new();

    let ty = llvm_type!(&ctx, i1);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<bool>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<bool>());

    let ty = llvm_type!(&ctx, i8);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i8>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i8>());

    let ty = llvm_type!(&ctx, i16);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i16>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i16>());

    let ty = llvm_type!(&ctx, i32);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i32>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i32>());

    let ty = llvm_type!(&ctx, i64);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i64>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i64>());

    let ty = llvm_type!(&ctx, (ptr i8));
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<*const i8>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<*const i8>());
    assert_eq!(dl.pointer_size(None) as usize, size_of::<*const i8>());
}
