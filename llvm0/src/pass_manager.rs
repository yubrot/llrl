use super::*;
use llvm_sys::core::*;
use llvm_sys::*;
use std::marker::PhantomData;

pub struct PassManager {
    _refs: LLVMPassManager,
}

impl PassManager {
    pub fn new() -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMCreatePassManager()) }
    }

    pub fn run(&self, module: &Module) {
        unsafe { LLVMRunPassManager(self.as_ptr(), module.as_ptr()) };
    }
}

impl_opaque_eq!(PassManager);

impl_opaque_debug!(PassManager);

unsafe_impl_opaque!(PassManager(LLVMPassManager));

impl OpaqueOwn for PassManager {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposePassManager(a);
    }
}

pub struct FunctionPassManager<'ctx, 'm> {
    _refs: LLVMPassManager,
    _module: PhantomData<&'m Module<'ctx>>,
}

impl<'ctx, 'm> FunctionPassManager<'ctx, 'm> {
    pub fn new(module: &'m Module<'ctx>) -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMCreateFunctionPassManagerForModule(module.as_ptr())) }
    }

    pub fn run<'p>(&self, functions: impl IntoIterator<Item = Function<'ctx, 'p>>)
    where
        'm: 'p,
    {
        unsafe { LLVMInitializeFunctionPassManager(self.as_ptr()) };
        for function in functions {
            unsafe { LLVMRunFunctionPassManager(self.as_ptr(), function.as_ptr()) };
        }
        unsafe { LLVMFinalizeFunctionPassManager(self.as_ptr()) };
    }
}

impl_opaque_eq!(['ctx, 'm] FunctionPassManager);

impl_opaque_debug!(['ctx, 'm] FunctionPassManager);

unsafe_impl_opaque!(['ctx, 'm] FunctionPassManager(LLVMPassManager));

impl<'ctx, 'm> OpaqueOwn for FunctionPassManager<'ctx, 'm> {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposePassManager(a);
    }
}
