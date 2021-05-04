use super::*;
use llvm_sys::transforms::pass_manager_builder::*;
use std::cmp::min;

pub struct PassManagerBuilder {
    _refs: LLVMOpaquePassManagerBuilder,
}

impl PassManagerBuilder {
    pub fn new() -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMPassManagerBuilderCreate()) }
    }

    pub fn set_opt_level(&self, opt_level: u32) {
        let opt_level = min(opt_level, 3);
        unsafe { LLVMPassManagerBuilderSetOptLevel(self.as_ptr(), opt_level) };
    }

    pub fn set_size_level(&self, size_level: u32) {
        let size_level = min(size_level, 2);
        unsafe { LLVMPassManagerBuilderSetSizeLevel(self.as_ptr(), size_level) };
    }

    pub fn set_disable_unroll_loops(&self, value: bool) {
        let value = if value { 1 } else { 0 };
        unsafe { LLVMPassManagerBuilderSetDisableUnrollLoops(self.as_ptr(), value) };
    }

    pub fn use_inliner_with_threshold(&self, threshold: u32) {
        unsafe { LLVMPassManagerBuilderUseInlinerWithThreshold(self.as_ptr(), threshold) };
    }

    pub fn populate_pass_manager(&self, pm: &PassManager) {
        unsafe { LLVMPassManagerBuilderPopulateModulePassManager(self.as_ptr(), pm.as_ptr()) };
    }

    pub fn populate_function_pass_manager(&self, fpm: &FunctionPassManager) {
        unsafe { LLVMPassManagerBuilderPopulateFunctionPassManager(self.as_ptr(), fpm.as_ptr()) };
    }
}

impl_opaque_eq!(PassManagerBuilder);

impl_opaque_debug!(PassManagerBuilder);

unsafe_impl_opaque!(PassManagerBuilder(LLVMOpaquePassManagerBuilder));

impl OpaqueOwn for PassManagerBuilder {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMPassManagerBuilderDispose(a);
    }
}
