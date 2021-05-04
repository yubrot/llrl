use super::*;
use llvm_sys::core::*;
use llvm_sys::*;

pub struct Context {
    _refs: LLVMContext,
}

impl Context {
    pub fn new() -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMContextCreate()) }
    }

    /// Obtain the global context provided by LLVM.
    ///
    /// # Safety
    /// This is marked as unsafe because the global context is not thread safe.
    pub unsafe fn global() -> &'static Self {
        Opaque::from_ptr(LLVMGetGlobalContext())
    }
}

impl_opaque_eq!(Context);

impl_opaque_debug!(Context);

unsafe_impl_opaque!(Context(LLVMContext));

impl OpaqueOwn for Context {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMContextDispose(a);
    }
}
