use super::*;
use llvm_sys::execution_engine::*;
use std::ffi::c_void;

pub struct GenericValue {
    _refs: LLVMOpaqueGenericValue,
}

impl GenericValue {
    pub fn of_int(ty: IntegerType<'_>, n: u64, signed: bool) -> Oo<Self> {
        unsafe {
            Oo::from_ptr(LLVMCreateGenericValueOfInt(
                ty.as_ptr(),
                n,
                if signed { 1 } else { 0 },
            ))
        }
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn of_pointer(v: *mut c_void) -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMCreateGenericValueOfPointer(v)) }
    }

    pub fn of_float(ty: FPType<'_>, v: f64) -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMCreateGenericValueOfFloat(ty.as_ptr(), v)) }
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn as_int(&self, signed: bool) -> u64 {
        LLVMGenericValueToInt(self.as_ptr(), if signed { 1 } else { 0 })
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn as_pointer(&self) -> *mut c_void {
        LLVMGenericValueToPointer(self.as_ptr())
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn as_float(&self, ty: FPType<'_>) -> f64 {
        LLVMGenericValueToFloat(ty.as_ptr(), self.as_ptr())
    }
}

impl_opaque_debug!(GenericValue);

unsafe_impl_opaque!(GenericValue(LLVMOpaqueGenericValue));

impl OpaqueOwn for GenericValue {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposeGenericValue(a);
    }
}
