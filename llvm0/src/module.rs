use super::*;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::target::*;
use llvm_sys::*;
use std::borrow::ToOwned;
use std::ffi::{CStr, CString};
use std::fmt;
use std::marker::PhantomData;

pub struct Module<'ctx> {
    _refs: LLVMModule,
    _context: PhantomData<&'ctx Context>,
}

impl<'ctx> Module<'ctx> {
    pub fn new(name: &str, context: &'ctx Context) -> Oo<Self> {
        let module_name = CString::new(name).unwrap();
        unsafe {
            Oo::from_ptr(LLVMModuleCreateWithNameInContext(
                module_name.as_ptr(),
                context.as_ptr(),
            ))
        }
    }

    pub fn context(&self) -> &'ctx Context {
        unsafe { Context::from_ptr(LLVMGetModuleContext(self.as_ptr())) }
    }

    pub fn data_layout(&self) -> &DataLayout {
        unsafe { DataLayout::from_ptr(LLVMGetModuleDataLayout(self.as_ptr())) }
    }

    pub fn set_data_layout(&self, dl: &DataLayout) {
        unsafe { LLVMSetModuleDataLayout(self.as_ptr(), dl.as_ptr()) };
    }

    pub fn target_triple(&self) -> &CStr {
        unsafe { CStr::from_ptr(LLVMGetTarget(self.as_ptr())) }
    }

    pub fn set_target_triple(&self, triple: &CStr) {
        unsafe { LLVMSetTarget(self.as_ptr(), triple.as_ptr()) };
    }

    pub fn add_global<'m>(
        &'m self,
        name: &str,
        ty: impl AnyType<'ctx>,
        address_space: Option<u32>,
    ) -> GlobalVariable<'ctx, 'm> {
        let name = CString::new(name).unwrap();
        let gv = match address_space {
            Some(a) => unsafe {
                LLVMAddGlobalInAddressSpace(self.as_ptr(), ty.as_ptr(), name.as_ptr(), a)
            },
            None => unsafe { LLVMAddGlobal(self.as_ptr(), ty.as_ptr(), name.as_ptr()) },
        };
        unsafe { GlobalVariable::from_ptr(gv) }
    }

    pub fn lookup_global<'m>(&'m self, name: &str) -> Option<GlobalVariable<'ctx, 'm>> {
        let name = CString::new(name).unwrap();
        let gv = unsafe { LLVMGetNamedGlobal(self.as_ptr(), name.as_ptr()) };
        if gv.is_null() {
            None
        } else {
            Some(unsafe { GlobalVariable::from_ptr(gv) })
        }
    }

    pub fn globals<'m>(&'m self) -> Vec<GlobalVariable<'ctx, 'm>> {
        let mut r = Vec::new();
        let mut t = unsafe { LLVMGetFirstGlobal(self.as_ptr()) };
        while !t.is_null() {
            r.push(unsafe { GlobalVariable::from_ptr(t) });
            t = unsafe { LLVMGetNextGlobal(t) };
        }
        r
    }

    pub fn add_function<'m>(&'m self, name: &str, ty: FunctionType<'ctx>) -> Function<'ctx, 'm> {
        let name = CString::new(name).unwrap();
        unsafe { Function::from_ptr(LLVMAddFunction(self.as_ptr(), name.as_ptr(), ty.as_ptr())) }
    }

    pub fn lookup_function<'m>(&'m self, name: &str) -> Option<Function<'ctx, 'm>> {
        let name = CString::new(name).unwrap();
        let f = unsafe { LLVMGetNamedFunction(self.as_ptr(), name.as_ptr()) };
        if f.is_null() {
            None
        } else {
            Some(unsafe { Function::from_ptr(f) })
        }
    }

    pub fn functions<'m>(&'m self) -> Vec<Function<'ctx, 'm>> {
        let mut r = Vec::new();
        let mut t = unsafe { LLVMGetFirstFunction(self.as_ptr()) };
        while !t.is_null() {
            r.push(unsafe { Function::from_ptr(t) });
            t = unsafe { LLVMGetNextFunction(t) };
        }
        r
    }

    pub fn print(&self) -> Message {
        unsafe { Message::from_ptr(LLVMPrintModuleToString(self.as_ptr())) }
    }

    pub fn verify(&self) -> Result<(), Message> {
        let mut out = std::mem::MaybeUninit::uninit();
        match unsafe {
            LLVMVerifyModule(
                self.as_ptr(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
                out.as_mut_ptr(),
            )
        } {
            1 => Err(unsafe { Message::from_ptr(out.assume_init()) }),
            _ => Ok(()),
        }
    }
}

impl_opaque_eq!(['ctx] Module);

impl_opaque_debug!(['ctx] Module);

impl<'ctx> fmt::Display for Module<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.print().to_string_lossy(), f)
    }
}

unsafe_impl_opaque!(['ctx] Module(LLVMModule));

impl<'ctx> OpaqueOwn for Module<'ctx> {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposeModule(a);
    }
}

impl<'ctx> ToOwned for Module<'ctx> {
    type Owned = Oo<Self>;

    fn to_owned(&self) -> Self::Owned {
        unsafe { Oo::from_ptr(LLVMCloneModule(self.as_ptr())) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attributes() {
        let ctx = Context::new();
        let module = Module::new("name", &ctx);

        let dl = DataLayout::new("e-m:e-i64:64-f80:128-n8:16:32:64-S128");
        module.set_data_layout(&dl);
        assert_eq!(module.data_layout().string_rep(), dl.string_rep());

        // TODO: target triple

        // global variables and functions are tested at value.rs

        // Verifying an empty module is OK:
        assert!(module.verify().is_ok());

        // Verifying a module that has an incomplete function is ERROR:
        let f = module.add_function("test", llvm_type!(&ctx, (function() i32)));
        f.append_block("block");
        assert!(module.verify().is_err());
    }
}
