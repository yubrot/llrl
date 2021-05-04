use super::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::borrow::Cow;
use std::ffi::CStr;
use std::fmt;
use std::marker::PhantomData;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct BasicBlock<'ctx, 'p> {
    inner: LLVMBasicBlockRef,
    parent: PhantomData<Function<'ctx, 'p>>,
}

impl<'ctx: 'p, 'p> BasicBlock<'ctx, 'p> {
    pub fn as_ptr(self) -> LLVMBasicBlockRef {
        self.inner
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn from_ptr(bb_ref: LLVMBasicBlockRef) -> Self {
        BasicBlock {
            inner: bb_ref,
            parent: PhantomData,
        }
    }

    pub fn context(self) -> &'ctx Context {
        self.parent().context()
    }

    pub fn parent(self) -> Function<'ctx, 'p> {
        // Since we don't provide an interface to `LLVMRemoveBasicBlockFromParent`,
        // every `BasicBlock` must have a parent function.
        unsafe { Function::from_ptr(LLVMGetBasicBlockParent(self.as_ptr())) }
    }

    pub fn name(self) -> Cow<'p, str> {
        unsafe { CStr::from_ptr(LLVMGetBasicBlockName(self.as_ptr())) }.to_string_lossy()
    }

    /// Remove this basic block from its parent `Function`.
    ///
    /// # Safety
    /// `erase_from_parent` is marked as unsafe because `BasicBlock` is Copy
    /// and is not tracked by Rust's ownership model. By removing `BasicBlock`,
    /// the caller must ensure that the removed `BasicBlock` and its children
    /// are not used anymore.
    pub unsafe fn erase_from_parent(self) {
        LLVMDeleteBasicBlock(self.as_ptr());
    }
}

impl<'ctx: 'p, 'p> fmt::Debug for BasicBlock<'ctx, 'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BasicBlock::from_ptr({:?})", self.as_ptr())
    }
}

impl<'ctx: 'p, 'p> fmt::Display for BasicBlock<'ctx, 'p> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.name())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name() {
        let ctx = Context::new();
        let module = Module::new("m", &ctx);
        let function = module.add_function("f", llvm_type!(&ctx, (function() i32)));
        let bb = function.append_block("test");

        assert_eq!(bb.name(), "test");
    }

    #[test]
    fn parent() {
        let ctx = Context::new();
        let module = Module::new("m", &ctx);
        let function = module.add_function("f", llvm_type!(&ctx, (function() i32)));
        let bb = function.append_block("test");

        assert_eq!(bb.parent(), function);
    }

    #[test]
    fn erase() {
        let ctx = Context::new();
        let module = Module::new("m", &ctx);
        let function = module.add_function("f", llvm_type!(&ctx, (function() i32)));
        let bb1 = function.append_block("test1");
        let bb2 = function.append_block("test2");

        assert_eq!(function.basic_blocks().len(), 2);
        unsafe { bb2.erase_from_parent() }; // bb2 is no longer available
        assert_eq!(function.basic_blocks().len(), 1);
        assert_eq!(function.basic_blocks()[0], bb1);
    }
}
