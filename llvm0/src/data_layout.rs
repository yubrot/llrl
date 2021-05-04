use super::*;
use llvm_sys::target::*;
use std::ffi::CString;
use std::fmt;

pub struct DataLayout {
    _refs: LLVMOpaqueTargetData,
}

impl DataLayout {
    pub fn new(string_rep: &str) -> Oo<Self> {
        let string_rep = CString::new(string_rep).unwrap();
        unsafe { Oo::from_ptr(LLVMCreateTargetData(string_rep.as_ptr())) }
    }

    pub fn string_rep(&self) -> String {
        unsafe { Message::from_ptr(LLVMCopyStringRepOfTargetData(self.as_ptr())) }
            .to_string_lossy()
            .into_owned()
    }

    pub fn byte_order(&self) -> ByteOrdering {
        unsafe { LLVMByteOrder(self.as_ptr()) }.into()
    }

    pub fn pointer_size(&self, address_space: Option<u32>) -> u32 {
        match address_space {
            None => unsafe { LLVMPointerSize(self.as_ptr()) },
            Some(a) => unsafe { LLVMPointerSizeForAS(self.as_ptr(), a) },
        }
    }

    pub fn int_ptr_type<'ctx>(
        &self,
        address_space: Option<u32>,
        context: &'ctx Context,
    ) -> IntegerType<'ctx> {
        match address_space {
            None => unsafe {
                IntegerType::from_ptr(LLVMIntPtrTypeInContext(context.as_ptr(), self.as_ptr()))
            },
            Some(a) => unsafe {
                IntegerType::from_ptr(LLVMIntPtrTypeForASInContext(
                    context.as_ptr(),
                    self.as_ptr(),
                    a,
                ))
            },
        }
    }

    pub fn type_size_in_bits<'ctx>(&self, ty: impl AnyType<'ctx>) -> u64 {
        unsafe { LLVMSizeOfTypeInBits(self.as_ptr(), ty.as_ptr()) }
    }

    pub fn type_store_size<'ctx>(&self, ty: impl AnyType<'ctx>) -> u64 {
        unsafe { LLVMStoreSizeOfType(self.as_ptr(), ty.as_ptr()) }
    }

    pub fn type_alloc_size<'ctx>(&self, ty: impl AnyType<'ctx>) -> u64 {
        unsafe { LLVMABISizeOfType(self.as_ptr(), ty.as_ptr()) }
    }

    pub fn abi_type_alignment<'ctx>(&self, ty: impl AnyType<'ctx>) -> u32 {
        unsafe { LLVMABIAlignmentOfType(self.as_ptr(), ty.as_ptr()) }
    }

    pub fn pref_type_alignment<'ctx>(&self, ty: impl AnyType<'ctx>) -> u32 {
        unsafe { LLVMPreferredAlignmentOfType(self.as_ptr(), ty.as_ptr()) }
    }

    pub fn preferred_alignment<'ctx: 'p, 'p>(&self, gv: GlobalVariable<'ctx, 'p>) -> u32 {
        unsafe { LLVMPreferredAlignmentOfGlobal(self.as_ptr(), gv.as_ptr()) }
    }

    pub fn element_containing_offset(&self, ty: StructType, offset: u64) -> u32 {
        unsafe { LLVMElementAtOffset(self.as_ptr(), ty.as_ptr(), offset) }
    }

    pub fn element_offset(&self, ty: StructType, element: u32) -> u64 {
        unsafe { LLVMOffsetOfElement(self.as_ptr(), ty.as_ptr(), element) }
    }
}

// TODO: implement Eq: this should be done by comparing string_rep?

impl fmt::Debug for DataLayout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DataLayout::new({:?})", &self.string_rep())
    }
}

impl fmt::Display for DataLayout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.string_rep(), f)
    }
}

unsafe_impl_opaque!(DataLayout(LLVMOpaqueTargetData));

impl OpaqueOwn for DataLayout {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposeTargetData(a);
    }
}

impl ToOwned for DataLayout {
    type Owned = Oo<Self>;

    fn to_owned(&self) -> Self::Owned {
        Self::new(&self.string_rep())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn query() {
        let dl = DataLayout::new("e-m:e-i64:64-f80:128-n8:16:32:64-S128");
        assert_eq!(&dl.string_rep(), "e-m:e-i64:64-f80:128-n8:16:32:64-S128");
        assert_eq!(dl.byte_order(), ByteOrdering::LittleEndian);
        assert_eq!(dl.pointer_size(None), 8u32);
        assert_eq!(dl.pointer_size(Some(4)), 8u32);

        let ctx = Context::new();
        let ty = llvm_type!(&ctx, (struct i32 i16 i8));
        assert_eq!(dl.type_size_in_bits(ty), 64u64);
        assert_eq!(dl.type_store_size(ty), 8u64);
        assert_eq!(dl.type_alloc_size(ty), 8u64);
        assert_eq!(dl.abi_type_alignment(ty), 4u32);
        assert_eq!(dl.element_containing_offset(ty, 0), 0u32);
        assert_eq!(dl.element_containing_offset(ty, 1), 0u32);
        assert_eq!(dl.element_containing_offset(ty, 4), 1u32);
        assert_eq!(dl.element_offset(ty, 1), 4u64);
        assert_eq!(dl.element_offset(ty, 2), 6u64);

        let ty = dl.int_ptr_type(None, &ctx);
        assert_eq!(ty.bit_width(), 64);
    }
}
