use super::*;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::fmt;
use std::marker::PhantomData;
use std::os::raw::c_char;

pub trait AnyValue<'ctx: 'p, 'p>: Copy {
    fn as_ptr(self) -> LLVMValueRef;

    #[allow(clippy::missing_safety_doc)]
    unsafe fn from_ptr(value_ref: LLVMValueRef) -> Self;

    #[allow(clippy::missing_safety_doc)]
    unsafe fn match_ptr(value_ref: LLVMValueRef) -> bool;

    fn context(self) -> &'ctx Context {
        self.get_type().context()
    }

    fn get_type(self) -> Type<'ctx> {
        unsafe { Type::from_ptr(LLVMTypeOf(self.as_ptr())) }
    }

    fn as_value(self) -> Value<'ctx, 'p> {
        unsafe { Value::from_ptr(self.as_ptr()) }
    }

    fn is_value_of<T: AnyValue<'ctx, 'p>>(self) -> bool {
        unsafe { T::match_ptr(self.as_ptr()) }
    }

    fn as_value_of<T: AnyValue<'ctx, 'p>>(self) -> Option<T> {
        if self.is_value_of::<T>() {
            Some(unsafe { T::from_ptr(self.as_ptr()) })
        } else {
            None
        }
    }

    fn name(self) -> Cow<'p, str> {
        let mut length = std::mem::MaybeUninit::uninit();
        let bytes = unsafe { LLVMGetValueName2(self.as_ptr(), length.as_mut_ptr()) };
        unsafe {
            String::from_utf8_lossy(std::slice::from_raw_parts(
                bytes as *const u8,
                length.assume_init(),
            ))
        }
    }

    fn set_name(self, name: &str) {
        let name = name.as_bytes();
        unsafe { LLVMSetValueName2(self.as_ptr(), name.as_ptr() as *const i8, name.len()) }
    }

    fn is_constant(self) -> bool {
        (unsafe { LLVMIsConstant(self.as_ptr()) }) != 0
    }

    fn is_null(self) -> bool {
        (unsafe { LLVMIsNull(self.as_ptr()) }) != 0
    }

    fn is_undef(self) -> bool {
        (unsafe { LLVMIsUndef(self.as_ptr()) }) != 0
    }

    fn print(self) -> Message {
        unsafe { Message::from_ptr(LLVMPrintValueToString(self.as_ptr())) }
    }
}

macro_rules! impl_debug_and_display_for_value {
    ($name:ident) => {
        impl<'ctx: 'p, 'p> fmt::Debug for $name<'ctx, 'p> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}::from_ptr({:?})",
                    std::stringify!($name),
                    self.as_ptr()
                )
            }
        }

        impl<'ctx: 'p, 'p> fmt::Display for $name<'ctx, 'p> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(&self.print().to_string_lossy(), f)
            }
        }
    };
}

pub unsafe trait AnyConstant<'ctx: 'p, 'p>: AnyValue<'ctx, 'p> {
    fn as_constant(self) -> Constant<'ctx, 'p> {
        unsafe { Constant::from_ptr(self.as_ptr()) }
    }

    fn bit_cast(self, ty: impl AnyType<'ctx>) -> Constant<'ctx, 'p> {
        unsafe { Constant::from_ptr(LLVMConstBitCast(self.as_ptr(), ty.as_ptr())) }
    }

    fn trunc_or_bit_cast(self, ty: impl AnyType<'ctx>) -> Constant<'ctx, 'p> {
        unsafe { Constant::from_ptr(LLVMConstTruncOrBitCast(self.as_ptr(), ty.as_ptr())) }
    }

    fn int_to_ptr(self, ty: impl AnyType<'ctx>) -> Constant<'ctx, 'p> {
        unsafe { Constant::from_ptr(LLVMConstIntToPtr(self.as_ptr(), ty.as_ptr())) }
    }

    fn ptr_to_int(self, ty: impl AnyType<'ctx>) -> Constant<'ctx, 'p> {
        unsafe { Constant::from_ptr(LLVMConstPtrToInt(self.as_ptr(), ty.as_ptr())) }
    }
}

pub unsafe trait AnyGlobalValue<'ctx: 'p, 'p>: AnyConstant<'ctx, 'p> {
    fn is_declaration(self) -> bool {
        (unsafe { LLVMIsDeclaration(self.as_ptr()) }) != 0
    }

    fn linkage(self) -> Linkage {
        unsafe { LLVMGetLinkage(self.as_ptr()) }.into()
    }

    fn set_linkage(self, linkage: Linkage) {
        unsafe { LLVMSetLinkage(self.as_ptr(), linkage.into()) };
    }

    fn section(self) -> Cow<'p, str> {
        unsafe { CStr::from_ptr(LLVMGetSection(self.as_ptr())) }.to_string_lossy()
    }

    fn set_section(self, section: &str) {
        let section = CString::new(section).unwrap();
        unsafe { LLVMSetSection(self.as_ptr(), section.as_ptr()) };
    }

    fn visibility(self) -> Visibility {
        unsafe { LLVMGetVisibility(self.as_ptr()) }.into()
    }

    fn set_visibility(self, visibility: Visibility) {
        unsafe { LLVMSetVisibility(self.as_ptr(), visibility.into()) };
    }

    fn dll_storage_class(self) -> DLLStorageClass {
        unsafe { LLVMGetDLLStorageClass(self.as_ptr()) }.into()
    }

    fn set_dll_storage_class(self, dll_storage_class: DLLStorageClass) {
        unsafe { LLVMSetDLLStorageClass(self.as_ptr(), dll_storage_class.into()) };
    }

    fn unnamed_address(self) -> UnnamedAddr {
        unsafe { LLVMGetUnnamedAddress(self.as_ptr()) }.into()
    }

    fn set_unnamed_address(self, unnamed_address: UnnamedAddr) {
        unsafe { LLVMSetUnnamedAddress(self.as_ptr(), unnamed_address.into()) };
    }

    fn value_type(self) -> Type<'ctx> {
        unsafe { Type::from_ptr(LLVMGlobalGetValueType(self.as_ptr())) }
    }

    fn alignment(self) -> u32 {
        unsafe { LLVMGetAlignment(self.as_ptr()) }
    }

    fn set_alignment(self, bytes: u32) {
        unsafe { LLVMSetAlignment(self.as_ptr(), bytes) };
    }

    // TODO: get/set metadata
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct Value<'ctx, 'p> {
    inner: LLVMValueRef,
    context: PhantomData<&'ctx Context>,
    parent: PhantomData<&'p ()>,
}

impl<'ctx: 'p, 'p> AnyValue<'ctx, 'p> for Value<'ctx, 'p> {
    fn as_ptr(self) -> LLVMValueRef {
        self.inner
    }

    unsafe fn from_ptr(value_ref: LLVMValueRef) -> Self {
        Value {
            inner: value_ref,
            context: PhantomData,
            parent: PhantomData,
        }
    }

    unsafe fn match_ptr(_value_ref: LLVMValueRef) -> bool {
        true
    }
}

impl_debug_and_display_for_value!(Value);

macro_rules! impl_any_value_for {
    ($name:ident($base:ident) { $kind:ident }) => {
        impl<'ctx: 'p, 'p> AnyValue<'ctx, 'p> for $name<'ctx, 'p> {
            fn as_ptr(self) -> LLVMValueRef {
                self.0.as_ptr()
            }

            unsafe fn from_ptr(value_ref: LLVMValueRef) -> Self {
                $name($base::from_ptr(value_ref))
            }

            unsafe fn match_ptr(value_ref: LLVMValueRef) -> bool {
                !$kind(value_ref).is_null()
            }
        }
    };
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct Constant<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx> Constant<'ctx, 'ctx> {
    pub fn nullptr(ty: PointerType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstPointerNull(ty.as_ptr())) }
    }

    pub fn undef(ty: impl AnyType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMGetUndef(ty.as_ptr())) }
    }

    pub fn align_of(ty: impl AnyType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMAlignOf(ty.as_ptr())) }
    }

    pub fn size_of(ty: impl AnyType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMSizeOf(ty.as_ptr())) }
    }
}

impl_any_value_for!(Constant(Value) { LLVMIsAConstant });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for Constant<'ctx, 'p> {}

impl_debug_and_display_for_value!(Constant);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantInt<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx> ConstantInt<'ctx, 'ctx> {
    pub fn get(ty: IntegerType<'ctx>, value: u64, sign_extend: bool) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstInt(
                ty.as_ptr(),
                value,
                if sign_extend { 1 } else { 0 },
            ))
        }
    }

    pub fn null(ty: IntegerType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstNull(ty.as_ptr())) }
    }

    pub fn all_ones(ty: IntegerType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstAllOnes(ty.as_ptr())) }
    }

    pub fn from_words(ty: IntegerType<'ctx>, words: &[u64]) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstIntOfArbitraryPrecision(
                ty.as_ptr(),
                words.len() as u32,
                words.as_ptr(),
            ))
        }
    }

    pub fn from_str(ty: IntegerType<'ctx>, s: &str, radix: u8) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstIntOfStringAndSize(
                ty.as_ptr(),
                s.as_ptr() as *const i8,
                s.len() as u32,
                radix,
            ))
        }
    }

    pub fn sext_value(self) -> i64 {
        unsafe { LLVMConstIntGetSExtValue(self.as_ptr()) }
    }

    pub fn zext_value(self) -> u64 {
        unsafe { LLVMConstIntGetZExtValue(self.as_ptr()) }
    }
}

impl_any_value_for!(ConstantInt(Value) { LLVMIsAConstantInt });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantInt<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantInt);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantFP<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx> ConstantFP<'ctx, 'ctx> {
    pub fn get(ty: FPType<'ctx>, n: f64) -> Self {
        unsafe { Self::from_ptr(LLVMConstReal(ty.as_ptr(), n)) }
    }

    pub fn null(ty: FPType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstNull(ty.as_ptr())) }
    }

    pub fn from_str(ty: FPType<'ctx>, s: &str) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstRealOfStringAndSize(
                ty.as_ptr(),
                s.as_bytes().as_ptr() as *const i8,
                s.len() as u32,
            ))
        }
    }

    pub fn double_value(self) -> f64 {
        let mut loses_info = std::mem::MaybeUninit::uninit();
        unsafe { LLVMConstRealGetDouble(self.as_ptr(), loses_info.as_mut_ptr()) }
    }
}

impl_any_value_for!(ConstantFP(Value) { LLVMIsAConstantFP });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantFP<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantFP);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantDataArray<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx> ConstantDataArray<'ctx, 'ctx> {
    pub fn string(s: &str, null_terminate: bool, context: &'ctx Context) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstStringInContext(
                context.as_ptr(),
                s.as_ptr() as *const c_char,
                s.len() as u32,
                if null_terminate { 0 } else { 1 },
            ))
        }
    }

    pub fn as_str(self) -> Cow<'ctx, str> {
        let mut length = std::mem::MaybeUninit::uninit();
        let bytes = unsafe { LLVMGetAsString(self.as_ptr(), length.as_mut_ptr()) };
        unsafe {
            String::from_utf8_lossy(std::slice::from_raw_parts(
                bytes as *const u8,
                length.assume_init(),
            ))
        }
    }
}

impl_any_value_for!(ConstantDataArray(Value) { LLVMIsAConstantDataArray });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantDataArray<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantDataArray);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantStruct<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> ConstantStruct<'ctx, 'p> {
    pub fn get(constants: &[Constant<'ctx, 'p>], packed: bool, context: &'ctx Context) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstStructInContext(
                context.as_ptr(),
                constants.as_ptr() as *mut LLVMValueRef,
                constants.len() as u32,
                if packed { 1 } else { 0 },
            ))
        }
    }

    pub fn null(ty: StructType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstNull(ty.as_ptr())) }
    }

    pub fn new(struct_ty: StructType<'ctx>, constants: &[Constant<'ctx, 'p>]) -> Self {
        assert!(!struct_ty.is_literal());
        unsafe {
            Self::from_ptr(LLVMConstNamedStruct(
                struct_ty.as_ptr(),
                constants.as_ptr() as *mut LLVMValueRef,
                constants.len() as u32,
            ))
        }
    }
}

impl_any_value_for!(ConstantStruct(Value) { LLVMIsAConstantStruct });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantStruct<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantStruct);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantArray<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> ConstantArray<'ctx, 'p> {
    pub fn get(element_ty: impl AnyType<'ctx>, constants: &[Constant<'ctx, 'p>]) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstArray(
                element_ty.as_ptr(),
                constants.as_ptr() as *mut LLVMValueRef,
                constants.len() as u32,
            ))
        }
    }

    pub fn null(ty: ArrayType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstNull(ty.as_ptr())) }
    }
}

impl_any_value_for!(ConstantArray(Value) { LLVMIsAConstantArray });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantArray<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantArray);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ConstantVector<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> ConstantVector<'ctx, 'p> {
    pub fn get(constants: &[Constant<'ctx, 'p>]) -> Self {
        unsafe {
            Self::from_ptr(LLVMConstVector(
                constants.as_ptr() as *mut LLVMValueRef,
                constants.len() as u32,
            ))
        }
    }

    pub fn null(ty: VectorType<'ctx>) -> Self {
        unsafe { Self::from_ptr(LLVMConstNull(ty.as_ptr())) }
    }
}

impl_any_value_for!(ConstantVector(Value) { LLVMIsAConstantVector });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for ConstantVector<'ctx, 'p> {}

impl_debug_and_display_for_value!(ConstantVector);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct GlobalVariable<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> GlobalVariable<'ctx, 'p> {
    pub fn initializer(self) -> Option<Constant<'ctx, 'p>> {
        let init = unsafe { LLVMGetInitializer(self.as_ptr()) };
        if init.is_null() {
            None
        } else {
            Some(unsafe { Constant::from_ptr(init) })
        }
    }

    pub fn set_initializer(self, v: Option<impl AnyConstant<'ctx, 'p>>) {
        let v = match v {
            Some(v) => v.as_ptr(),
            None => std::ptr::null_mut(),
        };
        unsafe { LLVMSetInitializer(self.as_ptr(), v) };
    }

    pub fn is_thread_local(self) -> bool {
        (unsafe { LLVMIsThreadLocal(self.as_ptr()) }) != 0
    }

    pub fn set_is_thread_local(self, v: bool) {
        unsafe { LLVMSetThreadLocal(self.as_ptr(), if v { 1 } else { 0 }) };
    }

    pub fn is_constant(self) -> bool {
        (unsafe { LLVMIsGlobalConstant(self.as_ptr()) }) != 0
    }

    pub fn set_is_constant(self, v: bool) {
        unsafe { LLVMSetGlobalConstant(self.as_ptr(), if v { 1 } else { 0 }) };
    }

    pub fn thread_local_mode(self) -> ThreadLocalMode {
        unsafe { LLVMGetThreadLocalMode(self.as_ptr()) }.into()
    }

    pub fn set_thread_local_mode(self, v: ThreadLocalMode) {
        unsafe { LLVMSetThreadLocalMode(self.as_ptr(), v.into()) };
    }

    pub fn is_externally_initialized(self) -> bool {
        (unsafe { LLVMIsExternallyInitialized(self.as_ptr()) }) != 0
    }

    pub fn set_is_externally_initialized(self, v: bool) {
        unsafe { LLVMSetExternallyInitialized(self.as_ptr(), if v { 1 } else { 0 }) };
    }

    /// Remove this global variable from its parent `Module`.
    ///
    /// # Safety
    /// `erase_from_parent` is marked as unsafe because `GlobalVariable` is
    /// Copy and is not tracked by Rust's ownership model. By removing
    /// `GlobalVariable`, the caller must ensure that the removed
    /// `GlobalVariable` are not used anymore.
    pub unsafe fn erase_from_parent(self) {
        LLVMDeleteGlobal(self.as_ptr());
    }
}

impl_any_value_for!(GlobalVariable(Value) { LLVMIsAGlobalVariable });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for GlobalVariable<'ctx, 'p> {}

unsafe impl<'ctx: 'p, 'p> AnyGlobalValue<'ctx, 'p> for GlobalVariable<'ctx, 'p> {}

impl_debug_and_display_for_value!(GlobalVariable);

// TODO: Global Aliases

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct Function<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> Function<'ctx, 'p> {
    // TODO: PersonalityFn, CallConv, GC, ...

    pub fn params_len(self) -> usize {
        unsafe { LLVMCountParams(self.as_ptr()) as usize }
    }

    pub fn params(self) -> Vec<Value<'ctx, 'p>> {
        let size = self.params_len();
        let mut dest = Vec::with_capacity(size);
        unsafe {
            LLVMGetParams(self.as_ptr(), dest.as_mut_ptr() as *mut LLVMValueRef);
            dest.set_len(size);
        }
        dest
    }

    pub fn basic_blocks_len(self) -> usize {
        unsafe { LLVMCountBasicBlocks(self.as_ptr()) as usize }
    }

    pub fn basic_blocks(self) -> Vec<BasicBlock<'ctx, 'p>> {
        let size = self.basic_blocks_len();
        let mut dest = Vec::with_capacity(size);
        unsafe {
            LLVMGetBasicBlocks(self.as_ptr(), dest.as_mut_ptr() as *mut LLVMBasicBlockRef);
            dest.set_len(size);
        }
        dest
    }

    pub fn entry_block(self) -> Option<BasicBlock<'ctx, 'p>> {
        if self.basic_blocks_len() != 0 {
            Some(unsafe { BasicBlock::from_ptr(LLVMGetEntryBasicBlock(self.as_ptr())) })
        } else {
            None
        }
    }

    pub fn append_block(self, name: &str) -> BasicBlock<'ctx, 'p> {
        let context = unsafe { LLVMGetTypeContext(self.get_type().as_ptr()) };
        let name = CString::new(name).unwrap();
        unsafe {
            BasicBlock::from_ptr(LLVMAppendBasicBlockInContext(
                context,
                self.as_ptr(),
                name.as_ptr(),
            ))
        }
    }

    pub fn verify(self) -> Result<(), ()> {
        match unsafe {
            LLVMVerifyFunction(
                self.as_ptr(),
                LLVMVerifierFailureAction::LLVMReturnStatusAction,
            )
        } {
            1 => Err(()),
            _ => Ok(()),
        }
    }

    /// Remove this function from its parent `Module`.
    ///
    /// # Safety
    /// `erase_from_parent` is marked as unsafe because `Function` is Copy
    /// and is not tracked by Rust's ownership model. By removing `Function`,
    /// the caller must ensure that the removed `Function` and its children
    /// (including appended `BasicBlock`s and params) are not used anymore.
    pub unsafe fn erase_from_parent(self) {
        LLVMDeleteFunction(self.as_ptr());
    }
}

impl_any_value_for!(Function(Value) { LLVMIsAFunction });

unsafe impl<'ctx: 'p, 'p> AnyConstant<'ctx, 'p> for Function<'ctx, 'p> {}

unsafe impl<'ctx: 'p, 'p> AnyGlobalValue<'ctx, 'p> for Function<'ctx, 'p> {}

impl_debug_and_display_for_value!(Function);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct PhiNode<'ctx, 'p>(Value<'ctx, 'p>);

impl<'ctx: 'p, 'p> PhiNode<'ctx, 'p> {
    pub fn add_incoming(self, value: impl AnyValue<'ctx, 'p>, bb: BasicBlock<'ctx, 'p>) {
        unsafe {
            LLVMAddIncoming(
                self.as_ptr(),
                [value.as_ptr()].as_mut_ptr(),
                [bb.as_ptr()].as_mut_ptr(),
                1,
            )
        };
    }
}

impl_any_value_for!(PhiNode(Value) { LLVMIsAPHINode });

impl_debug_and_display_for_value!(PhiNode);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_cast() {}

    #[test]
    fn constant_int() {
        let ctx = Context::new();
        let ty = IntegerType::get(8, &ctx);
        let a = ConstantInt::get(ty, 100, false);
        let b = ConstantInt::null(ty);
        let c = ConstantInt::all_ones(ty);
        let d = ConstantInt::from_words(ty, &[99]);
        let e = ConstantInt::from_str(ty, "-24", 10);
        assert_eq!(a.get_type(), ty.as_type());
        assert_eq!(a.sext_value(), 100);
        assert_eq!(b.sext_value(), 0);
        assert_eq!(c.sext_value(), -1);
        assert_eq!(d.sext_value(), 99);
        assert_eq!(e.sext_value(), -24);
        assert_eq!(a.zext_value(), 100);
        assert_eq!(b.zext_value(), 0);
        assert_eq!(c.zext_value(), 255);
        assert_eq!(d.zext_value(), 99);
        assert!(!a.is_null());
        assert!(b.is_null());
        assert!(!a.is_undef());
        assert!(!b.is_undef());
        assert_eq!(c.to_string(), "i8 -1");
    }

    #[test]
    fn constant_fp() {
        let ctx = Context::new();
        let ty = FPType::float(&ctx);
        let a = ConstantFP::get(ty, 10000.0);
        let b = ConstantFP::null(ty);
        let c = ConstantFP::from_str(ty, "1e+5");
        assert_eq!(a.get_type(), ty.as_type());
        assert_eq!(a.double_value(), 10000.0);
        assert_eq!(b.double_value(), 0.0);
        assert_eq!(c.double_value(), 100000.0);
        assert!(!a.is_null());
        assert!(b.is_null());
        assert!(!a.is_undef());
        assert!(!b.is_undef());
    }

    #[test]
    fn constant_array() {
        let ctx = Context::new();
        let a = ConstantDataArray::string("Hello", false, &ctx);
        let b = ConstantDataArray::string("World", true, &ctx);
        let c = ConstantArray::get(
            IntegerType::get(32, &ctx),
            &[
                ConstantInt::get(IntegerType::get(32, &ctx), -5 as i64 as u64, true).as_constant(),
                ConstantInt::get(IntegerType::get(32, &ctx), 10 as i64 as u64, true).as_constant(),
                ConstantInt::get(IntegerType::get(32, &ctx), -15 as i64 as u64, true).as_constant(),
            ],
        );
        let d = ConstantArray::null(ArrayType::get(IntegerType::get(16, &ctx), 10));
        assert_eq!(
            a.get_type(),
            ArrayType::get(IntegerType::get(8, &ctx), 5).as_type()
        );
        assert_eq!(a.as_str(), "Hello");
        assert_eq!(b.as_str(), "World\u{0}");
        assert_eq!(a.to_string(), "[5 x i8] c\"Hello\"");
        assert_eq!(b.to_string(), "[6 x i8] c\"World\\00\"");
        assert_eq!(c.to_string(), "[3 x i32] [i32 -5, i32 10, i32 -15]");
        assert_eq!(d.to_string(), "[10 x i16] zeroinitializer");
    }

    #[test]
    fn constant_struct() {
        let ctx = Context::new();
        let ty = StructType::new("hello", &ctx);
        ty.set_body(
            &[
                IntegerType::get(8, &ctx).as_type(),
                IntegerType::get(16, &ctx).as_type(),
                IntegerType::get(32, &ctx).as_type(),
            ],
            false,
        );
        let a = ConstantStruct::get(&[], false, &ctx);
        let b = ConstantStruct::null(StructType::get(
            &[
                IntegerType::get(32, &ctx).as_type(),
                FPType::double(&ctx).as_type(),
            ],
            false,
            &ctx,
        ));
        let c = ConstantStruct::new(
            ty,
            &[
                ConstantInt::get(IntegerType::get(8, &ctx), 10, false).as_constant(),
                ConstantInt::get(IntegerType::get(16, &ctx), 20, false).as_constant(),
                ConstantInt::get(IntegerType::get(32, &ctx), 30, false).as_constant(),
            ],
        );
        assert_eq!(a.get_type(), StructType::get(&[], false, &ctx).as_type());
        assert_eq!(
            b.get_type(),
            StructType::get(
                &[
                    IntegerType::get(32, &ctx).as_type(),
                    FPType::double(&ctx).as_type()
                ],
                false,
                &ctx
            )
            .as_type()
        );
        assert_eq!(a.to_string(), "{} zeroinitializer");
        assert_eq!(b.to_string(), "{ i32, double } zeroinitializer");
        assert_eq!(c.to_string(), "%hello { i8 10, i16 20, i32 30 }");
    }

    #[test]
    fn constant_vector() {
        let ctx = Context::new();
        let a = ConstantVector::get(&[
            ConstantInt::get(IntegerType::get(8, &ctx), 1, false).as_constant(),
            ConstantInt::get(IntegerType::get(8, &ctx), 2, false).as_constant(),
            ConstantInt::get(IntegerType::get(8, &ctx), 3, false).as_constant(),
            ConstantInt::get(IntegerType::get(8, &ctx), 4, false).as_constant(),
        ]);
        let b = ConstantVector::null(VectorType::get(IntegerType::get(16, &ctx), 8));
        assert_eq!(a.to_string(), "<4 x i8> <i8 1, i8 2, i8 3, i8 4>");
        assert_eq!(b.to_string(), "<8 x i16> zeroinitializer");
    }

    #[test]
    fn global_variable() {
        let ctx = Context::new();
        let module = Module::new("m", &ctx);
        let a = module.add_global("a", IntegerType::get(32, &ctx), None);
        let b = module.add_global("b", IntegerType::get(32, &ctx), None);
        b.set_initializer(Some(ConstantInt::get(
            IntegerType::get(32, &ctx),
            1234,
            true,
        )));
        assert_eq!(a.initializer(), None);
        assert_eq!(
            b.initializer(),
            Some(ConstantInt::get(IntegerType::get(32, &ctx), 1234, true).as_constant())
        );
        assert_eq!(module.lookup_global("c"), None);
        assert_eq!(module.lookup_global("b"), Some(b));
        unsafe { a.erase_from_parent() };
        assert_eq!(module.lookup_global("a"), None);
        assert_eq!(
            b.get_type(),
            PointerType::get(IntegerType::get(32, &ctx), 0).as_type()
        );
        assert_eq!(b.value_type(), IntegerType::get(32, &ctx).as_type());
        assert_eq!(b.name(), "b");
        b.set_name("x");
        assert_eq!(module.lookup_global("b"), None);
        assert_eq!(module.lookup_global("x"), Some(b));
        assert_eq!(&module.globals(), &[b]);
    }

    #[test]
    fn function() {
        let ctx = Context::new();
        let module = Module::new("m", &ctx);
        let ty = FunctionType::get(
            IntegerType::get(32, &ctx),
            &[
                IntegerType::get(8, &ctx).as_type(),
                IntegerType::get(16, &ctx).as_type(),
            ],
            false,
        );
        let f = module.add_function("f", ty);
        assert_eq!(f.params_len(), 2);
        assert_eq!(
            f.params()[0].get_type(),
            IntegerType::get(8, &ctx).as_type()
        );
        assert_eq!(
            f.params()[1].get_type(),
            IntegerType::get(16, &ctx).as_type()
        );
        assert!(f.verify().is_ok());
        assert_eq!(module.lookup_function("f"), Some(f));
        assert_eq!(module.lookup_function("g"), None);
        unsafe { f.erase_from_parent() };
        assert_eq!(module.lookup_function("f"), None);
        assert!(module.functions().is_empty());
    }
}
