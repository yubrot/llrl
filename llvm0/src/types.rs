use super::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::fmt;
use std::marker::PhantomData;

pub use llvm_sys::LLVMTypeKind as TypeKind;

pub trait AnyType<'ctx>: Copy {
    fn as_ptr(self) -> LLVMTypeRef;

    #[allow(clippy::missing_safety_doc)]
    unsafe fn from_ptr(type_ref: LLVMTypeRef) -> Self;

    #[allow(clippy::missing_safety_doc)]
    unsafe fn match_ptr(type_ref: LLVMTypeRef) -> bool;

    fn context(self) -> &'ctx Context {
        unsafe { Context::from_ptr(LLVMGetTypeContext(self.as_ptr())) }
    }

    fn kind(self) -> TypeKind {
        unsafe { LLVMGetTypeKind(self.as_ptr()) }
    }

    fn as_type(self) -> Type<'ctx> {
        unsafe { Type::from_ptr(self.as_ptr()) }
    }

    fn is_type_of<T: AnyType<'ctx>>(self) -> bool {
        unsafe { T::match_ptr(self.as_ptr()) }
    }

    fn as_type_of<T: AnyType<'ctx>>(self) -> Option<T> {
        if self.is_type_of::<T>() {
            Some(unsafe { T::from_ptr(self.as_ptr()) })
        } else {
            None
        }
    }

    fn is_sized(self) -> bool {
        (unsafe { LLVMTypeIsSized(self.as_ptr()) }) != 0
    }

    fn print(self) -> Message {
        unsafe { Message::from_ptr(LLVMPrintTypeToString(self.as_ptr())) }
    }
}

pub unsafe trait AnySequentialType<'ctx>: AnyType<'ctx> {
    fn element_type(self) -> Type<'ctx> {
        unsafe { Type::from_ptr(LLVMGetElementType(self.as_ptr())) }
    }
}

macro_rules! impl_debug_and_display_for_type {
    ($name:ident) => {
        impl<'ctx> fmt::Debug for $name<'ctx> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}::from_ptr({:?})",
                    std::stringify!($name),
                    self.as_ptr()
                )
            }
        }

        impl<'ctx> fmt::Display for $name<'ctx> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                fmt::Display::fmt(&self.print().to_string_lossy(), f)
            }
        }
    };
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct Type<'ctx> {
    inner: LLVMTypeRef,
    context: PhantomData<&'ctx Context>,
}

impl<'ctx> Type<'ctx> {
    pub fn label(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMLabelTypeInContext(context.as_ptr())) }
    }

    pub fn metadata(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMMetadataTypeInContext(context.as_ptr())) }
    }
}

impl<'ctx> AnyType<'ctx> for Type<'ctx> {
    fn as_ptr(self) -> LLVMTypeRef {
        self.inner
    }

    unsafe fn from_ptr(type_ref: LLVMTypeRef) -> Self {
        Type {
            inner: type_ref,
            context: PhantomData,
        }
    }

    unsafe fn match_ptr(_type_ref: LLVMTypeRef) -> bool {
        true
    }
}

impl_debug_and_display_for_type!(Type);

macro_rules! impl_any_type_for {
    ($name:ident($base:ident) { $( $kind:ident ),* }) => {
        impl<'ctx> AnyType<'ctx> for $name<'ctx> {
            fn as_ptr(self) -> LLVMTypeRef {
                self.0.as_ptr()
            }

            unsafe fn from_ptr(type_ref: LLVMTypeRef) -> Self {
                $name($base::from_ptr(type_ref))
            }

            unsafe fn match_ptr(type_ref: LLVMTypeRef) -> bool {
                match LLVMGetTypeKind(type_ref) {
                    $( TypeKind::$kind => true, )*
                    _ => false,
                }
            }
        }
    };
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct VoidType<'ctx>(Type<'ctx>);

impl<'ctx> VoidType<'ctx> {
    pub fn get(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMVoidTypeInContext(context.as_ptr())) }
    }
}

impl_any_type_for!(VoidType(Type) { LLVMVoidTypeKind });

impl_debug_and_display_for_type!(VoidType);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct IntegerType<'ctx>(Type<'ctx>);

impl<'ctx> IntegerType<'ctx> {
    pub fn get(bit_width: u32, context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMIntTypeInContext(context.as_ptr(), bit_width)) }
    }

    pub fn bit_width(self) -> u32 {
        unsafe { LLVMGetIntTypeWidth(self.as_ptr()) }
    }
}

impl_any_type_for!(IntegerType(Type) { LLVMIntegerTypeKind });

impl_debug_and_display_for_type!(IntegerType);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct FPType<'ctx>(Type<'ctx>);

impl<'ctx> FPType<'ctx> {
    pub fn half(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMHalfTypeInContext(context.as_ptr())) }
    }

    pub fn float(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMFloatTypeInContext(context.as_ptr())) }
    }

    pub fn double(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMDoubleTypeInContext(context.as_ptr())) }
    }

    pub fn x86_fp80(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMX86FP80TypeInContext(context.as_ptr())) }
    }

    pub fn fp128(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMFP128TypeInContext(context.as_ptr())) }
    }

    pub fn ppcfp128(context: &'ctx Context) -> Self {
        unsafe { Self::from_ptr(LLVMPPCFP128TypeInContext(context.as_ptr())) }
    }
}

impl_any_type_for!(FPType(Type) {
    LLVMHalfTypeKind,
    LLVMFloatTypeKind,
    LLVMDoubleTypeKind,
    LLVMX86_FP80TypeKind,
    LLVMFP128TypeKind,
    LLVMPPC_FP128TypeKind
});

impl_debug_and_display_for_type!(FPType);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct FunctionType<'ctx>(Type<'ctx>);

impl<'ctx> FunctionType<'ctx> {
    pub fn get(return_ty: impl AnyType<'ctx>, params: &[Type<'ctx>], is_var_arg: bool) -> Self {
        let is_var_arg = if is_var_arg { 1 } else { 0 };
        unsafe {
            Self::from_ptr(LLVMFunctionType(
                return_ty.as_ptr(),
                params.as_ptr() as *mut LLVMTypeRef,
                params.len() as u32,
                is_var_arg,
            ))
        }
    }

    pub fn param_types_len(self) -> usize {
        unsafe { LLVMCountParamTypes(self.as_ptr()) as usize }
    }

    pub fn param_types(self) -> Vec<Type<'ctx>> {
        let size = self.param_types_len();
        let mut dest = Vec::with_capacity(size);
        unsafe {
            LLVMGetParamTypes(self.as_ptr(), dest.as_mut_ptr() as *mut LLVMTypeRef);
            dest.set_len(size);
        }
        dest
    }

    pub fn return_type(self) -> Type<'ctx> {
        unsafe { Type::from_ptr(LLVMGetReturnType(self.as_ptr())) }
    }

    pub fn is_var_arg(self) -> bool {
        let is_var_arg = unsafe { LLVMIsFunctionVarArg(self.as_ptr()) };
        is_var_arg != 0
    }
}

impl_any_type_for!(FunctionType(Type) { LLVMFunctionTypeKind });

impl_debug_and_display_for_type!(FunctionType);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct StructType<'ctx>(Type<'ctx>);

impl<'ctx> StructType<'ctx> {
    pub fn get(elements: &[Type<'ctx>], packed: bool, context: &'ctx Context) -> Self {
        unsafe {
            Self::from_ptr(LLVMStructTypeInContext(
                context.as_ptr(),
                elements.as_ptr() as *mut LLVMTypeRef,
                elements.len() as u32,
                if packed { 1 } else { 0 },
            ))
        }
    }

    pub fn new(name: &str, context: &'ctx Context) -> Self {
        let name = CString::new(name).unwrap();
        unsafe { Self::from_ptr(LLVMStructCreateNamed(context.as_ptr(), name.as_ptr())) }
    }

    pub fn name(self) -> Option<Cow<'ctx, str>> {
        if self.is_literal() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(LLVMGetStructName(self.as_ptr())) }.to_string_lossy())
        }
    }

    pub fn is_literal(self) -> bool {
        (unsafe { LLVMIsLiteralStruct(self.as_ptr()) }) != 0
    }

    pub fn is_opaque(self) -> bool {
        (unsafe { LLVMIsOpaqueStruct(self.as_ptr()) }) != 0
    }

    pub fn is_packed(self) -> bool {
        (unsafe { LLVMIsPackedStruct(self.as_ptr()) }) != 0
    }

    pub fn set_body(self, elements: &[Type<'ctx>], packed: bool) -> bool {
        if self.is_opaque() {
            unsafe {
                LLVMStructSetBody(
                    self.as_ptr(),
                    elements.as_ptr() as *mut LLVMTypeRef,
                    elements.len() as u32,
                    if packed { 1 } else { 0 },
                )
            }
            true
        } else {
            false
        }
    }

    pub fn elements_len(self) -> Option<usize> {
        if self.is_opaque() {
            None
        } else {
            Some(unsafe { LLVMCountStructElementTypes(self.as_ptr()) } as usize)
        }
    }

    pub fn elements(self) -> Option<Vec<Type<'ctx>>> {
        let size = self.elements_len()?;
        let mut dest = Vec::with_capacity(size);
        unsafe {
            LLVMGetStructElementTypes(self.as_ptr(), dest.as_mut_ptr() as *mut LLVMTypeRef);
            dest.set_len(size);
        }
        Some(dest)
    }
}

impl_any_type_for!(StructType(Type) { LLVMStructTypeKind });

impl_debug_and_display_for_type!(StructType);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct ArrayType<'ctx>(Type<'ctx>);

impl<'ctx> ArrayType<'ctx> {
    pub fn get(element: impl AnyType<'ctx>, l: usize) -> Self {
        unsafe { Self::from_ptr(LLVMArrayType(element.as_ptr(), l as u32)) }
    }

    pub fn length(self) -> usize {
        (unsafe { LLVMGetArrayLength(self.as_ptr()) }) as usize
    }
}

impl_any_type_for!(ArrayType(Type) { LLVMArrayTypeKind });

impl_debug_and_display_for_type!(ArrayType);

unsafe impl<'ctx> AnySequentialType<'ctx> for ArrayType<'ctx> {}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct PointerType<'ctx>(Type<'ctx>);

impl<'ctx> PointerType<'ctx> {
    pub fn get(element: impl AnyType<'ctx>, address_space: u32) -> Self {
        unsafe { Self::from_ptr(LLVMPointerType(element.as_ptr(), address_space)) }
    }

    pub fn address_space(self) -> u32 {
        unsafe { LLVMGetPointerAddressSpace(self.as_ptr()) }
    }
}

impl_any_type_for!(PointerType(Type) { LLVMPointerTypeKind });

impl_debug_and_display_for_type!(PointerType);

unsafe impl<'ctx> AnySequentialType<'ctx> for PointerType<'ctx> {}

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Copy, Clone)]
pub struct VectorType<'ctx>(Type<'ctx>);

impl<'ctx> VectorType<'ctx> {
    pub fn get(element: impl AnyType<'ctx>, size: u32) -> Self {
        unsafe { Self::from_ptr(LLVMVectorType(element.as_ptr(), size)) }
    }

    pub fn size(self) -> u32 {
        unsafe { LLVMGetVectorSize(self.as_ptr()) }
    }
}

impl_any_type_for!(VectorType(Type) { LLVMVectorTypeKind });

impl_debug_and_display_for_type!(VectorType);

unsafe impl<'ctx> AnySequentialType<'ctx> for VectorType<'ctx> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_cast() {
        let context = Context::new();
        assert!(VoidType::get(&context).is_type_of::<Type>());
        assert!(!VoidType::get(&context).is_type_of::<IntegerType>());
        assert!(IntegerType::get(1, &context).is_type_of::<IntegerType>());
        assert!(!IntegerType::get(1, &context).is_type_of::<FPType>());
        assert!(FPType::float(&context).as_type_of::<FPType>().is_some());
        assert!(FPType::float(&context)
            .as_type_of::<IntegerType>()
            .is_none());
    }

    #[test]
    fn sized() {
        let context = Context::new();
        let void = VoidType::get(&context);
        let half = FPType::half(&context);
        assert!(!void.is_sized());
        assert!(!FunctionType::get(half, &[], false).is_sized());
        assert!(PointerType::get(FunctionType::get(half, &[], false), 0).is_sized());
    }

    #[test]
    fn integer_type_bits() {
        let context = Context::new();
        assert_eq!(IntegerType::get(4, &context).bit_width(), 4u32);
        assert_eq!(IntegerType::get(32, &context).bit_width(), 32u32);
    }

    #[test]
    fn function_type_components() {
        let context = Context::new();
        let ft = FunctionType::get(
            FPType::float(&context),
            &[
                IntegerType::get(32, &context).as_type(),
                IntegerType::get(64, &context).as_type(),
            ],
            false,
        );
        assert_eq!(ft.return_type().to_string(), "float");
        assert_eq!(ft.param_types_len(), 2usize);
        assert_eq!(ft.param_types()[0].to_string(), "i32");
        assert_eq!(ft.param_types()[1].to_string(), "i64");
        assert!(!ft.is_var_arg());
    }

    #[test]
    fn struct_types() {
        let context = Context::new();
        let n = IntegerType::get(32, &context).as_type();
        let m = IntegerType::get(64, &context).as_type();
        let a = StructType::get(&[n], false, &context);
        let b = StructType::get(&[n, m], true, &context);
        let c = StructType::new("c", &context);
        let d = StructType::new("c", &context);
        d.set_body(&[], false);
        let e = StructType::new("c", &context);
        e.set_body(&[n], true);

        assert!(a.is_literal());
        assert!(b.is_literal());
        assert!(!c.is_literal());
        assert!(!d.is_literal());
        assert!(!e.is_literal());
        assert!(!a.is_opaque());
        assert!(c.is_opaque());
        assert!(!e.is_opaque());
        assert!(!a.is_packed());
        assert!(b.is_packed());
        assert!(!d.is_packed());
        assert!(e.is_packed());
        assert_eq!(b.elements_len(), Some(2usize));
        assert_eq!(c.elements_len(), None);
        assert!(c.elements().is_none());
        match b.elements() {
            Some(es) => {
                assert_eq!(es[0].to_string(), "i32");
                assert_eq!(es[1].to_string(), "i64");
            }
            None => assert!(false),
        }
    }

    #[test]
    fn array_types() {
        let context = Context::new();
        let ty = ArrayType::get(IntegerType::get(32, &context), 4);
        assert_eq!(ty.length(), 4usize);
        assert_eq!(ty.element_type().to_string(), "i32");
    }

    #[test]
    fn pointer_types() {
        let context = Context::new();
        let ty = PointerType::get(FPType::float(&context), 0);
        assert_eq!(ty.address_space(), 0);
        assert_eq!(ty.element_type().to_string(), "float");
    }

    #[test]
    fn vector_types() {
        let context = Context::new();
        let ty = VectorType::get(FPType::float(&context), 4);
        assert_eq!(ty.size(), 4);
        assert_eq!(ty.element_type().to_string(), "float");
    }
}
