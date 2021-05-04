use super::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;
use std::ffi::CString;
use std::marker::PhantomData;
use std::os::raw::c_char;

// NOTE: CStr::from_bytes_with_nul_unchecked as const fn is not stable.
static EMPTY_CSTR: &[c_char] = &[0];

type LLVMBinaryFn =
    unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, *const c_char) -> LLVMValueRef;

type LLVMUnaryFn =
    unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, *const c_char) -> LLVMValueRef;

type LLVMConvFn =
    unsafe extern "C" fn(LLVMBuilderRef, LLVMValueRef, LLVMTypeRef, *const c_char) -> LLVMValueRef;

pub struct Builder<'ctx, 'p> {
    _refs: LLVMBuilderRef,
    _target: PhantomData<BasicBlock<'ctx, 'p>>,
}

macro_rules! binary_fn {
    ($name:ident, $f:expr) => {
        pub fn $name(
            &self,
            l: impl AnyValue<'ctx, 'p>,
            r: impl AnyValue<'ctx, 'p>,
        ) -> Value<'ctx, 'p> {
            self.build_binary($f, l, r)
        }
    };
}

macro_rules! unary_fn {
    ($name:ident, $f:expr) => {
        pub fn $name(&self, a: impl AnyValue<'ctx, 'p>) -> Value<'ctx, 'p> {
            self.build_unary($f, a)
        }
    };
}

macro_rules! conv_fn {
    ($name:ident, $f:expr) => {
        pub fn $name(&self, v: impl AnyValue<'ctx, 'p>, t: impl AnyType<'ctx>) -> Value<'ctx, 'p> {
            self.build_conv($f, v, t)
        }
    };
}

macro_rules! cmp_fn {
    ($name:ident, $f:ident, $pred:expr) => {
        pub fn $name(
            &self,
            l: impl AnyValue<'ctx, 'p>,
            r: impl AnyValue<'ctx, 'p>,
        ) -> Value<'ctx, 'p> {
            self.$f($pred, l, r)
        }
    };
}

impl<'ctx: 'p, 'p> Builder<'ctx, 'p> {
    pub fn new(target: BasicBlock<'ctx, 'p>) -> Oo<Self> {
        unsafe {
            let builder = LLVMCreateBuilderInContext(target.context().as_ptr());
            LLVMPositionBuilderAtEnd(builder, target.as_ptr());
            Oo::from_ptr(builder)
        }
    }

    pub fn context(&self) -> &'ctx Context {
        self.insert_point().context()
    }

    pub fn insert_point(&self) -> BasicBlock<'ctx, 'p> {
        unsafe { BasicBlock::from_ptr(LLVMGetInsertBlock(self.as_ptr())) }
    }

    pub fn set_insert_point(&self, target: BasicBlock<'ctx, 'p>, end: bool) {
        unsafe {
            if end {
                LLVMPositionBuilderAtEnd(self.as_ptr(), target.as_ptr());
            } else {
                let instr = LLVMGetFirstInstruction(target.as_ptr());
                if instr.is_null() {
                    LLVMPositionBuilderAtEnd(self.as_ptr(), target.as_ptr());
                } else {
                    LLVMPositionBuilderBefore(self.as_ptr(), instr);
                }
            }
        }
    }

    pub fn append_block(&self, name: &str) -> BasicBlock<'ctx, 'p> {
        self.insert_point().parent().append_block(name)
    }

    pub fn build_ret(&self, v: impl AnyValue<'ctx, 'p>) -> Value<'ctx, 'p> {
        unsafe { Value::from_ptr(LLVMBuildRet(self.as_ptr(), v.as_ptr())) }
    }

    pub fn build_ret_void(&self) -> Value<'ctx, 'p> {
        unsafe { Value::from_ptr(LLVMBuildRetVoid(self.as_ptr())) }
    }

    pub fn build_br(&self, bb: BasicBlock<'ctx, 'p>) -> Value<'ctx, 'p> {
        unsafe { Value::from_ptr(LLVMBuildBr(self.as_ptr(), bb.as_ptr())) }
    }

    pub fn build_cond_br(
        &self,
        cond: impl AnyValue<'ctx, 'p>,
        then_bb: BasicBlock<'ctx, 'p>,
        else_bb: BasicBlock<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        unsafe {
            Value::from_ptr(LLVMBuildCondBr(
                self.as_ptr(),
                cond.as_ptr(),
                then_bb.as_ptr(),
                else_bb.as_ptr(),
            ))
        }
    }

    pub fn build_unreachable(&self) -> Value<'ctx, 'p> {
        unsafe { Value::from_ptr(LLVMBuildUnreachable(self.as_ptr())) }
    }

    fn build_binary(
        &self,
        f: LLVMBinaryFn,
        l: impl AnyValue<'ctx, 'p>,
        r: impl AnyValue<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        let l = l.as_ptr();
        let r = r.as_ptr();
        unsafe { Value::from_ptr(f(self.as_ptr(), l, r, EMPTY_CSTR.as_ptr())) }
    }

    binary_fn!(build_add, LLVMBuildAdd);
    binary_fn!(build_nswadd, LLVMBuildNSWAdd);
    binary_fn!(build_fadd, LLVMBuildFAdd);
    binary_fn!(build_sub, LLVMBuildSub);
    binary_fn!(build_nswsub, LLVMBuildNSWSub);
    binary_fn!(build_fsub, LLVMBuildFSub);
    binary_fn!(build_mul, LLVMBuildMul);
    binary_fn!(build_nswmul, LLVMBuildNSWMul);
    binary_fn!(build_fmul, LLVMBuildFMul);
    binary_fn!(build_sdiv, LLVMBuildSDiv);
    binary_fn!(build_udiv, LLVMBuildUDiv);
    binary_fn!(build_fdiv, LLVMBuildFDiv);
    binary_fn!(build_srem, LLVMBuildSRem);
    binary_fn!(build_urem, LLVMBuildURem);
    binary_fn!(build_frem, LLVMBuildFRem);
    binary_fn!(build_shl, LLVMBuildShl);
    binary_fn!(build_ashr, LLVMBuildAShr);
    binary_fn!(build_lshr, LLVMBuildLShr);
    binary_fn!(build_and, LLVMBuildAnd);
    binary_fn!(build_or, LLVMBuildOr);
    binary_fn!(build_xor, LLVMBuildXor);

    fn build_unary(&self, f: LLVMUnaryFn, a: impl AnyValue<'ctx, 'p>) -> Value<'ctx, 'p> {
        let a = a.as_ptr();
        unsafe { Value::from_ptr(f(self.as_ptr(), a, EMPTY_CSTR.as_ptr())) }
    }

    unary_fn!(build_neg, LLVMBuildNeg);
    unary_fn!(build_nswneg, LLVMBuildNSWNeg);
    unary_fn!(build_fneg, LLVMBuildFNeg);
    unary_fn!(build_not, LLVMBuildNot);

    pub fn build_alloca(&self, name: &str, ty: impl AnyType<'ctx>) -> Value<'ctx, 'p> {
        let name = CString::new(name).unwrap();
        unsafe { Value::from_ptr(LLVMBuildAlloca(self.as_ptr(), ty.as_ptr(), name.as_ptr())) }
    }

    pub fn build_array_alloca(
        &self,
        name: &str,
        ty: impl AnyType<'ctx>,
        num: impl AnyValue<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        let name = CString::new(name).unwrap();
        unsafe {
            Value::from_ptr(LLVMBuildArrayAlloca(
                self.as_ptr(),
                ty.as_ptr(),
                num.as_ptr(),
                name.as_ptr(),
            ))
        }
    }

    pub fn build_entry_alloca(&self, name: &str, ty: impl AnyType<'ctx>) -> Value<'ctx, 'p> {
        let stash_bb = self.insert_point();
        let entry_bb = stash_bb.parent().entry_block().unwrap();
        self.set_insert_point(entry_bb, false);
        let alloca = self.build_alloca(name, ty);
        self.set_insert_point(stash_bb, true);
        alloca
    }

    pub fn build_malloc(&self, name: &str, ty: impl AnyType<'ctx>) -> Value<'ctx, 'p> {
        let name = CString::new(name).unwrap();
        unsafe { Value::from_ptr(LLVMBuildMalloc(self.as_ptr(), ty.as_ptr(), name.as_ptr())) }
    }

    pub fn build_load(&self, v: impl AnyValue<'ctx, 'p>) -> Value<'ctx, 'p> {
        unsafe {
            Value::from_ptr(LLVMBuildLoad(
                self.as_ptr(),
                v.as_ptr(),
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_store(&self, val: impl AnyValue<'ctx, 'p>, ptr: impl AnyValue<'ctx, 'p>) {
        unsafe { LLVMBuildStore(self.as_ptr(), val.as_ptr(), ptr.as_ptr()) };
    }

    pub fn build_gep(
        &self,
        ptr: impl AnyValue<'ctx, 'p>,
        indices: &[Value<'ctx, 'p>],
    ) -> Value<'ctx, 'p> {
        unsafe {
            Value::from_ptr(LLVMBuildGEP(
                self.as_ptr(),
                ptr.as_ptr(),
                indices.as_ptr() as *mut LLVMValueRef,
                indices.len() as u32,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_struct_gep(&self, ptr: impl AnyValue<'ctx, 'p>, index: u32) -> Value<'ctx, 'p> {
        unsafe {
            Value::from_ptr(LLVMBuildStructGEP(
                self.as_ptr(),
                ptr.as_ptr(),
                index,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    fn build_conv(
        &self,
        f: LLVMConvFn,
        v: impl AnyValue<'ctx, 'p>,
        t: impl AnyType<'ctx>,
    ) -> Value<'ctx, 'p> {
        let v = v.as_ptr();
        let t = t.as_ptr();
        unsafe { Value::from_ptr(f(self.as_ptr(), v, t, EMPTY_CSTR.as_ptr())) }
    }

    conv_fn!(build_trunc, LLVMBuildTrunc);
    conv_fn!(build_zext, LLVMBuildZExt);
    conv_fn!(build_sext, LLVMBuildSExt);
    conv_fn!(build_fp_to_ui, LLVMBuildFPToUI);
    conv_fn!(build_fp_to_si, LLVMBuildFPToSI);
    conv_fn!(build_ui_to_fp, LLVMBuildUIToFP);
    conv_fn!(build_si_to_fp, LLVMBuildSIToFP);
    conv_fn!(build_fptrunc, LLVMBuildFPTrunc);
    conv_fn!(build_fpext, LLVMBuildFPExt);
    conv_fn!(build_ptr_to_int, LLVMBuildPtrToInt);
    conv_fn!(build_int_to_ptr, LLVMBuildIntToPtr);
    conv_fn!(build_bit_cast, LLVMBuildBitCast);
    conv_fn!(build_addr_space_cast, LLVMBuildAddrSpaceCast);

    fn build_icmp(
        &self,
        op: IntegerPredicate,
        l: impl AnyValue<'ctx, 'p>,
        r: impl AnyValue<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        let l = l.as_ptr();
        let r = r.as_ptr();
        let op = op.into();
        unsafe { Value::from_ptr(LLVMBuildICmp(self.as_ptr(), op, l, r, EMPTY_CSTR.as_ptr())) }
    }

    cmp_fn!(build_eq, build_icmp, IntegerPredicate::Eq);
    cmp_fn!(build_ne, build_icmp, IntegerPredicate::Ne);
    cmp_fn!(build_ugt, build_icmp, IntegerPredicate::Ugt);
    cmp_fn!(build_uge, build_icmp, IntegerPredicate::Uge);
    cmp_fn!(build_ult, build_icmp, IntegerPredicate::Ult);
    cmp_fn!(build_ule, build_icmp, IntegerPredicate::Ule);
    cmp_fn!(build_sgt, build_icmp, IntegerPredicate::Sgt);
    cmp_fn!(build_sge, build_icmp, IntegerPredicate::Sge);
    cmp_fn!(build_slt, build_icmp, IntegerPredicate::Slt);
    cmp_fn!(build_sle, build_icmp, IntegerPredicate::Sle);

    fn build_fcmp(
        &self,
        op: FPPredicate,
        l: impl AnyValue<'ctx, 'p>,
        r: impl AnyValue<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        let l = l.as_ptr();
        let r = r.as_ptr();
        let op = op.into();
        unsafe { Value::from_ptr(LLVMBuildFCmp(self.as_ptr(), op, l, r, EMPTY_CSTR.as_ptr())) }
    }

    cmp_fn!(build_feq, build_fcmp, FPPredicate::Oeq);
    cmp_fn!(build_fgt, build_fcmp, FPPredicate::Ogt);
    cmp_fn!(build_fge, build_fcmp, FPPredicate::Oge);
    cmp_fn!(build_flt, build_fcmp, FPPredicate::Olt);
    cmp_fn!(build_fle, build_fcmp, FPPredicate::Ole);
    cmp_fn!(build_fne, build_fcmp, FPPredicate::One);

    pub fn build_phi(&self, ty: impl AnyType<'ctx>) -> PhiNode<'ctx, 'p> {
        unsafe {
            PhiNode::from_ptr(LLVMBuildPhi(
                self.as_ptr(),
                ty.as_ptr(),
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_call(
        &self,
        f: impl AnyValue<'ctx, 'p>,
        args: &[Value<'ctx, 'p>],
    ) -> Value<'ctx, 'p> {
        unsafe {
            Value::from_ptr(LLVMBuildCall(
                self.as_ptr(),
                f.as_ptr(),
                args.as_ptr() as *mut LLVMValueRef,
                args.len() as u32,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_select(
        &self,
        cond: impl AnyValue<'ctx, 'p>,
        then: impl AnyValue<'ctx, 'p>,
        else_: impl AnyValue<'ctx, 'p>,
    ) -> Value<'ctx, 'p> {
        let cond = cond.as_ptr();
        let then = then.as_ptr();
        let else_ = else_.as_ptr();
        unsafe {
            Value::from_ptr(LLVMBuildSelect(
                self.as_ptr(),
                cond,
                then,
                else_,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_extract_value(&self, agg: impl AnyValue<'ctx, 'p>, index: u32) -> Value<'ctx, 'p> {
        let agg = agg.as_ptr();
        unsafe {
            Value::from_ptr(LLVMBuildExtractValue(
                self.as_ptr(),
                agg,
                index,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }

    pub fn build_insert_value(
        &self,
        agg: impl AnyValue<'ctx, 'p>,
        elt: impl AnyValue<'ctx, 'p>,
        index: u32,
    ) -> Value<'ctx, 'p> {
        let agg = agg.as_ptr();
        let elt = elt.as_ptr();
        unsafe {
            Value::from_ptr(LLVMBuildInsertValue(
                self.as_ptr(),
                agg,
                elt,
                index,
                EMPTY_CSTR.as_ptr(),
            ))
        }
    }
}

impl_opaque_eq!(['ctx, 'p] Builder);

impl_opaque_debug!(['ctx, 'p] Builder);

unsafe_impl_opaque!(['ctx, 'p] Builder(LLVMBuilder));

impl<'ctx, 'p> OpaqueOwn for Builder<'ctx, 'p> {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposeBuilder(a);
    }
}
