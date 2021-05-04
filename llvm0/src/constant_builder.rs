use super::*;

#[macro_export]
macro_rules! llvm_constant {
    ($builder:expr, true) => {
        $builder.bool(true)
    };
    ($builder:expr, false) => {
        $builder.bool(false)
    };
    ($builder:expr, (bool $e:expr)) => {
        $builder.bool($e)
    };
    ($builder:expr, (i8 $e:expr)) => {
        llvm_constant!($builder, (signed i8 $e))
    };
    ($builder:expr, (i16 $e:expr)) => {
        llvm_constant!($builder, (signed i16 $e))
    };
    ($builder:expr, (i32 $e:expr)) => {
        llvm_constant!($builder, (signed i32 $e))
    };
    ($builder:expr, (i64 $e:expr)) => {
        llvm_constant!($builder, (signed i64 $e))
    };
    ($builder:expr, (isize $e:expr)) => {
        llvm_constant!($builder, (signed isize $e))
    };
    ($builder:expr, ((i $bw:expr) $e:expr)) => {
        llvm_constant!($builder, (signed (i $bw) $e))
    };
    ($builder:expr, (intptr $e:expr)) => {
        llvm_constant!($builder, (signed intptr $e))
    };
    ($builder:expr, (signed $ty:tt $e:expr)) => {
        $builder.signed(llvm_type!($builder, $ty), $e as _)
    };
    ($builder:expr, (u8 $e:expr)) => {
        llvm_constant!($builder, (unsigned u8 $e))
    };
    ($builder:expr, (u16 $e:expr)) => {
        llvm_constant!($builder, (unsigned u16 $e))
    };
    ($builder:expr, (u32 $e:expr)) => {
        llvm_constant!($builder, (unsigned u32 $e))
    };
    ($builder:expr, (u64 $e:expr)) => {
        llvm_constant!($builder, (unsigned u64 $e))
    };
    ($builder:expr, (usize $e:expr)) => {
        llvm_constant!($builder, (unsigned usize $e))
    };
    ($builder:expr, ((u $bw:expr) $e:expr)) => {
        llvm_constant!($builder, (unsigned (u $bw) $e))
    };
    ($builder:expr, (unsigned $ty:tt $e:expr)) => {
        $builder.unsigned(llvm_type!($builder, $ty), $e as _)
    };
    ($builder:expr, (fp $ty:tt $e:expr)) => {
        $builder.fp(llvm_type!($builder, $ty), $e as _)
    };
    ($builder:expr, (struct $( $c:tt )*)) => {
        $builder.struct_(&[$( llvm_constant!($builder, $c).as_constant() ),*])
    };
    ($builder:expr, [$x:tt $( $y:tt )*]) => {{
        let x = llvm_constant!($builder, $x);
        $builder.array(x.get_type(), &[
            x.as_constant(),
            $( llvm_constant!($builder, $y).as_constant() ),*
        ])
    }};
    ($builder:expr, (str $e:expr)) => {
        $builder.string($e)
    };
    ($builder:expr, (nullptr $ty:tt)) => {
        $builder.nullptr(llvm_type!($builder, $ty))
    };
    ($builder:expr, (undef $ty:tt)) => {
        $builder.undef(llvm_type!($builder, $ty))
    };
    ($builder:expr, (align_of $ty:tt)) => {
        $builder.align_of(llvm_type!($builder, $ty))
    };
    ($builder:expr, (size_of $ty:tt)) => {
        $builder.size_of(llvm_type!($builder, $ty))
    };
    ($builder:expr, (bit_cast $ty:tt $e:tt)) => {
        $builder.bit_cast(llvm_constant!($builder, $e), llvm_type!($builder, $ty))
    };
    ($builder:expr, (trunc_or_bit_cast $ty:tt $e:tt)) => {
        $builder.trunc_or_bit_cast(llvm_constant!($builder, $e), llvm_type!($builder, $ty))
    };
    ($builder:expr, (int_to_ptr $ty:tt $e:tt)) => {
        $builder.int_to_ptr(llvm_constant!($builder, $e), llvm_type!($builder, $ty))
    };
    ($builder:expr, (ptr_to_int $ty:tt $e:tt)) => {
        $builder.ptr_to_int(llvm_constant!($builder, $e), llvm_type!($builder, $ty))
    };
    ($builder:expr, $e:block) => {
        ($e)
    };
}

pub trait ConstantBuilder<'a>: TypeBuilder<'a> {
    fn bool(&self, value: bool) -> ConstantInt<'a, 'a> {
        ConstantInt::get(
            IntegerType::get(1, self.context()),
            if value { 1 } else { 0 },
            false,
        )
    }

    fn signed(&self, ty: IntegerType<'a>, value: i64) -> ConstantInt<'a, 'a> {
        ConstantInt::get(ty, value as u64, true)
    }

    fn unsigned(&self, ty: IntegerType<'a>, value: u64) -> ConstantInt<'a, 'a> {
        ConstantInt::get(ty, value, false)
    }

    fn fp(&self, ty: FPType<'a>, value: f64) -> ConstantFP<'a, 'a> {
        ConstantFP::get(ty, value)
    }

    fn struct_<'m>(&self, cs: &[Constant<'a, 'm>]) -> ConstantStruct<'a, 'm>
    where
        'a: 'm,
    {
        ConstantStruct::get(cs, false, self.context())
    }

    fn array<'m>(&self, ty: impl AnyType<'a>, cs: &[Constant<'a, 'm>]) -> ConstantArray<'a, 'm>
    where
        'a: 'm,
    {
        ConstantArray::get(ty, cs)
    }

    fn string(&self, s: &str) -> ConstantDataArray<'a, 'a> {
        ConstantDataArray::string(s, true, self.context())
    }

    fn nullptr(&self, ty: PointerType<'a>) -> Constant<'a, 'a> {
        Constant::nullptr(ty)
    }

    fn undef(&self, ty: impl AnyType<'a>) -> Constant<'a, 'a> {
        Constant::undef(ty)
    }

    fn align_of(&self, ty: impl AnyType<'a>) -> Constant<'a, 'a> {
        Constant::align_of(ty)
    }

    fn size_of(&self, ty: impl AnyType<'a>) -> Constant<'a, 'a> {
        Constant::size_of(ty)
    }

    fn bit_cast<'m>(&self, c: impl AnyConstant<'a, 'm>, ty: impl AnyType<'a>) -> Constant<'a, 'm>
    where
        'a: 'm,
    {
        c.bit_cast(ty)
    }

    fn trunc_or_bit_cast<'m>(
        &self,
        c: impl AnyConstant<'a, 'm>,
        ty: impl AnyType<'a>,
    ) -> Constant<'a, 'm>
    where
        'a: 'm,
    {
        c.trunc_or_bit_cast(ty)
    }

    fn int_to_ptr<'m>(&self, c: impl AnyConstant<'a, 'm>, ty: PointerType<'a>) -> Constant<'a, 'm>
    where
        'a: 'm,
    {
        c.int_to_ptr(ty)
    }

    fn ptr_to_int<'m>(&self, c: impl AnyConstant<'a, 'm>, ty: IntegerType<'a>) -> Constant<'a, 'm>
    where
        'a: 'm,
    {
        c.ptr_to_int(ty)
    }
}

impl<'a, T: TypeBuilder<'a>> ConstantBuilder<'a> for T {}

pub trait TargetConstantBuilder<'a>: ConstantBuilder<'a> + TargetTypeBuilder<'a> {}

impl<'a, T: TargetTypeBuilder<'a>> TargetConstantBuilder<'a> for T {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_bool() {
        let ctx = Context::new();
        assert_ne!(llvm_constant!(&ctx, true).zext_value(), 0u64);
        assert_eq!(llvm_constant!(&ctx, false).zext_value(), 0u64);
    }

    #[test]
    fn constant_signed_integer() {
        let ctx = Context::new();
        assert_eq!(
            llvm_constant!(&ctx, (i32 i32::max_value())).sext_value(),
            i32::max_value() as i64
        );
        assert_eq!(
            llvm_constant!(&ctx, (i64 i64::max_value())).sext_value(),
            i64::max_value()
        );
        assert_eq!(
            llvm_constant!(&ctx, (i64 i64::min_value())).sext_value(),
            i64::min_value()
        );
    }

    #[test]
    fn constant_unsigned_integer() {
        let ctx = Context::new();
        assert_eq!(
            llvm_constant!(&ctx, (unsigned u32 u32::min_value())).zext_value(),
            0u64
        );
        assert_eq!(
            llvm_constant!(&ctx, (unsigned u64 u64::max_value())).zext_value(),
            u64::max_value()
        );
    }

    #[test]
    fn constant_fp() {
        let ctx = Context::new();
        assert_eq!(llvm_constant!(&ctx, (fp float 3.5)).double_value(), 3.5);
        assert_eq!(
            llvm_constant!(&ctx, (fp double 1e+42)).double_value(),
            1e+42
        );
    }

    #[test]
    fn constant_tuple() {
        let ctx = Context::new();
        assert_eq!(
            llvm_constant!(&ctx, (struct)).to_string(),
            "{} zeroinitializer"
        );
        assert_eq!(
            llvm_constant!(&ctx, (struct (i32 1) (i32 10) (i32 100))).to_string(),
            "{ i32, i32, i32 } { i32 1, i32 10, i32 100 }"
        );
        assert_eq!(
            llvm_constant!(&ctx, (struct (struct (i8 1) (i16 2)))).to_string(),
            "{ { i8, i16 } } { { i8, i16 } { i8 1, i16 2 } }"
        );
    }

    #[test]
    fn constant_array() {
        let ctx = Context::new();
        assert_eq!(
            llvm_constant!(&ctx, [(i32 100) (i32 1000) (i32 10000) (i32 -100000)]).to_string(),
            "[4 x i32] [i32 100, i32 1000, i32 10000, i32 -100000]"
        );
    }

    #[test]
    fn constant_str() {
        let ctx = Context::new();
        assert_eq!(
            llvm_constant!(&ctx, (str "Hello, World!")).to_string(),
            r#"[14 x i8] c"Hello, World!\00""#
        );
    }
}
