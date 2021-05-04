use super::*;

#[macro_export]
macro_rules! llvm_type {
    ($builder:expr, void) => {
        $crate::TypeBuilder::void_type(&$builder)
    };
    ($builder:expr, i1) => {
        $crate::TypeBuilder::int_type(&$builder, 1)
    };
    ($builder:expr, i8) => {
        $crate::TypeBuilder::int_type(&$builder, 8)
    };
    ($builder:expr, i16) => {
        $crate::TypeBuilder::int_type(&$builder, 16)
    };
    ($builder:expr, i32) => {
        $crate::TypeBuilder::int_type(&$builder, 32)
    };
    ($builder:expr, i64) => {
        $crate::TypeBuilder::int_type(&$builder, 64)
    };
    ($builder:expr, isize) => {
        $crate::TypeBuilder::int_type(&$builder, std::mem::size_of::<isize>() as u32 * 8)
    };
    ($builder:expr, (i $bw:expr)) => {
        $crate::TypeBuilder::int_type(&$builder, $bw as _)
    };
    ($builder:expr, intptr) => {
        $crate::TargetTypeBuilder::intptr_type(&$builder)
    };
    ($builder:expr, u1) => {
        $crate::TypeBuilder::int_type(&$builder, 1)
    };
    ($builder:expr, u8) => {
        $crate::TypeBuilder::int_type(&$builder, 8)
    };
    ($builder:expr, u16) => {
        $crate::TypeBuilder::int_type(&$builder, 16)
    };
    ($builder:expr, u32) => {
        $crate::TypeBuilder::int_type(&$builder, 32)
    };
    ($builder:expr, u64) => {
        $crate::TypeBuilder::int_type(&$builder, 64)
    };
    ($builder:expr, usize) => {
        $crate::TypeBuilder::int_type(&$builder, std::mem::size_of::<usize>() as u32 * 8)
    };
    ($builder:expr, (u $bw:expr)) => {
        $crate::TypeBuilder::int_type(&$builder, $bw as _)
    };
    ($builder:expr, float) => {
        $crate::TypeBuilder::float_type(&$builder)
    };
    ($builder:expr, double) => {
        $crate::TypeBuilder::double_type(&$builder)
    };
    ($builder:expr, (function (... $params:expr) $ret:tt)) => {
        $crate::TypeBuilder::function_type(
            &$builder,
            &$params,
            llvm_type!($builder, $ret)
        )
    };
    ($builder:expr, (function ($( $param:tt )*) $ret:tt)) => {
        $crate::TypeBuilder::function_type(
            &$builder,
            &[$( llvm_type!($builder, $param).as_type() ),*],
            llvm_type!($builder, $ret)
        )
    };
    ($builder:expr, (struct ... $tys:expr)) => {
        $crate::TypeBuilder::struct_type(
            &$builder,
            &$tys,
        )
    };
    ($builder:expr, (struct $( $element:tt )*)) => {
        $crate::TypeBuilder::struct_type(
            &$builder,
            &[$( llvm_type!($builder, $element).as_type() ),*],
        )
    };
    ($builder:expr, (ptr $ty:tt)) => {
        $crate::TypeBuilder::pointer_type(
            &$builder,
            llvm_type!($builder, $ty),
        )
    };
    ($builder:expr, (typeof $v:expr)) => {
        $crate::AnyValue::get_type($v)
    };
    ($builder:expr, [$ty:tt; $n:expr]) => {
        $crate::TypeBuilder::array_type(
            &$builder,
            $n,
            llvm_type!($builder, $ty),
        )
    };
    ($builder:expr, $e:block) => {
        ($e)
    };
}

pub trait TypeBuilder<'a>: Sized {
    fn context(&self) -> &'a Context;

    fn void_type(&self) -> VoidType<'a> {
        VoidType::get(self.context())
    }

    fn int_type(&self, bit_width: u32) -> IntegerType<'a> {
        IntegerType::get(bit_width, self.context())
    }

    fn float_type(&self) -> FPType<'a> {
        FPType::float(self.context())
    }

    fn double_type(&self) -> FPType<'a> {
        FPType::double(self.context())
    }

    fn function_type(&self, params: &[Type<'a>], ret: impl AnyType<'a>) -> FunctionType<'a> {
        FunctionType::get(ret, params, false)
    }

    fn struct_type(&self, elements: &[Type<'a>]) -> StructType<'a> {
        StructType::get(elements, false, self.context())
    }

    fn pointer_type(&self, ty: impl AnyType<'a>) -> PointerType<'a> {
        PointerType::get(ty, 0)
    }

    fn array_type(&self, len: usize, ty: impl AnyType<'a>) -> ArrayType<'a> {
        ArrayType::get(ty, len)
    }
}

impl<'a> TypeBuilder<'a> for &'a Context {
    fn context(&self) -> &'a Context {
        self
    }
}

impl<'a> TypeBuilder<'a> for Module<'a> {
    fn context(&self) -> &'a Context {
        self.context()
    }
}

impl<'a> TypeBuilder<'a> for &'a Oo<Context> {
    fn context(&self) -> &'a Context {
        &***self
    }
}

pub trait TargetTypeBuilder<'a>: TypeBuilder<'a> {
    fn data_layout(&self) -> &DataLayout;

    fn intptr_type(&self) -> IntegerType<'a> {
        self.data_layout().int_ptr_type(None, self.context())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, i1).bit_width(), 1);
        assert_eq!(llvm_type!(&ctx, i8).bit_width(), 8);
        assert_eq!(llvm_type!(&ctx, i16).bit_width(), 16);
        assert_eq!(llvm_type!(&ctx, i32).bit_width(), 32);
        assert_eq!(llvm_type!(&ctx, i64).bit_width(), 64);
        assert_eq!(llvm_type!(&ctx, u1).bit_width(), 1);
        assert_eq!(llvm_type!(&ctx, u8).bit_width(), 8);
        assert_eq!(llvm_type!(&ctx, u16).bit_width(), 16);
        assert_eq!(llvm_type!(&ctx, u32).bit_width(), 32);
        assert_eq!(llvm_type!(&ctx, u64).bit_width(), 64);
    }

    #[test]
    fn fp_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, float).to_string(), "float");
        assert_eq!(llvm_type!(&ctx, double).to_string(), "double");
    }

    #[test]
    fn function_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, (function() i1)).to_string(), "i1 ()");
        assert_eq!(
            llvm_type!(&ctx, (function(i8 i16) i32)).to_string(),
            "i32 (i8, i16)"
        );
        assert_eq!(
            llvm_type!(&ctx, (function((function(u8) u16)) (function(u32) u64))).to_string(),
            "i64 (i32) (i16 (i8))"
        );
    }

    #[test]
    fn tuple_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, (struct)).to_string(), "{}");
        assert_eq!(llvm_type!(&ctx, (struct i8)).to_string(), "{ i8 }");
        assert_eq!(
            llvm_type!(&ctx, (struct i1 i1 i8)).to_string(),
            "{ i1, i1, i8 }"
        );
    }

    #[test]
    fn pointer_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, (ptr i8)).to_string(), "i8*");
        assert_eq!(llvm_type!(&ctx, (ptr (ptr i16))).to_string(), "i16**");
    }

    #[test]
    fn array_types() {
        let ctx = Context::new();
        assert_eq!(llvm_type!(&ctx, [i8; 4]).to_string(), "[4 x i8]");
        assert_eq!(
            llvm_type!(&ctx, [[i16; 2]; 4]).to_string(),
            "[4 x [2 x i16]]"
        );
    }
}
