//! A set of NodeId definitions reserved as language built-ins.

use super::*;
use crate::sexp;
use crate::source_loc::SourceLocation;
use once_cell::sync::Lazy;
use ordered_float::OrderedFloat;
use std::collections::HashMap;

pub fn module() -> String {
    SOURCE_CODE.to_string()
}

pub static SOURCE_CODE: &str = include_str!("builtin.llrl");

pub const LIMIT_VARIADIC_SIZE: usize = 16;

macro_rules! define_builtin_reserved {
    ($( [$index:literal] $id:tt: $ty:ty = $name:literal; )*) => {
        $( pub const $id: NodeId<$ty> = NodeId::new_unchecked(ModuleId::builtin(), $index); )*

        pub static RESERVED_CONSTRUCTS: Lazy<HashMap<String, Construct>> = Lazy::new(|| {
            let mut map = HashMap::new();

            $( map.insert($name.to_string(), $id.into()); )*

            reserve_variadic_constructs(&mut map);

            map
        });
    }
}

fn reserve_variadic_constructs(map: &mut HashMap<String, Construct>) {
    for i in 0..=LIMIT_VARIADIC_SIZE {
        map.insert(format!("Fun.{}", i), fun(i).into());
        map.insert(format!("Tuple.{}", i), tuple_type(i).into());
        map.insert(format!("{}:", i), tuple(i).into());
    }
}

pub fn fun(arity: usize) -> NodeId<BuiltinTypeCon> {
    assert!(
        arity <= LIMIT_VARIADIC_SIZE,
        "Unsupported fun arity: {}",
        arity
    );
    NodeId::new_unchecked(ModuleId::builtin(), arity as u32)
}

pub fn tuple_type(size: usize) -> NodeId<DataTypeCon> {
    assert!(
        size <= LIMIT_VARIADIC_SIZE,
        "Unsupported tuple size: {}",
        size
    );
    NodeId::new_unchecked(ModuleId::builtin(), 100 + size as u32)
}

pub fn tuple(size: usize) -> NodeId<DataValueCon> {
    assert!(
        size <= LIMIT_VARIADIC_SIZE,
        "Unsupported tuple size: {}",
        size
    );
    NodeId::new_unchecked(ModuleId::builtin(), 200 + size as u32)
}

pub fn matches_fun(id: NodeId<BuiltinTypeCon>) -> Option<usize> {
    if id.module().is_builtin() && id.index_in_module() <= LIMIT_VARIADIC_SIZE as u32 {
        Some(id.index_in_module() as usize)
    } else {
        None
    }
}

pub fn matches_tuple_type(id: NodeId<DataTypeCon>) -> Option<usize> {
    if id.module().is_builtin()
        && 100 <= id.index_in_module()
        && id.index_in_module() <= 100 + LIMIT_VARIADIC_SIZE as u32
    {
        Some((id.index_in_module() - 100) as usize)
    } else {
        None
    }
}

pub fn matches_tuple(id: NodeId<DataValueCon>) -> Option<usize> {
    if id.module().is_builtin()
        && 200 <= id.index_in_module()
        && id.index_in_module() <= 200 + LIMIT_VARIADIC_SIZE as u32
    {
        Some((id.index_in_module() - 200) as usize)
    } else {
        None
    }
}

define_builtin_reserved! {
    // 0..99 is reserved for function types
    // 100..199 is reserved for tuple types
    // 200..299 is reserved for tuple values

    [300] BOOL: DataTypeCon = "Bool";
    [301] FALSE: DataValueCon = "false";
    [302] TRUE: DataValueCon = "true";

    [311] I8: BuiltinTypeCon = "I8";
    [312] I16: BuiltinTypeCon = "I16";
    [313] I32: BuiltinTypeCon = "I32";
    [314] I64: BuiltinTypeCon = "I64";

    [321] U8: BuiltinTypeCon = "U8";
    [322] U16: BuiltinTypeCon = "U16";
    [323] U32: BuiltinTypeCon = "U32";
    [324] U64: BuiltinTypeCon = "U64";

    [331] F32: BuiltinTypeCon = "F32";
    [332] F64: BuiltinTypeCon = "F64";

    [340] STRING: BuiltinTypeCon = "String";

    [350] CHAR: BuiltinTypeCon = "Char";

    [360] CAPTURED_USE: BuiltinTypeCon = "CapturedUse";

    [370] OPTION: DataTypeCon = "Option";
    [371] NONE: DataValueCon = "none";
    [372] SOME: DataValueCon = "some";

    [380] RESULT: DataTypeCon = "Result";
    [381] ERR: DataValueCon = "err";
    [382] OK: DataValueCon = "ok";

    [400] NUMBER: ClassCon = "Number";
    [401] NUMBER_CONSTRAINT: Constraint = "#Number";

    [410] NUMBER_I8: InstanceCon = "Number.I8";
    [411] NUMBER_I16: InstanceCon = "Number.I16";
    [412] NUMBER_I32: InstanceCon = "Number.I32";
    [413] NUMBER_I64: InstanceCon = "Number.I64";

    [420] NUMBER_U8: InstanceCon = "Number.U8";
    [421] NUMBER_U16: InstanceCon = "Number.U16";
    [422] NUMBER_U32: InstanceCon = "Number.U32";
    [423] NUMBER_U64: InstanceCon = "Number.U64";

    [430] NUMBER_F32: InstanceCon = "Number.F32";
    [431] NUMBER_F64: InstanceCon = "Number.F64";

    [450] FPNUMBER: ClassCon = "FPNumber";
    [451] FPNUMBER_CONSTRAINT: Constraint = "#FPNumber";

    [480] FPNUMBER_F32: InstanceCon = "FPNumber.F32";
    [481] FPNUMBER_F64: InstanceCon = "FPNumber.F64";

    [500] SYNTAX_TYPE: BuiltinTypeCon = "Syntax";
    [501] SYNTAX_VALUE: BuiltinValueCon = "syntax";

    [510] SEXP: DataTypeCon = "Sexp";
}

pub type CapturedUse = Construct;

pub type Syntax<A> = <A as SyntaxBody>::Packed;

pub type SyntaxMetadata = SourceLocation;

pub trait SyntaxBody {
    type Packed;

    fn pack(self, meta: SyntaxMetadata) -> Self::Packed;

    fn unpack(packed: Self::Packed) -> (SyntaxMetadata, Self);
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Sexp {
    Integer(bool, u64),
    FPNumber(OrderedFloat<f64>),
    Bool(bool),
    Symbol(String),
    String(String),
    Char(char),
    Cons(Syntax<Sexp>, Syntax<Sexp>),
    Nil,
    Use(CapturedUse),
}

impl SyntaxBody for Sexp {
    type Packed = sexp::Sexp;

    fn pack(self, meta: SyntaxMetadata) -> Self::Packed {
        match self {
            Self::Integer(signed, value) => sexp::Sexp::integer(meta, signed, value),
            Self::FPNumber(value) => sexp::Sexp::fpnumber(meta, value),
            Self::Bool(value) => sexp::Sexp::bool(meta, value),
            Self::Symbol(s) => sexp::Sexp::symbol(meta, s),
            Self::String(s) => sexp::Sexp::string(meta, s),
            Self::Char(c) => sexp::Sexp::char(meta, c),
            Self::Cons(car, cdr) => sexp::Sexp::cons(meta, car, cdr),
            Self::Nil => sexp::Sexp::nil(meta),
            Self::Use(use_) => sexp::Sexp::native(meta, use_),
        }
    }

    fn unpack(mut s: Self::Packed) -> (SyntaxMetadata, Self) {
        let meta = s.loc;
        let body = match s.rep {
            sexp::SexpRep::Integer(signed, value) => Self::Integer(signed, value),
            sexp::SexpRep::FPNumber(value) => Self::FPNumber(value),
            sexp::SexpRep::Symbol(s) => Self::Symbol(s.into_owned()),
            sexp::SexpRep::String(s) => Self::String(s.into_owned()),
            sexp::SexpRep::Char(c) => Self::Char(c),
            sexp::SexpRep::List(list) if list.is_empty() => Self::Nil,
            sexp::SexpRep::List(ref mut list) => {
                let car = list.remove(0);
                // TODO: s.loc.start should be set to list[0].loc.start
                Self::Cons(car, s)
            }
            sexp::SexpRep::ListLike(list_like) if list_like.1.is_empty() => {
                Self::Cons(list_like.0, list_like.2)
            }
            sexp::SexpRep::ListLike(ref mut list_like) => {
                let cadr = list_like.1.remove(0);
                let car = std::mem::replace(&mut list_like.0, cadr);
                // TODO: s.loc.start should be set to list_like.0.loc.start
                Self::Cons(car, s)
            }
            sexp::SexpRep::Bool(value) => Self::Bool(value),
            sexp::SexpRep::Native(native) => {
                if let Some(&use_) = native.downcast_ref() {
                    Self::Use(use_)
                } else {
                    panic!("Unknown native: {}", native)
                }
            }
        };
        (meta, body)
    }
}
