//! Provides bindings to call functions in the execution environment with emitter::Value.

// NOTE: The dynamic conversion between emitter::Value and concrete type values and the binding
// of functions in the execution environment can be separated, but since emitter::Backend has an
// interface to handle dynamic values, this implementation combines these two processes at present.
// NOTE: Can we use utilities like libffi to support a wider variety of functions?

use super::{EeResult, EeSexp, EeString, EeSyntax, EeValue};
use crate::emitter::{ir::*, Value};

pub unsafe fn bind_macro(f: *const ()) -> Box<dyn Fn(Vec<Value>) -> Result<Value, String>> {
    bind::<(Env, Syntax<Sexp>), Result<Syntax<Sexp>, String>>(f)
}

pub unsafe fn bind_main(f: *const ()) -> Box<dyn Fn(Vec<Value>) -> Result<Value, String>> {
    bind::<(i32, Argv), bool>(f)
}

/// Gives bindings for the specified function pointer.
///
/// # Safety
/// The caller must ensure that the pointer points to a function and conforms to the specified type.
unsafe fn bind<Args, Ret>(f: *const ()) -> Box<dyn Fn(Vec<Value>) -> Result<Value, String>>
where
    Args: FunArgs<Ret>,
    Ret: FunValue,
{
    Box::new(move |args| Args::call(f, args))
}

trait FunArgs<Ret> {
    unsafe fn call(f: *const (), args: Vec<Value>) -> Result<Value, String>;
}

macro_rules! impl_executor_fun_args {
    ($l:literal, $( $name:ident: $tp:ident, )*) => {
        impl<Ret: FunValue, $( $tp: FunValue ),*> FunArgs<Ret> for ($( $tp, )*) {
            unsafe fn call(f: *const (), mut args: Vec<Value>) -> Result<Value, String> {
                let f = std::mem::transmute::<_, extern "C" fn($( $tp::EeValue, )*) -> Ret::EeValue>(f);

                if args.len() != $l {
                    Err(format!("Expected {} arguments but got {} arguments", $l, args.len()))?;
                }
                args.reverse();
                $( let $name = $tp::into_ee(args.pop().unwrap())?; )*
                let ret = f($( $name, )*);
                Ok(Ret::from_ee(ret))
            }
        }
    };
}

impl_executor_fun_args!(0,);
impl_executor_fun_args!(1, a: A,);
impl_executor_fun_args!(2, a: A, b: B,);
impl_executor_fun_args!(3, a: A, b: B, c: C,);
impl_executor_fun_args!(4, a: A, b: B, c: C, d: D,);
impl_executor_fun_args!(5, a: A, b: B, c: C, d: D, e: E,);

trait FunValue {
    type EeValue;

    fn into_ee(value: Value) -> Result<Self::EeValue, String>;

    fn from_ee(value: Self::EeValue) -> Value;
}

impl FunValue for bool {
    type EeValue = bool;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::I(n) => Ok(if n == 0 { false } else { true }),
            v => Err(format!("Cannot treat {} as bool", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        Value::I(if value { 1 } else { 0 })
    }
}

impl FunValue for i32 {
    type EeValue = i32;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::I(n) => Ok(n as i32),
            v => Err(format!("Cannot treat {} as i32", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        Value::I(value as i64)
    }
}

#[derive(Debug, Clone, Copy)]
struct Argv;

const EMPTY_ARGV: &[*const u8] = &[std::ptr::null()];

impl FunValue for Argv {
    type EeValue = *const *const u8;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::EmptyArgv => Ok(EMPTY_ARGV.as_ptr()),
            v => Err(format!("Cannot treat {} as argv", v)),
        }
    }

    fn from_ee(_: Self::EeValue) -> Value {
        Value::Unknown
    }
}

#[derive(Debug, Clone, Copy)]
struct Env;

impl FunValue for Env {
    type EeValue = *const u8;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::Null => Ok(std::ptr::null_mut()),
            v => Err(format!("Cannot treat {} as env", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        if value.is_null() {
            Value::Null
        } else {
            Value::Unknown
        }
    }
}

impl FunValue for Syntax<Sexp> {
    type EeValue = EeSyntax<EeSexp>;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::SyntaxSexp(syntax_sexp) => Ok(Self::EeValue::from_host(syntax_sexp)),
            v => Err(format!("Cannot treat {} as Syntax<Sexp>", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        Value::SyntaxSexp(value.into_host())
    }
}

impl FunValue for String {
    type EeValue = EeString;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::String(s) => Ok(Self::EeValue::from_host(s)),
            v => Err(format!("Cannot treat {} as String", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        Value::String(value.into_host())
    }
}

impl<T: FunValue, E: FunValue> FunValue for Result<T, E> {
    type EeValue = EeResult<T::EeValue, E::EeValue>;

    fn into_ee(value: Value) -> Result<Self::EeValue, String> {
        match value {
            Value::Result(Ok(t)) => Ok(EeResult::ok(T::into_ee(*t)?)),
            Value::Result(Err(e)) => Ok(EeResult::err(E::into_ee(*e)?)),
            v => Err(format!("Cannot treat {} as Result<_, _>", v)),
        }
    }

    fn from_ee(value: Self::EeValue) -> Value {
        match value.into_inner() {
            Ok(t) => Value::Result(Ok(Box::new(T::from_ee(t)))),
            Err(e) => Value::Result(Err(Box::new(E::from_ee(e)))),
        }
    }
}
