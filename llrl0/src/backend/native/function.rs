//! Provides bindings to call native targeting backend's functions.

// NOTE: Can we use utilities like libffi to support a wider variety of functions?

use super::{NativeResult, NativeSexp, NativeString, NativeSyntax, NativeValue};
use crate::lowering::ir::*;

pub unsafe fn native_macro(f: *const ()) -> impl Fn(Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
    type NativeMacro = extern "C" fn(
        *const u8,
        NativeSyntax<NativeSexp>,
    ) -> NativeResult<NativeSyntax<NativeSexp>, NativeString>;

    let f = std::mem::transmute::<_, NativeMacro>(f);

    move |sexp| {
        let sexp = NativeSyntax::<NativeSexp>::from_host(sexp);
        f(std::ptr::null(), sexp).into_host()
    }
}

pub unsafe fn native_main(f: *const ()) -> impl Fn() -> bool {
    const EMPTY_ARGV: &[*const u8] = &[std::ptr::null()];

    type NativeMain = extern "C" fn(i32, *const *const u8) -> bool;

    let f = std::mem::transmute::<_, NativeMain>(f);

    move || f(0, EMPTY_ARGV.as_ptr())
}
