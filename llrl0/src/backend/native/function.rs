//! Provides bindings to call native targeting backend's functions.

// NOTE: Can we use utilities like libffi to support a wider variety of functions?

use super::{NativeResult, NativeSexp, NativeString, NativeSyntax, NativeValue};
use crate::lowering::ir::*;

#[allow(clippy::missing_safety_doc)]
pub unsafe fn native_macro(f: *const ()) -> impl Fn(Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
    type NativeMacro = extern "C" fn(
        NativeSyntax<NativeSexp>,
    ) -> NativeResult<NativeSyntax<NativeSexp>, NativeString>;

    let f = std::mem::transmute::<_, NativeMacro>(f);

    move |sexp| {
        let sexp = NativeSyntax::<NativeSexp>::from_host(sexp);
        f(sexp).into_host()
    }
}

#[allow(clippy::missing_safety_doc)]
#[allow(clippy::redundant_closure)]
pub unsafe fn native_main(f: *const ()) -> impl Fn() -> bool {
    type NativeMain = extern "C" fn() -> bool;

    let f = std::mem::transmute::<_, NativeMain>(f);

    move || f()
}

/// Native function calling convention.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum CallConv {
    Default,
    Macro, // NativeMacro
    Main,  // NativeMain
}

impl CallConv {
    pub fn takes_env_as_argument(self) -> bool {
        matches!(self, Self::Default)
    }

    pub fn returns_by_pointer_store(self) -> bool {
        // Macros are called from Rust code with C-compatible ABI, so the result of the macro functions
        // must be returned through pointers. The main function is also called from Rust with
        // C-compatible ABI, but the type of the return value used in tests is bool (i8), which can
        // be returned directly as C-compatible form.
        matches!(self, Self::Macro)
    }
}

impl From<FunctionKind> for CallConv {
    fn from(kind: FunctionKind) -> Self {
        match kind {
            FunctionKind::Standard | FunctionKind::Transparent => Self::Default,
            FunctionKind::Macro => Self::Macro,
            FunctionKind::Main => Self::Main,
        }
    }
}
