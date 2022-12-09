//! Backends responsible for executable code generation and JIT execution.

pub mod native;
pub mod options;
pub mod unit;

#[cfg(feature = "chibi-backend")]
pub mod chibi;
#[cfg(feature = "llvm-backend")]
pub mod llvm;

#[cfg(all(feature = "chibi-backend", not(feature = "llvm-backend")))]
pub use self::chibi as default;

#[cfg(feature = "llvm-backend")]
pub use self::llvm as default;
