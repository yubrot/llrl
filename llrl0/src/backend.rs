//! Backends responsible for executable code generation and JIT execution.

pub mod native;
pub mod options;
pub mod unit;

#[cfg(feature = "llvm-backend")]
pub mod llvm;
#[cfg(feature = "chibi-backend")]
pub mod chibi;
#[cfg(feature = "interpreter-backend")]
pub mod interpreter;

#[cfg(feature = "llvm-backend")]
pub use self::llvm as default;
#[cfg(all(feature = "chibi-backend", not(feature = "llvm-backend")))]
pub use self::chibi as default;
#[cfg(all(feature = "interpreter-backend", not(feature = "llvm-backend"), not(feature = "chibi-backend")))]
pub use self::chibi as default;
