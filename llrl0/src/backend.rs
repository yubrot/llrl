//! Backends responsible for executable code generation and JIT execution.

use std::path;

pub mod interpreter;
#[cfg(feature = "llvm-backend")]
pub mod llvm;
pub mod native;
pub mod unit;

#[cfg(feature = "llvm-backend")]
pub use self::llvm as default;

/// Common backend trait that can generate executable binary.
pub trait ProduceExecutable {
    fn produce_executable(
        &self,
        dest: path::PathBuf,
        clang_options: Vec<String>,
    ) -> Result<String, String>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Options {
    pub optimize: Option<bool>,
    pub verbose: bool,
}

impl Options {
    pub fn optimize(self, optimize: bool) -> Self {
        Self {
            optimize: Some(optimize),
            ..self
        }
    }

    pub fn verbose(self, verbose: bool) -> Self {
        Self { verbose, ..self }
    }
}
