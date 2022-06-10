use crate::emitter;
use std::path;
use std::process;

pub trait Backend: emitter::Backend {
    fn produce_executable(
        &self,
        dest: path::PathBuf,
        clang_options: Vec<String>,
    ) -> process::Output;
}

pub trait BackendBuilder: Sized {
    type Backend: Backend;

    fn new() -> Self;

    fn optimize(self, optimize: bool) -> Self;

    fn verbose(self, verbose: bool) -> Self;

    fn build(self) -> Self::Backend;
}

#[cfg(feature = "llvm-backend")]
pub use super::llvm as default;
