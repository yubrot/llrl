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

/// Common backend builder trait.
pub trait Builder: Sized {
    type Dest;

    fn new() -> Self;

    fn optimize(self, optimize: bool) -> Self;

    fn verbose(self, verbose: bool) -> Self;

    fn build(self) -> Self::Dest;
}
