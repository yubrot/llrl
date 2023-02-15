//! A set of definitions common to the backend of the native target.

use std::path;

pub mod calling;
pub mod data;
pub mod linking;
pub mod mem_layout;

/// Native target backend.
pub trait NativeBackend {
    /// Produce an executbale binary.
    fn produce_executable(
        &mut self,
        dest: path::PathBuf,
        clang_options: Vec<String>,
    ) -> Result<(), String>;

    /// Execute the main function.
    /// If the last expression is typed to bool, the result of its evaluation is returned.
    /// Otherwise, false is returned.
    fn execute_main(&mut self) -> Result<bool, String>;
}
