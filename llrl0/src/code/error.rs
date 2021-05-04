use crate::parser;
use crate::path;

/// The error that occurred during the construction of the `Code`.
#[derive(PartialEq, PartialOrd, thiserror::Error, Debug, Clone)]
pub enum Error {
    #[error("Package not found")]
    PackageNotFound,

    #[error("Module not found")]
    ModuleNotFound,

    #[error("Load failed: {0}")]
    LoadFailed(String),

    #[error("Parse failed: {0}")]
    ParseFailed(#[from] parser::Error),

    #[error("Invalid import path \"{0}\": {1}")]
    InvalidImportPath(String, path::Error),

    #[error("Cannot import module itself")]
    CannotImportModuleItself,
}
