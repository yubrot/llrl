use crate::formatting::ContextualDisplay;
use crate::parser;
use crate::path;
use crate::preprocess;
use crate::source_loc::SourceLocationTable;
use std::fmt;

/// The error that occurred during the construction of the `Source`.
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Error {
    PackageNotFound,
    ModuleNotFound,
    LoadFailed(String),
    ParseFailed(parser::Error),
    PreprocessFailed(preprocess::Error),
    InvalidImportPath(String, path::Error),
    CannotImportModuleItself,
}

impl From<parser::Error> for Error {
    fn from(error: parser::Error) -> Self {
        Self::ParseFailed(error)
    }
}

impl From<preprocess::Error> for Error {
    fn from(error: preprocess::Error) -> Self {
        Self::PreprocessFailed(error)
    }
}

impl ContextualDisplay<SourceLocationTable> for Error {
    fn fmt_with(&self, ctx: &SourceLocationTable, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::PackageNotFound => write!(f, "Package not found"),
            Error::ModuleNotFound => write!(f, "Module not found"),
            Error::LoadFailed(ref e) => write!(f, "Load failed: {}", e),
            Error::ParseFailed(ref e) => write!(f, "Parse failed: {}", e),
            Error::PreprocessFailed(ref e) => write!(f, "Preprocess failed: {}", e.fmt_on(ctx)),
            Error::InvalidImportPath(ref s, ref e) => {
                write!(f, "Invalid import path \"{}\": {}", s, e)
            }
            Error::CannotImportModuleItself => write!(f, "Cannot import module itself"),
        }
    }
}
