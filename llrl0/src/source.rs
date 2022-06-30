use crate::lexer::Lexer;
use crate::parser::parse;
use crate::path::Path;
use crate::sexp::Ss;
use crate::source_loc::SourceLocator;
use crate::syntax;
use crate::topological_sort;
use std::collections::HashMap;

mod collector;
mod error;
mod loader;
mod set;

pub use collector::collect;
pub use error::Error;
pub use loader::{LoadablePackage, LoadableSource, Loader};
pub use set::SourceSet;

#[cfg(test)]
mod tests;

pub const SOURCE_CODE_EXTENSION: &str = "llrl";

/// A source of a llrl module.
#[derive(Debug, Clone)]
pub struct Source {
    pub path: Path,
    pub implicit_std: bool,
    pub dependencies: HashMap<String, Path>,
    pub rep: SourceRep,
    pub errors: Vec<Error>,
}

impl Source {
    pub fn from_error(path: Path, error: impl Into<Error>) -> Self {
        Self {
            path,
            implicit_std: false,
            dependencies: HashMap::new(),
            rep: SourceRep::Empty,
            errors: vec![error.into()],
        }
    }

    pub fn from_code(path: Path, code: Ss) -> Self {
        let mut dependencies = HashMap::new();
        let mut errors = Vec::new();
        let mut implicit_std = true;

        // Collect the dependencies from (import ..) and (no-implicit-std) declarations.
        for s in code.ss.iter() {
            if let Ok(()) = s.matches::<syntax::NoImplicitStd>() {
                implicit_std = false;
            } else if let Ok(import) = s.matches::<syntax::Import>() {
                match import.path.parse::<Path>() {
                    Ok(mut import_path) => {
                        if import_path.package.is_current() && !path.package.is_current() {
                            // relative to the `path.package`
                            import_path.package = path.package.clone();
                        }
                        if import_path == path {
                            errors.push(Error::CannotImportModuleItself);
                        } else {
                            dependencies.insert(import.path.to_string(), import_path);
                        }
                    }
                    Err(e) => errors.push(Error::InvalidImportPath(import.path.to_string(), e)),
                }
            }
        }

        // Every module except builtin depend on builtin.
        if path != Path::builtin() {
            dependencies.insert(Path::builtin().to_string(), Path::builtin());
        }

        // Module implicitly imports std.
        // This behavior can be disabled by (no-implicit-std).
        if implicit_std {
            if path == Path::std() {
                errors.push(Error::CannotImportModuleItself);
            } else {
                dependencies.insert(Path::std().to_string(), Path::std());
            }
        }

        Self {
            path,
            implicit_std,
            dependencies,
            rep: SourceRep::Code(code),
            errors,
        }
    }

    pub fn from_code_text(path: Path, locator: &mut SourceLocator, code_text: &str) -> Self {
        let parse_result = parse::<Ss, _>(locator, Lexer::new(code_text));
        let mut source = match parse_result.final_result {
            Ok(code) => Self::from_code(path, code),
            Err(parse_error) => Self::from_error(path, parse_error),
        };

        for parse_error in parse_result.inner_errors {
            source.errors.push(parse_error.into());
        }

        source
    }
}

impl topological_sort::DependencyList<Path> for Source {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&Path)) {
        self.dependencies.values().for_each(f);
    }
}

#[derive(Debug, Clone)]
pub enum SourceRep {
    Empty,
    Code(Ss),
}
