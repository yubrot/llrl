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
mod map;

pub use collector::collect;
pub use error::Error;
pub use loader::{LoadablePackage, LoadableSource, Loader};
pub use map::CodeMap;

#[cfg(test)]
mod tests;

pub const SOURCE_CODE_EXTENSION: &'static str = "llrl";

/// `Code` is a representation to describe a llrl module.
#[derive(Debug, Clone)]
pub struct Code {
    pub path: Path,
    pub implicit_std: bool,
    pub dependencies: HashMap<String, Path>,
    pub rep: CodeRep,
    pub errors: Vec<Error>,
}

impl Code {
    pub fn from_error(path: Path, error: impl Into<Error>) -> Self {
        Self {
            path,
            implicit_std: false,
            dependencies: HashMap::new(),
            rep: CodeRep::Empty,
            errors: vec![error.into()],
        }
    }

    pub fn from_source(path: Path, source: Ss) -> Self {
        let mut dependencies = HashMap::new();
        let mut errors = Vec::new();
        let mut implicit_std = true;

        for s in source.ss.iter() {
            if let Ok(()) = s.matches::<syntax::NoImplicitPrelude>() {
                implicit_std = false;
            } else if let Ok(import) = s.matches::<syntax::Import>() {
                match import.path.parse::<Path>() {
                    Ok(mut import_path) => {
                        if import_path.package.is_current() && !path.package.is_current() {
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
            rep: CodeRep::Source(source),
            errors,
        }
    }

    pub fn from_source_text(path: Path, locator: &mut SourceLocator, source_text: &str) -> Self {
        let parse_result = parse::<Ss, _>(locator, Lexer::new(source_text));
        let mut code = match parse_result.final_result {
            Ok(source) => Self::from_source(path, source),
            Err(parse_error) => Self::from_error(path, parse_error),
        };

        for parse_error in parse_result.inner_errors {
            code.errors.push(parse_error.into());
        }

        code
    }
}

impl topological_sort::DependencyList<Path> for Code {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&Path)) {
        self.dependencies.values().for_each(f);
    }
}

#[derive(Debug, Clone)]
pub enum CodeRep {
    Empty,
    Source(Ss),
}
