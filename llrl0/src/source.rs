use crate::lexer::Lexer;
use crate::parser::parse;
use crate::path::Path;
use crate::preprocess::Preprocessor;
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
    pub fn empty(path: Path) -> Self {
        Self {
            path,
            implicit_std: true,
            dependencies: HashMap::new(),
            rep: SourceRep::Empty,
            errors: Vec::new(),
        }
    }

    pub fn from_error(path: Path, error: impl Into<Error>) -> Self {
        let mut source = Self::empty(path);
        source.errors.push(error.into());
        source
    }

    pub fn from_code(path: Path, code: Ss) -> Self {
        let mut source = Self::empty(path);
        source.rep = SourceRep::Code(code);
        source
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

    pub fn preprocess(&mut self, pp: &Preprocessor) {
        let code = match self.rep {
            SourceRep::Empty => return,
            SourceRep::Code(ref mut code) => code,
        };

        match pp.run(code) {
            Ok(()) => {}
            Err(e) => self.errors.push(e.into()),
        }
    }

    pub fn resolve_dependencies(&mut self) {
        let code = match self.rep {
            SourceRep::Empty => return,
            SourceRep::Code(ref mut code) => code,
        };

        // Collect the dependencies from (import ..) and (no-implicit-std) declarations.
        for s in code.ss.iter() {
            if let Ok(()) = s.matches::<syntax::NoImplicitStd>() {
                self.implicit_std = false;
            } else if let Ok(import) = s.matches::<syntax::Import>() {
                match import.path.parse::<Path>() {
                    Ok(mut import_path) => {
                        if import_path.package.is_current() && !self.path.package.is_current() {
                            // relative to the `path.package`
                            import_path.package = self.path.package.clone();
                        }
                        if import_path == self.path {
                            self.errors.push(Error::CannotImportModuleItself);
                        } else {
                            self.dependencies
                                .insert(import.path.to_string(), import_path);
                        }
                    }
                    Err(e) => {
                        self.errors
                            .push(Error::InvalidImportPath(import.path.to_string(), e));
                    }
                }
            }
        }

        // Every module except builtin depend on builtin.
        if self.path != Path::builtin() {
            self.dependencies
                .insert(Path::builtin().to_string(), Path::builtin());
        }

        // Module implicitly imports std.
        // This behavior can be disabled by (no-implicit-std).
        if self.implicit_std {
            if self.path == Path::std() {
                self.errors.push(Error::CannotImportModuleItself);
            } else {
                self.dependencies
                    .insert(Path::std().to_string(), Path::std());
            }
        }
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
