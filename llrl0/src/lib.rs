pub mod formatting;
pub mod interning;
pub mod string;
pub mod topological_sort;

pub mod path;

pub mod source_loc;

#[macro_use]
pub mod sexp;

pub mod lexer;
pub mod parser;
pub mod token;

pub mod syntax;

#[macro_use]
pub mod ast;

pub mod pattern_matching;
pub mod unification;

pub mod report;

pub mod source;

pub mod module;

pub mod lowering;

pub mod backend;

pub mod pipeline;

#[cfg(test)]
mod tests;

pub mod prelude {
    pub use super::path::{Error as PathError, ModuleName, PackageName, Path};

    pub use super::source_loc::{SourceLocation, SourceLocationTable};

    pub use super::lexer::Lexer;
    pub use super::parser::{parse, EntireResult as ParseResult, Error as ParseError};
    pub use super::sexp::{Sexp, SexpRep, Ss};

    pub use super::ast;

    pub use super::pattern_matching::Error as PatternMatchingError;
    pub use super::unification::Error as UnificationError;

    pub use super::report::{Phase, Report};

    pub use super::source::{
        collect as collect_sources, Error as SourceError, Loader as SourceLoader, Source, SourceSet,
    };

    pub use super::module::{
        parallel_build as build_modules, Error as ModuleError, Module, ModuleId,
    };

    pub use super::lowering::{Backend, Lowerizer};

    pub use super::backend::default::Backend as DefaultBackend;
    pub use super::backend::{ExecuteMain, Options as BackendOptions, ProduceExecutable};

    pub use super::pipeline::{Error as PipelineError, Pipeline};
}
