//! Provides an intermediate representation for lowering.

mod ct;
mod def;
mod display;
pub mod rewriter;
mod rt;
pub mod traverser;

pub use crate::ast::builtin::{CapturedUse, Sexp, Syntax, SyntaxBody, SyntaxMetadata};
pub use ct::*;
pub use def::*;
pub use rt::*;
