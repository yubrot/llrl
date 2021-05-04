use crate::emitter::{self, ir::*};
use crate::report::Report;
use std::fmt;
use std::sync::Arc;

impl emitter::Backend for () {
    type Value = Value;

    fn put_def(&mut self, _: CtId, _: Arc<CtDef>) {}

    fn put_main(&mut self, _: Init) {}

    fn execute_main(&mut self) -> Result<Self::Value, String> {
        Err("Internal error: Unit backend does not support execute_main".to_string())
    }

    fn execute_function(&mut self, _: CtId, _: Vec<Value>) -> Result<Value, String> {
        Err("Internal error: Unit backend does not support excute_function".to_string())
    }

    fn complete(self, _: &mut Report) {}
}

#[derive(Debug, Clone)]
pub struct Value;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "value")
    }
}

impl emitter::BackendValue for Value {
    fn as_bool(&self) -> Option<bool> {
        None
    }

    fn from_macro_src(_: &Syntax<Sexp>) -> Self {
        Self
    }

    fn into_macro_dest(self) -> Result<Syntax<Sexp>, String> {
        Err("Internal error: Unit backend does not support into_macro_dest".to_string())
    }
}
