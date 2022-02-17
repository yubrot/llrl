use crate::emitter::{self, ir::*, Value};
use crate::report::Report;
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
