//! Temporary backend implementation that does nothing.

use crate::lowering::{self, ir::*};
use crate::report::Report;
use std::sync::Arc;

impl lowering::Backend for () {
    fn put_def(&mut self, _: CtId, _: Arc<Def>) {}

    fn put_main(&mut self, _: Init) {}

    fn execute_macro(&mut self, _: CtId, _: Syntax<Sexp>) -> Result<Syntax<Sexp>, String> {
        Err("Internal error: Unit backend does not support excute_function".to_string())
    }

    fn complete(self, _: &mut Report) {}
}
