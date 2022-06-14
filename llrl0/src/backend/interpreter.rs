use crate::emitter::{self, ir::*};
use crate::report::Report;
use std::collections::HashMap;
use std::sync::Arc;

mod error;
mod evaluator;
mod value;

pub use error::Error;
pub use evaluator::Evaluator;
pub use value::Value;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Default)]
pub struct Backend {
    defs: HashMap<CtId, Arc<CtDef>>,
    main: Vec<Init>,
}

impl Backend {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn defs(&self) -> &HashMap<CtId, Arc<CtDef>> {
        &self.defs
    }
}

impl emitter::Backend for Backend {
    fn put_def(&mut self, id: CtId, def: Arc<CtDef>) {
        self.defs.insert(id, def);
    }

    fn put_main(&mut self, init: Init) {
        self.main.push(init);
    }

    fn execute_main(&mut self) -> std::result::Result<bool, String> {
        let mut result = Value::Unit;
        for init in self.main.iter() {
            result = Evaluator::new(self)
                .eval(&init.expr)
                .map_err(|e| e.to_string())?;
        }
        result.test().map_err(|e| e.to_string())
    }

    fn execute_macro(
        &mut self,
        id: CtId,
        s: Syntax<Sexp>,
    ) -> std::result::Result<Syntax<Sexp>, String> {
        Evaluator::new(self)
            .eval_call(Value::Clos(id, None), vec![Value::from_syntax_sexp(s)])
            .and_then(Value::into_result_syntax_sexp_string)
            .map_err(|e| e.to_string())?
    }

    fn complete(self, _: &mut Report) {}
}
