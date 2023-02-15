//! Lowering implementation.
//! Converting high-level module constructs to low-level representations.

use crate::ast;
use crate::module::{Backend as ModuleBackend, Module, ModuleId};
use crate::report::{Phase, Report};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

mod branch_expander;
mod context;
mod data_expander;
mod heap2stack;
pub mod ir;
mod normalizer;
mod translator;

pub use context::Context;

/// Low-level compiler backend used by the `Lowerizer`.
pub trait Backend {
    fn put_def(&mut self, id: ir::CtId, def: Arc<ir::Def>);

    fn put_main(&mut self, init: ir::Init);

    fn execute_macro(
        &mut self,
        id: ir::CtId,
        s: ir::Syntax<ir::Sexp>,
    ) -> Result<ir::Syntax<ir::Sexp>, String>;

    fn complete(self, report: &mut Report);
}

#[derive(Debug)]
pub struct Lowerizer<B: Backend> {
    backend: B,
    report: Report,
    modules: HashMap<ModuleId, Arc<Module>>,
    initialized_modules: HashSet<ModuleId>,
    ctx: Context,
}

impl<B: Backend> Lowerizer<B> {
    pub fn new(backend: B) -> Self {
        Self {
            backend,
            report: Report::new(),
            modules: HashMap::new(),
            initialized_modules: HashSet::new(),
            ctx: Context::new(),
        }
    }

    pub fn complete(self, report: &mut Report) -> B {
        report.merge(&self.report);
        self.backend
    }

    fn populate<T>(&mut self, src: &T) -> T::Dest
    where
        T: translator::Translate,
        T::Dest: ir::traverser::Traverse + ir::rewriter::Rewrite,
    {
        let (result, defs) = self
            .report
            .on(Phase::Lowerize, || self.ctx.populate(&src, &self.modules));
        for (id, def) in defs {
            self.backend.put_def(id, Arc::clone(def));
        }
        result
    }

    fn entry_module(&mut self, mid: ModuleId) {
        if !self.initialized_modules.insert(mid) {
            return;
        }
        let module = Arc::clone(&self.modules[&mid]);
        for init_expr in module.ast_root().init_expressions.iter() {
            match init_expr {
                expr @ ast::InitExpr::Eval(_) => {
                    if let Some(init) = self.populate(expr) {
                        self.backend.put_main(init);
                    }
                }
                ast::InitExpr::EnsureInitialized(mid) => self.entry_module(*mid),
            }
        }
    }
}

impl<B: Backend> ModuleBackend for Lowerizer<B> {
    fn add_module(&mut self, module: Arc<Module>, is_entry_point: bool) {
        let mid = module.id();
        self.modules.insert(mid, module);
        if is_entry_point {
            self.entry_module(mid);
        }
    }

    fn execute_macro(
        &mut self,
        id: ast::NodeId<ast::Macro>,
        s: &ir::Syntax<ir::Sexp>,
    ) -> Result<ir::Syntax<ir::Sexp>, String> {
        let f = self.populate(&id);
        self.backend.execute_macro(f.id(), s.clone())
    }
}
