use super::{Error, External, Module, ModuleId};
use crate::ast::{self, builtin};
use crate::path::Path;
use crate::report::{Phase, Report};
use crate::source::Source;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Compiler backend used by `build`.
pub trait Backend {
    /// Add a built module. Built modules are added from this method without omission.
    fn add_module(&mut self, module: Arc<Module>, is_entry_point: bool);

    /// Execute the macro. The macro to be executed always resides in the module added with `add_module`.
    fn execute_macro(
        &mut self,
        id: ast::NodeId<ast::Macro>,
        s: &builtin::Syntax<builtin::Sexp>,
    ) -> Result<builtin::Syntax<builtin::Sexp>, String>;
}

impl Backend for () {
    fn add_module(&mut self, _module: Arc<Module>, _is_entry_point: bool) {}

    fn execute_macro(
        &mut self,
        _id: ast::NodeId<ast::Macro>,
        _s: &builtin::Syntax<builtin::Sexp>,
    ) -> Result<builtin::Syntax<builtin::Sexp>, String> {
        Err("Unsupported".to_string())
    }
}

/// Build a set of modules from the sources sorted by dependency.
///
/// Returns a set of modules that were successfully built and a set of sources and errors in case of build failure.
pub fn build(
    sources: Vec<Source>,
    entry_points: HashSet<Path>,
    backend: &mut impl Backend,
    report: &mut Report,
) -> (Vec<Arc<Module>>, Vec<(Source, Error)>) {
    if sources.is_empty() {
        return Default::default();
    }

    assert!(
        // ModuleId(0) must be builtin
        sources[0].path == Path::builtin() &&
        // Every other module must depend on builtin
        sources
            .iter()
            .skip(1)
            .all(|source| source.dependencies.contains_key(&Path::builtin().to_string())) &&
        // Every implicit_std enabled module must depend on std
        sources
            .iter()
            .all(|source|
                !source.implicit_std ||
                source.dependencies.contains_key(&Path::std().to_string())
            )
    );

    let mut ctx = BuildContext {
        modules: Vec::with_capacity(sources.len()),
        path_to_module: HashMap::new(),
        backend,
    };

    for source in sources {
        let mid = ModuleId::from_index(ctx.modules.len());
        let module = match report.on(Phase::BuildModule, || Module::build(mid, &source, &mut ctx)) {
            Ok(module) => Arc::new(module),
            Err(e) => return (ctx.modules, vec![(source, e)]),
        };
        report.merge(module.report());
        ctx.modules.push(Arc::clone(&module));
        ctx.path_to_module.insert(source.path.clone(), mid);
        ctx.backend
            .add_module(module, entry_points.contains(&source.path));
    }

    (ctx.modules, Vec::new())
}

#[derive(Debug)]
struct BuildContext<'b, B> {
    modules: Vec<Arc<Module>>,
    path_to_module: HashMap<Path, ModuleId>,
    backend: &'b mut B,
}

impl<'a, B: Backend> External for BuildContext<'a, B> {
    fn module(&self, mid: ModuleId) -> &Module {
        self.modules.get(mid.as_index()).unwrap().as_ref()
    }

    fn find_module(&self, path: &Path) -> Option<&Module> {
        let mid = *self.path_to_module.get(path)?;
        Some(self.module(mid))
    }

    fn execute_macro(
        &mut self,
        id: ast::NodeId<ast::Macro>,
        s: &builtin::Syntax<builtin::Sexp>,
    ) -> Result<builtin::Syntax<builtin::Sexp>, String> {
        self.backend.execute_macro(id, s)
    }
}
