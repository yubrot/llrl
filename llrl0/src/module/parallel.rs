use super::{Error, Module, ModuleId, External};
use crate::ast::{self, builtin};
use crate::path::Path;
use crate::report::{Phase, Report};
use crate::source::Source;
use either::*;
use itertools::Itertools;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, RwLock};
use typed_arena::Arena;

/// Compiler backend used by `build`.
pub trait Backend: Sync {
    /// Add a built module. Built modules are added from this method without omission.
    fn add_module(&self, module: Arc<Module>, is_entry_point: bool);

    /// Execute the macro. The macro to be executed always resides in the module added with `add_module`.
    fn execute_macro(
        &self,
        id: ast::NodeId<ast::Macro>,
        s: &builtin::Syntax<builtin::Sexp>,
    ) -> Result<builtin::Syntax<builtin::Sexp>, String>;
}

impl Backend for () {
    fn add_module(&self, _module: Arc<Module>, _is_entry_point: bool) {}

    fn execute_macro(
        &self,
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
    backend: &impl Backend,
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

    let mut path_to_module = HashMap::<Path, ModuleId>::new();
    let mut modules = Vec::<BuildingModule>::with_capacity(sources.len());

    for source in sources {
        let mid = ModuleId::from_index(modules.len());
        let module = BuildingModule::new(mid, source);

        for dep in module.source.dependencies.values() {
            if let Some(dep_id) = path_to_module.get(dep) {
                module.leader_count.fetch_add(1, Ordering::SeqCst);
                modules[dep_id.as_index()].followers.push(module.mid);
            }
        }

        path_to_module.insert(module.source.path.clone(), mid);
        modules.push(module);
    }

    report.enter_phase(Phase::BuildModule);
    let context = BuildingContext::new(&modules, &path_to_module, &entry_points, backend);
    rayon::scope(|scope| context.run(scope));
    report.leave_phase(Phase::BuildModule);

    modules
        .into_iter()
        .partition_map(|module| match module.result.into_inner().unwrap() {
            Ok(module) => {
                report.merge(module.report());
                Left(module)
            }
            Err(error) => Right((module.source, error)),
        })
}

#[derive(Debug)]
struct BuildingModule {
    mid: ModuleId,
    source: Source,
    leader_count: AtomicU32,
    followers: Vec<ModuleId>,
    result: RwLock<Result<Arc<Module>, Error>>,
}

impl BuildingModule {
    fn new(mid: ModuleId, source: Source) -> Self {
        Self {
            mid,
            source,
            leader_count: AtomicU32::new(1),
            followers: Vec::new(),
            result: RwLock::new(Err(Error::DependentModuleBuildFailed)),
        }
    }
}

#[derive(Debug)]
struct BuildingContext<'a, B> {
    modules: &'a Vec<BuildingModule>,
    path_to_module: &'a HashMap<Path, ModuleId>,
    entry_points: &'a HashSet<Path>,
    backend: &'a B,
}

impl<'a, B: Backend> Clone for BuildingContext<'a, B> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, B: Backend> Copy for BuildingContext<'a, B> {}

impl<'a, B: Backend> BuildingContext<'a, B> {
    fn new(
        modules: &'a Vec<BuildingModule>,
        path_to_module: &'a HashMap<Path, ModuleId>,
        entry_points: &'a HashSet<Path>,
        backend: &'a B,
    ) -> Self {
        Self {
            modules,
            path_to_module,
            entry_points,
            backend,
        }
    }

    fn run(self, scope: &rayon::Scope<'a>) {
        for module in self.modules.iter() {
            self.release_dependency_one(module, scope);
        }
    }

    fn release_dependency_one(self, module: &'a BuildingModule, scope: &rayon::Scope<'a>) {
        if module.leader_count.fetch_sub(1, Ordering::SeqCst) != 1 {
            return;
        }

        scope.spawn(move |scope| {
            let deps_arena = Arena::new();
            let external = ModuleBuildingContext::new(self, &deps_arena);
            match Module::build(module.mid, &module.source, &external) {
                Ok(built_module) => {
                    let built_module = Arc::new(built_module);
                    let is_entry_point = self.entry_points.contains(built_module.path());
                    self.backend
                        .add_module(Arc::clone(&built_module), is_entry_point);
                    *module.result.write().unwrap() = Ok(built_module);
                    for id in module.followers.iter() {
                        self.release_dependency_one(&self.modules[id.as_index()], scope);
                    }
                }
                Err(err) => {
                    *module.result.write().unwrap() = Err(err);
                }
            }
        });
    }
}

struct ModuleBuildingContext<'a, B> {
    ctx: BuildingContext<'a, B>,
    deps_arena: &'a Arena<Arc<Module>>,
    deps_cache: RefCell<HashMap<ModuleId, &'a Module>>,
}

impl<'a, B: Backend> ModuleBuildingContext<'a, B> {
    fn new(ctx: BuildingContext<'a, B>, deps_arena: &'a Arena<Arc<Module>>) -> Self {
        Self {
            ctx,
            deps_arena,
            deps_cache: RefCell::new(HashMap::new()),
        }
    }
}

impl<'a, B: Backend> External for ModuleBuildingContext<'a, B> {
    fn module(&self, mid: ModuleId) -> &Module {
        let ctx = self.ctx;
        let arena = self.deps_arena;
        *self.deps_cache.borrow_mut().entry(mid).or_insert_with(|| {
            let module = ctx.modules.get(mid.as_index()).unwrap();
            arena.alloc(Arc::clone(&module.result.read().unwrap().as_ref().unwrap()))
        })
    }

    fn find_module(&self, path: &Path) -> Option<&Module> {
        let id = *self.ctx.path_to_module.get(path)?;
        Some(self.module(id))
    }

    fn execute_macro(
        &self,
        id: ast::NodeId<ast::Macro>,
        s: &builtin::Syntax<builtin::Sexp>,
    ) -> Result<builtin::Syntax<builtin::Sexp>, String> {
        self.ctx.backend.execute_macro(id, s)
    }
}
