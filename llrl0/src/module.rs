use crate::ast;
use crate::path::Path;
use crate::report::Report;
use crate::sexp::Sexp;
use crate::source::Source;
use crate::topological_sort;

mod builder;
mod error;
mod formatter;
mod meaning;
mod scope;
mod sema;
mod set;

pub use builder::{build as build_modules, Backend};
pub use error::{Error, SemaErrorContext, TextualErrorContext};
pub use formatter::Formatter;
pub use meaning::*;
pub use scope::*;
pub use set::ModuleSet;

pub type Result<T> = std::result::Result<T, Error>;

pub use ast::ModuleId;

/// The unit of the compilation and semantic analysis of the llrl programming language.
/// Every construct of the language belongs to one of the modules, and every semantic
/// information is also stored here.
#[derive(Debug, Clone)]
pub struct Module {
    path: Path,
    ast_id_generator: ast::NodeIdGenerator,
    ast_root: ast::Root,
    top_level: TopLevel,
    imports: Imports,
    exports: Exports,
    symbol_map: SymbolMap,
    available_instances: AvailableInstances,
    inferred_kinds: InferredKinds,
    inferred_types: InferredTypes,
    report: Report,
}

impl Module {
    /// Build a module from `Source`.
    pub fn build(mid: ModuleId, source: &Source, external: &impl External) -> Result<Self> {
        use once_cell::sync::Lazy;
        use std::sync::Mutex;

        // Module::build for the builtin will always have the same result. we cache it because
        // we are repeatedly building the builtin in our unit tests. We would like to replace this
        // with a fast build cache mechanism, but for now there are no plan to support separate compilation.
        static BUILTIN_CACHE: Lazy<Mutex<Option<Module>>> = Lazy::new(|| Mutex::new(None));

        if mid == ModuleId::builtin() {
            if let Some(ref cache) = *BUILTIN_CACHE.lock().unwrap() {
                return Ok(cache.clone());
            }
        }

        let mut module = Module::new(mid, source.path.clone());
        match sema::run(&mut module, source, external) {
            Ok(()) => {
                if mid == ModuleId::builtin() {
                    let mut module = module.clone();
                    module.report = Report::new();
                    *BUILTIN_CACHE.lock().unwrap() = Some(module);
                }
                Ok(module)
            }
            Err(e) => Err(e.into_result_error(Formatter::new((&module, external)))),
        }
    }

    pub fn id(&self) -> ModuleId {
        self.ast_root.id.module()
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn ast_root(&self) -> &ast::Root {
        &self.ast_root
    }

    pub fn imports(&self) -> &Imports {
        &self.imports
    }

    pub fn exports(&self) -> &Exports {
        &self.exports
    }

    pub fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }

    pub fn available_instances(&self) -> &AvailableInstances {
        &self.available_instances
    }

    pub fn inferred_kinds(&self) -> &InferredKinds {
        &self.inferred_kinds
    }

    pub fn inferred_types(&self) -> &InferredTypes {
        &self.inferred_types
    }

    pub fn report(&self) -> &Report {
        &self.report
    }

    fn new(mid: ModuleId, path: Path) -> Self {
        let mut ast_id_generator = ast::NodeIdGenerator::in_module(mid);
        let ast_root = ast::Root::new(ast_id_generator.generate());
        Self {
            path,
            ast_id_generator,
            ast_root,
            top_level: TopLevel::new(),
            imports: Imports::new(),
            exports: Exports::new(),
            symbol_map: SymbolMap::new(),
            available_instances: AvailableInstances::new(),
            inferred_kinds: InferredKinds::new(),
            inferred_types: InferredTypes::new(),
            report: Report::new(),
        }
    }

    fn define_function(&mut self, function: ast::Function) -> sema::Result<()> {
        let symbol = self.symbol_map.get(function.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, function.id))?;
        self.ast_root.functions.insert(function.id, function);
        Ok(())
    }

    fn define_c_function(&mut self, c_function: ast::CFunction) -> sema::Result<()> {
        let symbol = self.symbol_map.get(c_function.id).unwrap();
        self.top_level.define(
            &symbol.name,
            LocatedConstruct::new(symbol.loc, c_function.id),
        )?;
        self.ast_root.c_functions.insert(c_function.id, c_function);
        Ok(())
    }

    fn define_builtin_op(&mut self, builtin_op: ast::BuiltinOp) -> sema::Result<()> {
        let symbol = self.symbol_map.get(builtin_op.id).unwrap();
        self.top_level.define(
            &symbol.name,
            LocatedConstruct::new(symbol.loc, builtin_op.id),
        )?;
        self.ast_root.builtin_ops.insert(builtin_op.id, builtin_op);
        Ok(())
    }

    fn define_macro(&mut self, macro_: ast::Macro) -> sema::Result<()> {
        let symbol = self.symbol_map.get(macro_.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, macro_.id))?;
        self.ast_root.macros.insert(macro_.id, macro_);
        Ok(())
    }

    fn define_data_type_con(&mut self, con: ast::DataTypeCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.data_type_cons.insert(con.id, con);
        Ok(())
    }

    fn define_data_value_con(&mut self, con: ast::DataValueCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.data_value_cons.insert(con.id, con);
        Ok(())
    }

    fn define_builtin_type_con(&mut self, con: ast::BuiltinTypeCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.builtin_type_cons.insert(con.id, con);
        Ok(())
    }

    fn define_builtin_value_con(&mut self, con: ast::BuiltinValueCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.builtin_value_cons.insert(con.id, con);
        Ok(())
    }

    fn define_class_con(&mut self, con: ast::ClassCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.class_cons.insert(con.id, con);
        Ok(())
    }

    fn define_class_method(&mut self, method: ast::ClassMethod) -> sema::Result<()> {
        let symbol = self.symbol_map.get(method.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, method.id))?;
        self.ast_root.class_methods.insert(method.id, method);
        Ok(())
    }

    fn define_instance_con(&mut self, con: ast::InstanceCon) -> sema::Result<()> {
        let symbol = self.symbol_map.get(con.id).unwrap();
        self.top_level
            .define(&symbol.name, LocatedConstruct::new(symbol.loc, con.id))?;
        self.ast_root.instance_cons.insert(con.id, con);
        Ok(())
    }

    fn add_instance_method(&mut self, method: ast::InstanceMethod) -> sema::Result<()> {
        self.ast_root.instance_methods.insert(method.id, method);
        Ok(())
    }

    fn add_init_expr(&mut self, expr: ast::InitExpr) {
        self.ast_root.init_expressions.push(expr);
    }
}

impl topological_sort::DependencyList<ModuleId> for Module {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&ModuleId)) {
        self.imports.iter().for_each(f);
    }
}

/// An abstract representation of the outside of the module.
pub trait External {
    fn module(&self, mid: ModuleId) -> &Module;

    fn find_module(&self, path: &Path) -> Option<&Module>;

    fn execute_macro(
        &self,
        id: ast::NodeId<ast::Macro>,
        s: &Sexp,
    ) -> std::result::Result<Sexp, String>;
}
