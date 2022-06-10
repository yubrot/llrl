use super::*;
use crate::report::Phase;
use crate::sexp::Sexp;
use crate::source::SourceRep;

mod ast_builder;
mod exporter;
mod importer;
mod kind_inference;
mod resolver;
mod type_inference;
mod validator;

pub type Error = super::Error<BuilderErrorContext>;
pub type Result<T> = std::result::Result<T, Error>;

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

impl<'a, E: External> ModuleSet for (&'a Module, &'a E) {
    fn module_of(&self, mid: ModuleId) -> &Module {
        if mid == self.0.id() {
            self.0
        } else {
            self.1.module(mid)
        }
    }
}

pub fn run(module: &mut Module, source: &Source, external: &impl External) -> Result<()> {
    for path in source.dependencies.values() {
        let import_module = external
            .find_module(path)
            .unwrap_or_else(|| panic!("find_module({})", path));

        module.imports.insert(import_module.id());
    }

    match source.rep {
        SourceRep::Empty => {}
        SourceRep::Code(ref code) => {
            module.report.enter_phase(Phase::Import);
            importer::run(module, code, source, external)?;
            module.report.leave_phase(Phase::Import);

            module.report.enter_phase(Phase::BuildAst);
            ast_builder::run(module, code, external)?;
            module.report.leave_phase(Phase::BuildAst);

            module.report.enter_phase(Phase::Resolve);
            resolver::run(module, external)?;
            module.report.leave_phase(Phase::Resolve);

            module.report.enter_phase(Phase::KindInference);
            kind_inference::run(module, external)?;
            module.report.leave_phase(Phase::KindInference);

            module.report.enter_phase(Phase::TypeInference);
            type_inference::run(module, external)?;
            module.report.leave_phase(Phase::TypeInference);

            module.report.enter_phase(Phase::Validate);
            validator::run(module, external)?;
            module.report.leave_phase(Phase::Validate);

            module.report.enter_phase(Phase::Export);
            exporter::run(module, code)?;
            module.report.leave_phase(Phase::Export);
        }
    }

    Ok(())
}
