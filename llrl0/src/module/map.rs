use super::{Module, ModuleId, TextualUnit};
use crate::ast;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

pub trait ModuleMap {
    fn module_of(&self, mid: ModuleId) -> &Module;

    fn ast<'a, T: ast::RootConstruct<'a>>(&'a self, id: ast::NodeId<T>) -> Option<T::Dest> {
        self.module_of(id.module()).ast_root().get(id)
    }

    fn textual_unit_of(&self, construct: impl Into<ast::Construct>) -> Option<&TextualUnit> {
        let construct = construct.into();
        self.module_of(construct.module())
            .textual_information
            .get(construct)
    }

    fn kind_of(&self, construct: impl Into<ast::Construct>) -> Option<Cow<ast::Kind>> {
        let construct = construct.into();
        self.module_of(construct.module())
            .inferred_kinds()
            .get(construct)
    }

    fn type_of(&self, construct: impl Into<ast::Construct>) -> Option<&ast::Type> {
        let construct = construct.into();
        self.module_of(construct.module())
            .inferred_types()
            .type_(construct)
    }

    fn scheme_of(&self, construct: impl Into<ast::Construct>) -> Option<&ast::Scheme> {
        let construct = construct.into();
        self.module_of(construct.module())
            .inferred_types()
            .scheme(construct)
    }

    fn instantiation_of(
        &self,
        construct: impl Into<ast::Construct>,
    ) -> Option<&ast::Instantiation> {
        let construct = construct.into();
        self.module_of(construct.module())
            .inferred_types()
            .instantiation(construct)
    }
}

impl<'a, T: ModuleMap> ModuleMap for &'a T {
    fn module_of(&self, mid: ModuleId) -> &Module {
        T::module_of(*self, mid)
    }
}

impl ModuleMap for HashMap<ModuleId, Module> {
    fn module_of(&self, mid: ModuleId) -> &Module {
        &self[&mid]
    }
}

impl ModuleMap for HashMap<ModuleId, Arc<Module>> {
    fn module_of(&self, mid: ModuleId) -> &Module {
        &self[&mid]
    }
}
