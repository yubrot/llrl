use super::{AvailableInstances, External, ModuleId, SymbolMap};
use crate::ast;
use derive_new::new;
use std::borrow::Cow;

#[derive(Debug, new)]
pub struct TypeEnv<'a, E> {
    available_instances: &'a AvailableInstances,
    local_ast_id_generator: &'a mut ast::NodeIdGenerator,
    local_ast_root: &'a ast::Root,
    symbol_map: &'a mut SymbolMap,
    external: &'a E,
}

impl<'a, E: External> TypeEnv<'a, E> {
    fn ast_of(&self, mid: ModuleId) -> &'a ast::Root {
        if mid == self.local_ast_root.id.module() {
            &self.local_ast_root
        } else {
            &self.external.module(mid).ast_root
        }
    }

    pub fn available_instances(&self) -> &'a AvailableInstances {
        self.available_instances
    }

    pub fn function_scheme(&self, id: ast::NodeId<ast::Function>) -> Option<&'a ast::Scheme> {
        if id.module() == self.local_ast_root.id.module() {
            self.local_ast_root.functions[&id]
                .ann
                .as_ref()
                .map(|ann| &ann.body)
        } else {
            Some(
                self.external
                    .module(id.module())
                    .inferred_types
                    .scheme(id)
                    .unwrap_or_else(|| panic!("Uninferred function: {}", id)),
            )
        }
    }

    pub fn c_function_type(&self, id: ast::NodeId<ast::CFunction>) -> &'a ast::Type {
        if id.module() == self.local_ast_root.id.module() {
            &self.local_ast_root.c_functions[&id].ann.body
        } else {
            self.external
                .module(id.module())
                .inferred_types
                .type_(id)
                .unwrap_or_else(|| panic!("Uninferred c-function: {}", id))
        }
    }

    pub fn builtin_op_scheme(&self, id: ast::NodeId<ast::BuiltinOp>) -> &'a ast::Scheme {
        if id.module() == self.local_ast_root.id.module() {
            &self.local_ast_root.builtin_ops[&id].ann.body
        } else {
            self.external
                .module(id.module())
                .inferred_types
                .scheme(id)
                .unwrap_or_else(|| panic!("Uninferred builtin-op: {}", id))
        }
    }

    pub fn get_or_create_data_value_con_scheme(
        &mut self,
        id: ast::NodeId<ast::DataValueCon>,
    ) -> Cow<'a, ast::Scheme> {
        if id.module() == self.local_ast_root.id.module() {
            let con = &self.local_ast_root.data_value_cons[&id];
            let type_con = &self.local_ast_root.data_type_cons[&con.type_con];
            Cow::Owned(con.to_scheme(type_con))
        } else {
            Cow::Borrowed(
                self.external
                    .module(id.module())
                    .inferred_types
                    .scheme(id)
                    .unwrap_or_else(|| panic!("Uninferred data value constructor: {}", id)),
            )
        }
    }

    pub fn get_or_create_builtin_value_con_scheme(
        &mut self,
        id: ast::NodeId<ast::BuiltinValueCon>,
    ) -> Cow<'a, ast::Scheme> {
        if id.module() == self.local_ast_root.id.module() {
            let con = &self.local_ast_root.builtin_value_cons[&id];
            let type_con = &self.local_ast_root.builtin_type_cons[&con.type_con];
            Cow::Owned(con.to_scheme(type_con))
        } else {
            Cow::Borrowed(
                self.external
                    .module(id.module())
                    .inferred_types
                    .scheme(id)
                    .unwrap_or_else(|| panic!("Uninferred builtin value constructor: {}", id)),
            )
        }
    }

    pub fn class_con(&self, id: ast::NodeId<ast::ClassCon>) -> &'a ast::ClassCon {
        &self.ast_of(id.module()).class_cons[&id]
    }

    pub fn class_method(&mut self, id: ast::NodeId<ast::ClassMethod>) -> &'a ast::ClassMethod {
        &self.ast_of(id.module()).class_methods[&id]
    }

    pub fn get_or_create_class_method_external_scheme(
        &mut self,
        id: ast::NodeId<ast::ClassMethod>,
    ) -> Cow<'a, ast::Scheme> {
        if id.module() == self.local_ast_root.id.module() {
            let method = &self.local_ast_root.class_methods[&id];
            let class_con = &self.local_ast_root.class_cons[&method.class_con];
            Cow::Owned(method.to_external_scheme(class_con))
        } else {
            Cow::Borrowed(
                self.external
                    .module(id.module())
                    .inferred_types
                    .scheme(id)
                    .unwrap_or_else(|| panic!("Uninferred class method: {}", id)),
            )
        }
    }

    pub fn instance_con(&self, id: ast::NodeId<ast::InstanceCon>) -> &'a ast::InstanceCon {
        &self.ast_of(id.module()).instance_cons[&id]
    }

    pub fn value_con_arity(&self, value_con: ast::ValueCon) -> Option<u32> {
        let fields = match value_con {
            ast::ValueCon::Data(id) => &self.ast_of(id.module()).data_value_cons[&id].fields,
            ast::ValueCon::Builtin(id) => &self.ast_of(id.module()).builtin_value_cons[&id].fields,
        };
        fields.as_ref().map(|fields| fields.len() as u32)
    }

    pub fn new_constraint_id(&mut self) -> ast::NodeId<ast::Constraint> {
        self.local_ast_id_generator.generate()
    }

    pub fn new_gen(
        &mut self,
        related_construct: ast::Construct,
        name: impl Into<String>,
    ) -> ast::NodeId<ast::TypeParameter> {
        let gen = self.local_ast_id_generator.generate();
        let loc = self.symbol_map.get(related_construct).unwrap().loc;
        self.symbol_map.set(gen, loc, name);
        gen
    }
}
