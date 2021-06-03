use super::{External, InferredKinds, ModuleId};
use crate::ast;
use crate::unification::types::KindEnvironment;
use derive_new::new;
use std::borrow::Cow;

#[derive(Debug, new)]
pub struct KindEnv<'a, E> {
    mid: ModuleId,
    local_inferred_kinds: &'a mut InferredKinds,
    external: &'a E,
}

impl<'a, E: External> KindEnv<'a, E> {
    pub fn get(&self, construct: impl Into<ast::Construct>) -> Option<Cow<ast::Kind>> {
        let construct = construct.into();
        if construct.module() == self.mid {
            self.local_inferred_kinds.get(construct)
        } else {
            let module = self.external.module(construct.module());
            module.inferred_kinds.get(construct)
        }
    }

    pub fn put(&mut self, construct: impl Into<ast::Construct>, kind: ast::Kind) {
        let construct = construct.into();
        debug_assert_eq!(construct.module(), self.mid);
        self.local_inferred_kinds.set(construct, kind);
    }
}

impl<'a, E: External> KindEnvironment for KindEnv<'a, E> {
    fn resolve_kind(&self, construct: impl Into<ast::Construct>) -> Cow<ast::Kind> {
        let construct = construct.into();
        self.get(construct)
            .unwrap_or_else(|| panic!("Kind of {} is undefined", construct))
    }
}
