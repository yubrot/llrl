use super::{ModuleMap, Symbol};
use crate::ast;
use crate::formatting::ContextualDisplay as _;
use derive_new::new;
use std::borrow::Cow;
use std::fmt;

#[derive(Debug, new)]
pub struct Formatter<M> {
    map: M,
}

impl<M: ModuleMap> Formatter<M> {
    pub fn symbol_of(&self, construct: impl Into<ast::Construct>) -> Option<&Symbol> {
        self.map.symbol_of(construct)
    }

    pub fn name_of(&self, construct: impl Into<ast::Construct>) -> Cow<str> {
        let construct = construct.into();
        self.symbol_of(construct).map_or_else(
            || construct.to_string().into(),
            |unit| unit.name.as_str().into(),
        )
    }
}

impl<M: ModuleMap> ast::KindFormatter for Formatter<M> {
    fn fmt_kind_use(&self, f: &mut fmt::Formatter<'_>, id: ast::Use<ast::KindUse>) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }
}

impl<M: ModuleMap> ast::TypeFormatter for Formatter<M> {
    fn fmt_type_use(&self, f: &mut fmt::Formatter<'_>, id: ast::Use<ast::TypeUse>) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }

    fn fmt_data_type_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::DataTypeCon>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }

    fn fmt_builtin_type_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::BuiltinTypeCon>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }

    fn fmt_type_parameter(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::TypeParameter>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }

    fn fmt_class_use(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::Use<ast::NodeId<ast::ClassCon>>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }
}

impl<M: ModuleMap> ast::ValueConFormatter for Formatter<M> {
    fn fmt_data_value_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::DataValueCon>,
    ) -> fmt::Result {
        if id == ast::builtin::TRUE {
            write!(f, "#t")
        } else if id == ast::builtin::FALSE {
            write!(f, "#f")
        } else {
            write!(f, "{}", self.name_of(id))
        }
    }

    fn fmt_builtin_value_con(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::BuiltinValueCon>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }
}

impl<M: ModuleMap> ast::PatternFormatter for Formatter<M> {
    fn fmt_pattern_var(
        &self,
        f: &mut fmt::Formatter<'_>,
        id: ast::NodeId<ast::PatternVar>,
    ) -> fmt::Result {
        write!(f, "{}", self.name_of(id))
    }

    fn fmt_value_con_use(
        &self,
        f: &mut fmt::Formatter<'_>,
        use_: ast::Use<ast::ValueCon>,
    ) -> fmt::Result {
        match use_ {
            ast::Use::Unresolved(id) => write!(f, "{}", self.name_of(id)),
            ast::Use::Resolved(con, _) => write!(f, "{}", con.fmt_on(self)),
        }
    }
}
