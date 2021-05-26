use super::TestContext;
use crate::formatting::ContextualDisplay as _;
use crate::module::{Formatter, ModuleMap};
use crate::prelude::*;
use crate::sexp::{self, matcher as m};
use itertools::Itertools;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;

#[derive(Debug)]
pub(super) enum ModuleValidation {
    KindOf(ModuleValidationTarget, String, String),
    TypeOf(ModuleValidationTarget, String, String),
}

impl ModuleValidation {
    pub(super) fn run(
        &self,
        module_map: &impl ModuleMap,
        entry_module: &Module,
        ctx: &TestContext,
    ) {
        match self {
            Self::KindOf(target, name, kind) => {
                let construct = match target.get(entry_module, name).as_slice() {
                    [construct] => *construct,
                    [] => panic!("{}: Cannot get {} of {}", ctx.header(), name, target),
                    _ => panic!(
                        "{}: There are two or more {}s named {}",
                        ctx.header(),
                        target,
                        name
                    ),
                };
                let inferred_kind = match module_map.kind_of(construct) {
                    Some(kind) => kind.fmt_on(&Formatter::new(&module_map)).to_string(),
                    None => panic!("{}: Cannot get kind of {}", ctx.header(), name),
                };

                assert_eq!(
                    inferred_kind,
                    *kind,
                    "{}: Expected kind `{}` but got `{}`",
                    ctx.header(),
                    kind,
                    inferred_kind
                );
            }
            Self::TypeOf(target, name, ty) => {
                let construct = match target.get(entry_module, name).as_slice() {
                    [construct] => *construct,
                    [] => panic!("{}: Cannot get {} of {}", ctx.header(), name, target),
                    _ => panic!(
                        "{}: There are two or more {}s named {}",
                        ctx.header(),
                        target,
                        name
                    ),
                };
                let inferred_type = match module_map.type_of(construct) {
                    Some(ty) => format!("{{{}}}", ty.fmt_on(&Formatter::new(&module_map))),
                    None => match module_map.scheme_of(construct) {
                        Some(scheme) => scheme.fmt_on(&Formatter::new(&module_map)).to_string(),
                        None => panic!("{}: Cannot get type of {}", ctx.header(), name),
                    },
                };

                assert_eq!(
                    inferred_type,
                    *ty,
                    "{}: Expected type `{}` but got `{}`",
                    ctx.header(),
                    ty,
                    inferred_type
                );
            }
        }
    }
}

impl<'a> m::Match<'a> for ModuleValidation {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok(("kind-of", target, name, k)) =
            s.matches::<(m::Symbol, ModuleValidationTarget, m::Symbol, m::Any)>()
        {
            Ok(Self::KindOf(target, name.to_string(), k.to_string()))
        } else if let Ok(("type-of", target, (sexp::ANNOTATE, name, scheme))) = s.matches::<(
            m::Symbol,
            ModuleValidationTarget,
            (m::Symbol, m::Symbol, m::Rest<m::Any>),
        )>() {
            Ok(Self::TypeOf(
                target,
                name.to_string(),
                format!("{{{}}}", scheme.iter().format(" ")),
            ))
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        "<module-validation>".into()
    }
}

impl_default_match_slice!(['a] MatchSlice<'a> for ModuleValidation);

#[derive(Debug)]
pub(super) enum ModuleValidationTarget {
    Data,
    BuiltinType,
    DataValueCon,
    BuiltinValueCon,
    Function,
    CFunction,
    BuiltinOp,
    Macro,
    Class,
    ClassMethod,
    Instance,
    InstanceMethod,
    LocalVar,
    LocalFun,
}

impl ModuleValidationTarget {
    pub(super) fn get(&self, module: &Module, name: &str) -> Vec<ast::Construct> {
        fn find<T>(module: &Module, name: &str) -> Vec<ast::Construct>
        where
            ast::NodeId<T>: TryFrom<ast::Construct> + Into<ast::Construct>,
        {
            module
                .textual_information()
                .find::<ast::NodeId<T>>(name)
                .into_iter()
                .map(|id| id.into())
                .collect()
        }

        match *self {
            Self::Data => find::<ast::DataTypeCon>(module, name),
            Self::BuiltinType => find::<ast::BuiltinTypeCon>(module, name),
            Self::DataValueCon => find::<ast::DataValueCon>(module, name),
            Self::BuiltinValueCon => find::<ast::BuiltinValueCon>(module, name),
            Self::Function => find::<ast::Function>(module, name),
            Self::CFunction => find::<ast::CFunction>(module, name),
            Self::BuiltinOp => find::<ast::BuiltinOp>(module, name),
            Self::Macro => find::<ast::Macro>(module, name),
            Self::Class => find::<ast::ClassCon>(module, name),
            Self::ClassMethod => find::<ast::ClassMethod>(module, name),
            Self::Instance => find::<ast::InstanceCon>(module, name),
            Self::InstanceMethod => find::<ast::InstanceMethod>(module, name),
            Self::LocalVar => find::<ast::LocalVar>(module, name),
            Self::LocalFun => find::<ast::LocalFun>(module, name),
        }
    }
}

impl fmt::Display for ModuleValidationTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Data => write!(f, "data"),
            Self::BuiltinType => write!(f, "builtin-type"),
            Self::DataValueCon => write!(f, "data-value-con"),
            Self::BuiltinValueCon => write!(f, "builtin-value-con"),
            Self::Function => write!(f, "function"),
            Self::CFunction => write!(f, "c-function"),
            Self::BuiltinOp => write!(f, "builtin-op"),
            Self::Macro => write!(f, "macro"),
            Self::Class => write!(f, "class"),
            Self::ClassMethod => write!(f, "class-method"),
            Self::Instance => write!(f, "instance"),
            Self::InstanceMethod => write!(f, "instance-method"),
            Self::LocalVar => write!(f, "local-var"),
            Self::LocalFun => write!(f, "local-fun"),
        }
    }
}

impl<'a> m::Match<'a> for ModuleValidationTarget {
    type Result = Self;

    fn match_sexp(s: &'a Sexp) -> m::Result<Self> {
        if let Ok(name) = s.matches::<m::Symbol>() {
            match name {
                "data" => Ok(Self::Data),
                "builtin-type" => Ok(Self::BuiltinType),
                "data-value-con" => Ok(Self::DataValueCon),
                "builtin-value-con" => Ok(Self::BuiltinValueCon),
                "function" => Ok(Self::Function),
                "c-function" => Ok(Self::CFunction),
                "builtin-op" => Ok(Self::BuiltinOp),
                "macro" => Ok(Self::Macro),
                "class" => Ok(Self::Class),
                "class-method" => Ok(Self::ClassMethod),
                "instance" => Ok(Self::Instance),
                "instance-method" => Ok(Self::InstanceMethod),
                "local-var" => Ok(Self::LocalVar),
                "local-fun" => Ok(Self::LocalFun),
                _ => Err(Self::error(s)),
            }
        } else {
            Err(Self::error(s))
        }
    }

    fn expect() -> Cow<'static, str> {
        "<target>".into()
    }
}
