use super::{Error, External, Module, Result, Scope};
use crate::ast;
use crate::code::Code;
use crate::path::Path;
use crate::sexp::Ss;
use crate::syntax;
use std::borrow::Cow;

static WILDCARD: &'static str = "_";

#[derive(Debug, Clone, Copy)]
pub enum WildcardPortTarget<'a> {
    Everything,
    StartsWith(&'a str),
    EndsWith(&'a str),
}

impl<'a> WildcardPortTarget<'a> {
    pub fn from_pattern(input: &'a str) -> Option<Self> {
        if input == WILDCARD {
            Some(Self::Everything)
        } else if input.starts_with(WILDCARD) {
            Some(Self::EndsWith(&input[WILDCARD.len()..]))
        } else if input.ends_with(WILDCARD) {
            let l = input.len() - WILDCARD.len();
            Some(Self::StartsWith(&input[0..l]))
        } else {
            None
        }
    }

    pub fn deconstruct<'b>(&self, input: &'b str) -> Option<&'b str> {
        match self {
            Self::Everything => Some(input),
            Self::StartsWith(prefix) => {
                if input.starts_with(prefix) && input.len() != prefix.len() {
                    Some(&input[prefix.len()..])
                } else {
                    None
                }
            }
            Self::EndsWith(suffix) => {
                if input.ends_with(suffix) && input.len() != suffix.len() {
                    let l = input.len() - suffix.len();
                    Some(&input[0..l])
                } else {
                    None
                }
            }
        }
    }

    pub fn construct<'b>(&self, input: &'b str) -> Cow<'b, str> {
        match self {
            Self::Everything => Cow::Borrowed(input),
            Self::StartsWith(prefix) => Cow::Owned(format!("{}{}", prefix, input)),
            Self::EndsWith(suffix) => Cow::Owned(format!("{}{}", input, suffix)),
        }
    }
}

pub fn run(module: &mut Module, source: &Ss, code: &Code, external: &impl External) -> Result<()> {
    if code.path != Path::builtin() {
        module.add_init_expr(ast::InitExpr::EnsureInitialized(ast::ModuleId::builtin()));
    }

    for s in source.ss.iter() {
        if let Ok(import) = s.matches::<syntax::Import>() {
            let import_path = &code.dependencies[import.path];
            let import_module = external.find_module(import_path).unwrap();

            for target in import.targets {
                let select = target.matches::<syntax::PortTarget>()?;

                if let Some(src) = WildcardPortTarget::from_pattern(select.target.sym) {
                    let dest = match select.name {
                        Some(name) => match WildcardPortTarget::from_pattern(name.sym) {
                            Some(dest) => dest,
                            None => Err(Error::WildcardPortNameMustBeWildcard(select.loc))?,
                        },
                        None => src,
                    };

                    for (name, c) in import_module.exports.iter() {
                        if let Some(name) = src.deconstruct(name) {
                            module
                                .top_level
                                .define(&dest.construct(name), c.with_loc(select.loc))?;
                        }
                    }
                } else {
                    let name = select.name.unwrap_or(select.target);

                    if let Some(c) = import_module.exports.get(select.target.sym) {
                        module.top_level.define(name.sym, c.with_loc(select.loc))?;
                    } else {
                        Err(Error::unresolved(
                            select.loc,
                            "import-target",
                            select.target.sym,
                        ))?;
                    }
                }
            }

            module.add_init_expr(ast::InitExpr::EnsureInitialized(import_module.id()));
        }
    }

    if code.implicit_std {
        let std_module = external.find_module(&Path::std()).unwrap();
        module.add_init_expr(ast::InitExpr::EnsureInitialized(std_module.id()));

        for (name, c) in std_module.exports.iter() {
            if module.top_level.get(name).is_none() {
                module.top_level.define(name, c.with_loc(source.loc))?;
            }
        }
    }

    for (_, c) in module.top_level.iter() {
        if let ast::Construct::InstanceCon(id) = c.construct {
            debug_assert_ne!(id.module(), module.id());
            module
                .available_instances
                .add(&external.module(id.module()).ast_root.instance_cons[&id]);
        }
    }

    Ok(())
}
