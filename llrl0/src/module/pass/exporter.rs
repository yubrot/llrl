use super::{importer::WildcardPortTarget, Error, Module, Result, Scope};
use crate::sexp::Ss;
use crate::syntax;

pub fn run(module: &mut Module, source: &Ss) -> Result<()> {
    for s in source.ss.iter() {
        if let Ok(export) = s.matches::<syntax::Export>() {
            for target in export.targets {
                let select = target.matches::<syntax::PortTarget>()?;

                if let Some(src) = WildcardPortTarget::from_pattern(select.target.sym) {
                    let dest = match select.name {
                        Some(name) => match WildcardPortTarget::from_pattern(name.sym) {
                            Some(dest) => dest,
                            None => Err(Error::WildcardPortNameMustBeWildcard(select.loc))?,
                        },
                        None => src,
                    };

                    for (name, binding) in module.top_level.iter() {
                        if let Some(name) = src.deconstruct(name) {
                            module.exports.add(&dest.construct(name), binding)?;
                        }
                    }
                } else {
                    let name = select.name.unwrap_or(select.target);

                    if let Some(binding) = module.top_level.get(select.target.sym) {
                        module.exports.add(name.sym, binding)?;
                    } else {
                        Err(Error::unresolved(
                            select.loc,
                            "export-target",
                            select.target.sym,
                        ))?;
                    }
                }
            }
        }
    }

    Ok(())
}
