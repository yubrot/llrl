use super::{importer::WildcardPortTarget, Error, Module, Result, Scope};
use crate::sexp::Ss;
use crate::syntax;

pub fn run(module: &mut Module, code: &Ss) -> Result<()> {
    for s in code.ss.iter() {
        if let Ok(export) = s.matches::<syntax::Export>() {
            for target in export.targets {
                let select = target
                    .matches::<syntax::PortTarget>()
                    .map_err(|e| Box::new(e.into()))?;

                if let Some(src) = WildcardPortTarget::from_pattern(select.target.sym) {
                    let dest = match select.name {
                        Some(name) => match WildcardPortTarget::from_pattern(name.sym) {
                            Some(dest) => dest,
                            None => Err(Error::WildcardPortNameMustBeWildcard(select.loc))?,
                        },
                        None => src,
                    };

                    for (name, c) in module.top_level.iter() {
                        if let Some(name) = src.deconstruct(name) {
                            if let Some(old_c) = module.exports.add(&dest.construct(name), c) {
                                Err(Error::conflicting_exports(name, old_c, c))?;
                            }
                        }
                    }
                } else {
                    let name = select.name.unwrap_or(select.target);

                    if let Some(c) = module.top_level.get(select.target.sym) {
                        if let Some(old_c) = module.exports.add(name.sym, c) {
                            Err(Error::conflicting_exports(name.sym, old_c, c))?;
                        }
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
