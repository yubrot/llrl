//! A very simple and conservative pass for promoting heap allocations to stack allocations.

use std::collections::HashSet;

use super::{ir::*, rewriter};

pub fn run(src: &mut impl rewriter::Rewrite) {
    let _ = rewriter::rewrite(src, &mut Heap2Stack::default());
}

#[derive(Debug, Default)]
struct Heap2Stack {
    load_only_vars: HashSet<RtId>,
}

impl rewriter::Rewriter for Heap2Stack {
    type Error = ();

    fn before_rt(&mut self, rt: &mut Rt) -> Result<bool, Self::Error> {
        match rt {
            Rt::Local(id) => {
                self.load_only_vars.remove(id);
            }
            Rt::Unary(unary) => {
                if let (Unary::Load, Rt::Local(_)) = **unary {
                    return Ok(false);
                }
            }
            Rt::Binary(binary) => {
                if let (Binary::Store, ref mut val, Rt::Local(_)) = **binary {
                    self.rewrite(val)?;
                    return Ok(false);
                }
            }
            Rt::LetVar(let_) => {
                for var in let_.0.iter() {
                    if matches!(var.init, Rt::Alloc(_)) {
                        self.load_only_vars.insert(var.id);
                    }
                }
            }
            _ => {}
        }
        Ok(true)
    }

    fn after_rt(&mut self, rt: &mut Rt) -> Result<(), Self::Error> {
        if let Rt::LetVar(let_) = rt {
            for var in let_.0.iter_mut() {
                if self.load_only_vars.contains(&var.id) {
                    if let Rt::Alloc(ref mut alloc) = var.init {
                        alloc.0 = Location::StackStatic;
                    }
                }
            }
        }
        Ok(())
    }
}
