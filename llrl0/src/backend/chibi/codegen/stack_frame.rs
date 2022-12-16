use super::Context;
use crate::backend::native::mem_layout::{Class, Layout};
use crate::lowering::ir::*;
use std::collections::HashMap;

// A llrl stack frame is configured to satisfy the following:
// * rbp is 16-byte aligned
// * [rbp - 8n] has the the local variables eightbytes
// * [rbp] has the previous rbp value
// * [rbp + 8] has the return address (pushed by callq)
// ^--- current stack frame ---^
// * [rbp + 16 + 8n] has the argument values eightbytes (pushed by callee)
// These are satisfied by `FunctionCodegen::prologue`.

#[derive(Debug)]
pub struct StackFrame {
    pub ret_ptr: Option<Offset>,
    pub var_offsets: HashMap<RtId, Offset>,
    pub vars: usize,   // number of eightbytes required to store local variables
    pub allocs: usize, // number of eightbytes required for Location::StackStatic
    pub depth: usize,  // number of eightbytes used after rbp
}

impl StackFrame {
    pub fn new(f: &Function, ctx: &Context) -> Self {
        let mut builder = StackFrameBuilder {
            frame: Self {
                ret_ptr: None,
                var_offsets: HashMap::new(),
                vars: 0,
                allocs: 0,
                depth: 0,
            },
            scope_vars: 0,
            ctx,
        };
        builder.collect(f);
        assert_eq!(builder.frame.depth, 0);
        builder.frame
    }

    pub fn reserved_area(&self) -> Layout {
        Layout::memory((self.vars + self.allocs) * 8, 8)
    }

    pub fn consume_alloc_area(&mut self, layout: &Layout) -> Offset {
        assert!(layout.num_eightbytes() <= self.allocs);
        self.vars += layout.num_eightbytes();
        self.allocs -= layout.num_eightbytes();
        (self.vars as i32) * -8
    }
}

pub type Offset = i32;

#[derive(Debug)]
struct StackFrameBuilder<'a> {
    frame: StackFrame,
    scope_vars: usize, // number of eightbytes required to store variables at the scope
    ctx: &'a Context,
}

impl<'a> StackFrameBuilder<'a> {
    fn collect(&mut self, f: &Function) {
        if let Some(env) = f.env.as_ref() {
            assert_eq!(f.kind, FunctionKind::Standard);
            // save the env pointer in StackFrame
            self.add_var(env.id, &Ct::Env);

            for elem in env.elems.iter() {
                self.add_var(elem.id, &elem.ty);
            }
        }

        if self.ctx.layout(&f.ret).class == Class::Memory {
            assert_ne!(f.kind, FunctionKind::Main);
            // save the ret pointer in StackFrame
            let offset = self.allocate(&Layout::pointer());
            assert!(self.frame.ret_ptr.replace(offset).is_none());
        }

        if f.kind == FunctionKind::Macro {
            // Save the macro input (Syntax Sexp)
            assert_eq!(f.params.len(), 1);
            let param = &f.params[0];
            self.add_var(param.id, &param.ty);
        } else {
            // Arguments are passed through the stack
            let params = f
                .params
                .iter()
                .map(|p| (p.id, self.ctx.layout(&p.ty).num_eightbytes() as i32))
                .collect::<Vec<_>>();

            // [rbp + 16 + 8n] has the argument values eightbyte (see FunctionCodegen::prologue)
            let mut offset = 16 + params.iter().map(|(_, n)| *n).sum::<i32>() * 8;
            for (id, eightbyte) in params {
                // Arguments are pushed onto the stack in left-to-right order
                offset -= eightbyte * 8;
                self.add_parameter(id, offset);
            }
        }

        traverser::traverse(&f.body, self).unwrap();
    }

    fn allocate(&mut self, layout: &Layout) -> Offset {
        self.scope_vars += layout.num_eightbytes();
        self.frame.vars = self.frame.vars.max(self.scope_vars);
        (self.scope_vars as i32) * -8
    }

    fn add_var(&mut self, id: RtId, ty: &Ct) {
        let layout = self.ctx.layout(ty);
        let offset = self.allocate(&layout);
        self.frame.var_offsets.insert(id, offset);
    }

    fn add_parameter(&mut self, id: RtId, offset: Offset) {
        self.frame.var_offsets.insert(id, offset);
    }
}

impl<'a> traverser::Traverser for StackFrameBuilder<'a> {
    type Error = ();

    fn before_rt(&mut self, rt: &Rt) -> Result<bool, ()> {
        match rt {
            Rt::LetVar(let_) => {
                let scope_vars = self.scope_vars;
                for v in let_.0.iter() {
                    self.add_var(v.id, &v.ty);
                    self.traverse(&v.init)?;
                }
                self.traverse(&let_.1)?;
                self.scope_vars = scope_vars; // variables are only available in this scope
                Ok(false)
            }
            Rt::LetCont(let_) => {
                let stashed_vars = std::mem::replace(&mut self.frame.vars, self.scope_vars);
                self.traverse(&let_.1)?;
                // After traversing let_.1, self.frame.vars indicates the size of the stack frame
                // required to evaluate let_.1
                self.scope_vars = self.frame.vars;
                // Restore the stashed_vars
                self.frame.vars = self.frame.vars.max(stashed_vars);

                for cont in let_.0.iter() {
                    let scope_vars = self.scope_vars;
                    for p in cont.params.iter() {
                        self.add_var(p.id, &p.ty);
                    }
                    self.traverse(&cont.body)?;
                    self.scope_vars = scope_vars; // cont parameters are only available in this scope
                }
                Ok(false)
            }
            Rt::Alloc(alloc) if alloc.0 == Location::StackStatic => {
                if let Some(ty) = alloc.1.ty() {
                    let layout = self.ctx.layout(ty.as_ref());
                    self.frame.allocs += layout.num_eightbytes();
                }
                Ok(true)
            }
            Rt::AllocArray(alloc) if alloc.0 == Location::StackStatic => panic!("Not implemented"),
            Rt::ConstructEnv(con) if con.0 == Location::StackStatic => panic!("Not implemented"),
            _ => Ok(true),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stack_frame() {
        use std::sync::Arc;

        let mut ct_id_gen = CtIdGen::new();
        let mut rt_id_gen = RtIdGen::new();

        // (value-data Pair (pair: I64 I64))
        let pair_id = ct_id_gen.next();
        let pair = Struct::new(StructRepr::Standard, vec![Ct::S(64), Ct::S(64)]);

        // (value-data Triple (pair: I64 I64 I64))
        let triple_id = ct_id_gen.next();
        let triple = Struct::new(StructRepr::Standard, vec![Ct::S(64), Ct::S(64), Ct::S(64)]);

        let mut ctx = Context::new();
        ctx.add_types(
            &[
                (pair_id, Arc::new(Def::Struct(pair))),
                (triple_id, Arc::new(Def::Struct(triple))),
            ]
            .into_iter()
            .collect(),
        );

        // some RtIds for tests
        let ids = (0..10).map(|_| rt_id_gen.next()).collect::<Vec<_>>();

        // standard function with arguments
        {
            let sf = StackFrame::new(
                &Function::new(
                    Some(FunctionEnv::new(
                        ids[0],
                        vec![
                            RtParam::new(ids[1], Ct::S(32)),
                            RtParam::new(ids[2], Ct::F64),
                        ],
                    )),
                    vec![
                        RtParam::new(ids[3], Ct::S(64)),
                        RtParam::new(ids[4], Ct::Id(pair_id)),
                    ],
                    Ct::Id(pair_id),
                    Rt::Never,
                    FunctionKind::Standard,
                    false,
                ),
                &ctx,
            );
            assert_eq!(sf.ret_ptr, None);
            assert_eq!(
                sf.var_offsets,
                [
                    (ids[0], -8),
                    (ids[1], -16),
                    (ids[2], -24),
                    (ids[3], 32),
                    (ids[4], 16)
                ]
                .into_iter()
                .collect()
            );
            assert_eq!(sf.vars, 3);
            assert_eq!(sf.allocs, 0);
            assert_eq!(sf.depth, 0);
            assert_eq!(sf.reserved_area(), Layout::memory(24, 8));
        }

        // return by pointer store
        {
            let sf = StackFrame::new(
                &Function::new(
                    None,
                    vec![],
                    Ct::Id(triple_id),
                    Rt::Never,
                    FunctionKind::Standard,
                    false,
                ),
                &ctx,
            );
            assert_eq!(sf.ret_ptr, Some(-8));
            assert_eq!(sf.var_offsets, HashMap::new());
            assert_eq!(sf.vars, 1);
        }

        // macro
        {
            let sf = StackFrame::new(
                &Function::new(
                    None,
                    vec![RtParam::new(ids[0], Ct::Id(pair_id))],
                    Ct::Id(triple_id),
                    Rt::Never,
                    FunctionKind::Macro,
                    false,
                ),
                &ctx,
            );
            assert_eq!(sf.ret_ptr, Some(-8));
            assert_eq!(sf.var_offsets, [(ids[0], -24)].into_iter().collect());
            assert_eq!(sf.vars, 3);
        }

        // LetVar
        {
            let body = Rt::let_var(
                vec![
                    RtVar::new(
                        ids[0],
                        Ct::S(64),
                        Rt::let_var(vec![RtVar::new(ids[1], Ct::S(64), Rt::Never)], Rt::Never),
                    ),
                    RtVar::new(
                        ids[2],
                        Ct::S(64),
                        Rt::let_var(vec![RtVar::new(ids[3], Ct::S(64), Rt::Never)], Rt::Never),
                    ),
                ],
                Rt::let_var(vec![RtVar::new(ids[4], Ct::S(64), Rt::Never)], Rt::Never),
            );
            let sf = StackFrame::new(
                &Function::new(None, vec![], Ct::Unit, body, FunctionKind::Standard, false),
                &ctx,
            );
            assert_eq!(
                sf.var_offsets,
                [
                    (ids[0], -8),
                    (ids[1], -16),
                    (ids[2], -16),
                    (ids[3], -24),
                    (ids[4], -24)
                ]
                .into_iter()
                .collect()
            );
            assert_eq!(sf.vars, 3);
            assert_eq!(sf.allocs, 0);
        }

        // LetCont
        {
            let body = Rt::seq(
                vec![Rt::let_var(
                    vec![RtVar::new(ids[0], Ct::S(64), Rt::Never)],
                    Rt::Never,
                )],
                Rt::let_cont(
                    vec![
                        RtCont::new(ids[1], vec![RtParam::new(ids[2], Ct::S(64))], Rt::Never),
                        RtCont::new(
                            ids[3],
                            vec![
                                RtParam::new(ids[4], Ct::S(64)),
                                RtParam::new(ids[5], Ct::S(64)),
                            ],
                            Rt::let_var(vec![RtVar::new(ids[6], Ct::S(64), Rt::Never)], Rt::Never),
                        ),
                    ],
                    Rt::let_var(
                        vec![
                            RtVar::new(ids[7], Ct::S(64), Rt::Never),
                            RtVar::new(ids[8], Ct::S(64), Rt::Never),
                        ],
                        Rt::Never,
                    ),
                ),
            );
            let sf = StackFrame::new(
                &Function::new(None, vec![], Ct::Unit, body, FunctionKind::Standard, false),
                &ctx,
            );
            assert_eq!(
                sf.var_offsets,
                [
                    (ids[0], -8),
                    (ids[2], -24),
                    (ids[4], -24),
                    (ids[5], -32),
                    (ids[6], -40),
                    (ids[7], -8),
                    (ids[8], -16)
                ]
                .into_iter()
                .collect()
            );
            assert_eq!(sf.vars, 5);
            assert_eq!(sf.allocs, 0);
        }

        // Alloc
        {
            let body = Rt::seq(
                vec![Rt::alloc(
                    Location::StackStatic,
                    Rt::nullary(Nullary::Uninitialized(Ct::Id(triple_id))),
                )],
                Rt::Never,
            );
            let mut sf = StackFrame::new(
                &Function::new(None, vec![], Ct::Unit, body, FunctionKind::Standard, false),
                &ctx,
            );
            assert_eq!(sf.var_offsets, HashMap::new());
            assert_eq!(sf.vars, 0);
            assert_eq!(sf.allocs, 3);

            assert_eq!(sf.consume_alloc_area(&ctx.layout(&Ct::Id(triple_id))), -24);
            assert_eq!(sf.vars, 3);
            assert_eq!(sf.allocs, 0);
        }
    }
}
