use super::Context;
use crate::backend::native::calling::CallConv;
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
        let call_conv = CallConv::from(f.kind);

        if let Some(env) = f.env.as_ref() {
            assert_eq!(call_conv, CallConv::Default);
            // save the env pointer in StackFrame
            self.add_var(env.id, &Ct::Env);

            for elem in env.elems.iter() {
                self.add_var(elem.id, &elem.ty);
            }
        }

        if self.ctx.layout(&f.ret).class == Class::Memory {
            assert_ne!(call_conv, CallConv::Main);
            // save the ret pointer in StackFrame
            let offset = self.allocate(&Layout::pointer());
            assert!(self.frame.ret_ptr.replace(offset).is_none());
        }

        if call_conv == CallConv::Macro {
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
