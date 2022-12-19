//! # chibi backend ABI
//!
//! The implementation of llrl's chibi backend code generation uses a similar method to
//! [rui314/chibicc](https://github.com/rui314/chibicc) (which is why this backend is named chibi).
//! The chibi backend has no implementation such as register allocation, and generates machine code
//! such that it targets the stack machine. This is inefficient compared to machine code that
//! makes better use of registers, but it is easier to implement.
//! The value obtained by the evaluation is first held in the rax and rdx registers or the xmm0 and
//! xmm1 registers only if the value size is small. Then, the last value is put on the stack only
//! when it is necessary to further evaluate the expression to obtain another value.
//!
//! ## Default calling conventions
//!
//! ### Registers
//! Callee-saved registers are the same as in the System V AMD64 ABI (rbx, rbp, rsp, r12..r15).
//!
//! ### Arguments
//! An environment is passed by the rsi register.
//! Every argument is passed in memory, like the value of the MEMORY class in the System V AMD64 ABI.
//! Arguments are pushed onto the stack in left-to-right order. 16-byte alignment is required.
//!
//! ### Return values
//! The return value is classified in the same way as in the System V AMD64 ABI.
//! The return value is passed in different ways depending on the classification:
//! * INTEGER class: Passed through the registers rax and rdx.
//! * SSE class: Passed through the registers xmm0 and xmm1.
//! * Otherwise: Passed through memory. The memory address to be written to is given by the rdi register.
//!   On return, the rax register will contain the address that has been passed by the rdi register.
//!
//! ## Other calling conventions
//! * Macro: No environemnt is passed, and the input S-expression is passed through the rsi register.
//! * Main: No environemnt is passed.

use super::{Context, FunctionSymbol};
use crate::backend::native::data::*;
use crate::backend::native::mem_layout::{Class, Layout};
use crate::lowering::ir::{self, *};
use derive_new::new;
use std::collections::HashMap;
use std::io::{self, Write};
use std::sync::Arc;
use xten::asm::*;

mod call_frame;
mod reg_assign;
mod stack_frame;
mod stack_inst;

#[macro_use]
mod macros;

use call_frame::{CallArg, CallFrame, CallRet};
use reg_assign::{AssignedReg, RegAssign};
use stack_frame::StackFrame;
use stack_inst::StackAwareInstWriter;

pub fn c_main_adapter_object(ctx: &mut Context) -> io::Result<Object> {
    let mut w = Writer::new();

    let c_main = w.get_label("main");
    let llrt_init = w.get_label("llrt_init");
    let llrl_main = w.get_label(&ctx.main_function_symbol().expect("main not found").name);

    w.define(c_main, true);
    w.subq(Rsp, 8i8)?; // 16-byte alignment
    w.callq(llrt_init)?;
    w.callq(llrl_main)?;
    w.xorl(Eax, Eax)?; // returns 0
    w.addq(Rsp, 8i8)?;
    w.retq()?;

    w.produce()
}

pub fn object(
    defs: &HashMap<CtId, Arc<Def>>,
    main: Option<&Function>,
    ctx: &mut Context,
) -> io::Result<Object> {
    let mut w = Writer::new();

    for (id, def) in defs {
        if let Def::Function(ref f) = **def {
            let symbol = FunctionSymbol::new(id.index().to_string(), f.kind, f.ty());
            ctx.define_function_symbol(*id, symbol);
        }
    }

    for (id, def) in defs {
        if let Def::Function(ref f) = **def {
            let label = w.get_label(&ctx.function_symbol(*id).unwrap().name);
            w.define(label, true);
            FunctionCodegen::run(&mut w, ctx, f)?;
        }
    }

    if let Some(main) = main {
        let symbol = FunctionSymbol::new(
            "llrl_main".to_string(),
            FunctionKind::Main,
            Function::main_ty(),
        );
        let label = w.get_label(&symbol.name);
        ctx.define_main_function_symbol(symbol);

        w.define(label, true);
        FunctionCodegen::run(&mut w, ctx, main)?;
    }

    w.produce()
}

#[derive(Debug)]
struct FunctionCodegen<'a> {
    w: &'a mut Writer,
    ctx: &'a mut Context,
    stack_frame: StackFrame,
    epilogue_label: Label,
    local_conts: HashMap<RtId, LocalCont>,
}

impl<'a> FunctionCodegen<'a> {
    fn run(w: &mut Writer, ctx: &mut Context, f: &Function) -> io::Result<()> {
        let stack_frame = StackFrame::new(f, ctx);
        let epilogue_label = w.issue_label();
        let mut cg = FunctionCodegen {
            w,
            ctx,
            stack_frame,
            epilogue_label,
            local_conts: HashMap::new(),
        };
        cg.prologue(f)?;
        cg.eval(&f.body)?;
        cg.epilogue(f)?;
        cg.w.retq()
    }

    fn prologue(&mut self, f: &Function) -> io::Result<()> {
        self.w.pushq(Rbp)?;
        self.w.movq(Rbp, Rsp)?;

        self.extend_stack(&self.stack_frame.reserved_area())?;

        // Initialize the local variables to correspond to the StackFrame

        if let Some(offset) = self.stack_frame.ret_ptr {
            self.w.movq(memory(Rbp + offset), Rdi)?;
        }

        if let Some(env) = f.env.as_ref() {
            let env_offset = *self.stack_frame.var_offsets.get(&env.id).unwrap();
            self.w.movq(memory(Rbp + env_offset), Rsi)?;

            let mut env_offset = 0;
            for RtParam { id, ty } in env.elems.iter() {
                let layout = self.ctx.layout(ty).in_stack();
                let offset = *self.stack_frame.var_offsets.get(id).unwrap();
                self.load(Rsi + env_offset, &layout)?;
                self.store(Rbp + offset, &layout)?;
                env_offset += layout.size as i32;
            }
        }

        if f.kind == FunctionKind::Macro {
            assert!(f.env.is_none());
            let offset = *self.stack_frame.var_offsets.get(&f.params[0].id).unwrap();
            self.w.movq(memory(Rbp + offset), Rsi)?;
        } else {
            // Arguments are passed through the stack
        }

        Ok(())
    }

    fn epilogue(&mut self, f: &Function) -> io::Result<()> {
        self.w.define(self.epilogue_label, false);

        if let Some(offset) = self.stack_frame.ret_ptr {
            let layout = self.ctx.layout(&f.ret);
            self.w.movq(Rax, memory(Rbp + offset))?;
            self.store(Rax, &layout)?;
        }

        self.w.movq(Rsp, Rbp)?;
        self.w.popq(Rbp)?;

        Ok(())
    }

    fn eval(&mut self, expr: &Rt) -> io::Result<Option<Layout>> {
        Ok(match expr {
            Rt::Var(id, ty) => {
                let layout = self.ctx.layout(ty);
                let offset = *self.stack_frame.var_offsets.get(id).unwrap();
                self.load(Rbp + offset, &layout.clone().in_stack())?;
                Some(layout)
            }
            Rt::LocalFun(_) => {
                panic!("Found Rt::LocalFun: this must be erased by lowerizer")
            }
            Rt::StaticFun(capture) => match &capture.fun {
                Ct::Id(id) => {
                    // {rax, rdx} <- {function-pointer, env-pointer}
                    if let Some(ref env) = capture.env {
                        continues!(self.eval(env)?);
                        self.w.movq(Rdx, Rax)?;
                    } else {
                        self.w.xorl(Edx, Edx)?;
                    }
                    let function_label = self
                        .w
                        .get_label(&self.ctx.function_symbol(*id).unwrap().name);
                    self.w.movq(Rax, AddressTable(function_label))?;
                    Some(Layout::clos())
                }
                ct => panic!("Unresolved Ct: {}, this must be resolved by lowerizer", ct),
            },
            Rt::Const(c) => Some(self.eval_const(c)?),
            Rt::Call(call) => match call.callee {
                RtCallee::Standard(ref callee) => self.eval_call(callee, &call.args)?,
                RtCallee::CDirect(ref sym, ref ret) => self.eval_c_call(
                    |self_| Ok(Some(AddressTable(self_.w.get_label(sym)))),
                    ret,
                    &call.args,
                )?,
                RtCallee::CIndirect(ref addr, ref ret) => self.eval_c_call(
                    |self_| {
                        continues!(self_.eval(addr)?);
                        Ok(Some(Rax))
                    },
                    ret,
                    &call.args,
                )?,
                // At this time, both mains and macros are called with the C calling conventions.
                RtCallee::MainIndirect(ref addr) => self.eval_c_call(
                    |self_| {
                        assert!(call.args.is_empty());
                        continues!(self_.eval(addr)?);
                        Ok(Some(Rax))
                    },
                    &Ct::BOOL,
                    &call.args,
                )?,
                RtCallee::MacroIndirect(ref addr, ref ret) => self.eval_c_call(
                    |self_| {
                        assert_eq!(call.args.len(), 1);
                        continues!(self_.eval(addr)?);
                        Ok(Some(Rax))
                    },
                    ret,
                    &call.args,
                )?,
            },
            Rt::ContCall(call) => self.eval_cont_call(call.cont, &call.args)?,
            Rt::Nullary(nullary) => self.eval_nullary(nullary)?,
            Rt::Unary(unary) => self.eval_unary(&unary.0, &unary.1)?,
            Rt::Binary(binary) => self.eval_binary(&binary.0, &binary.1, &binary.2)?,
            Rt::Ternary(t) => self.eval_ternary(&t.0, &t.1, &t.2, &t.3)?,
            Rt::Alloc(alloc) => {
                let layout = match alloc.1.ty() {
                    Some(ty) => self.ctx.layout(&ty),
                    None => diverges!(self.eval(&alloc.1)?),
                };

                if alloc.0 == ir::Location::StackStatic {
                    let offset = self.stack_frame.consume_alloc_area(&layout);

                    // initial value
                    let layout = continues!(self.eval(&alloc.1)?);
                    self.store(Rbp + offset, &layout)?;

                    self.w.leaq(Rax, memory(Rbp + offset))?;
                } else {
                    // TODO: ir::Location::StackDynamic support
                    self.w.movq(Rdi, layout.size as i32)?;
                    self.eval_builtin_call("GC_malloc", &Layout::pointer())?;

                    self.push(&Layout::pointer())?;

                    // initial value
                    let init_layout = continues!(self.eval(&alloc.1)?);
                    self.w
                        .movq(Rdi, memory(Rsp + init_layout.size_in_stack(true) as i32))?;
                    self.store(Rdi, &init_layout)?;

                    self.pop(&Layout::pointer())?;
                }
                Some(Layout::pointer())
            }
            Rt::AllocArray(alloc) => {
                assert_ne!(alloc.0, ir::Location::StackStatic, "Not implemented");

                // TODO: ir::Location::StackDynamic support
                let elem_size = self.ctx.layout(&alloc.1).size;
                continues!(self.eval(&alloc.2)?);
                self.push_eightbyte(Rax)?; // save length
                self.w.imulq(Rdi, Rax, elem_size as i32)?;
                self.eval_builtin_call("GC_malloc", &Layout::pointer())?;
                self.pop_eightbyte(Rdx)?; // restore length
                Some(Layout::array())
            }
            Rt::ConstructEnv(con) => {
                assert_ne!(con.0, ir::Location::StackStatic, "Not implemented");

                // TODO: ir::Location::StackDynamic support
                let mut size = 0;
                for elem in con.1.iter() {
                    match elem.ty() {
                        Some(ty) => size += self.ctx.layout(&ty).size_in_stack(false),
                        None => diverges!(self.eval_seq(&con.1)?),
                    }
                }

                self.w.movq(Rdi, size as i32)?;
                self.eval_builtin_call("GC_malloc", &Layout::pointer())?;
                self.push(&Layout::pointer())?;

                let mut env_offset = 0;
                for elem in con.1.iter() {
                    let layout = continues!(self.eval(elem)?).in_stack();
                    self.w
                        .movq(Rdi, memory(Rsp + layout.size_in_stack(true) as i32))?;
                    self.store(Rdi + env_offset, &layout)?;
                    env_offset += layout.size as i32;
                }

                self.pop(&Layout::pointer())?;
                Some(Layout::pointer())
            }
            Rt::ConstructData(_) => {
                panic!("Found Rt::ConstructData: this must be erased by lowerizer")
            }
            Rt::ConstructStruct(con) => {
                let layout = self.ctx.layout(&con.0);

                // Allocate a temporary area in the stack
                self.extend_stack(&layout)?;

                // Put components into the temporary area
                for ((o, _), e) in layout.composite.as_ref().unwrap().elems.iter().zip(&con.1) {
                    let layout = continues!(self.eval(e)?);
                    let offset = *o + layout.size_in_stack(true);
                    self.w.leaq(Rdi, memory(Rsp + offset as i32))?;
                    self.store(Rdi, &layout)?;
                }

                // Obtain a value from the temporary area
                self.pop(&layout)?;
                Some(layout)
            }
            Rt::ConstructSyntax(con) => {
                let body_layout = match con.1.ty() {
                    Some(ty) => self.ctx.layout(&ty),
                    None => diverges!(self.eval(&con.1)?),
                };
                let buf_align = body_layout.align.max(4);
                let buf_size = 8 + ((body_layout.size + buf_align - 1) / buf_align) * buf_align;
                self.w.movq(Rdi, buf_size as i32)?;
                self.eval_builtin_call("GC_malloc", &Layout::pointer())?;

                let metadata = NativeSyntaxMetadata::from_host(con.0);
                let metadata = unsafe { std::mem::transmute::<_, i64>(metadata) };
                self.w.movq(Rdi, metadata)?;
                self.w.movq(memory(Rax), Rdi)?;

                self.push(&Layout::pointer())?;

                let body_layout = continues!(self.eval(&con.1)?);
                self.w
                    .movq(Rdi, memory(Rsp + body_layout.size_in_stack(true) as i32))?;
                self.store(Rdi + 8i32, &body_layout)?;

                self.pop(&Layout::pointer())?;
                Some(Layout::pointer())
            }
            Rt::Seq(seq) => {
                continues!(self.eval_seq(&seq.0)?);
                self.eval(&seq.1)?
            }
            Rt::If(if_) => self.eval_if(&if_.0, &if_.1, &if_.2)?,
            Rt::While(w) => self.eval_while(&w.0, &w.1)?,
            Rt::And(_) => {
                panic!("Found Rt::And: this must be erased by lowerizer")
            }
            Rt::Or(_) => {
                panic!("Found Rt::Or: this must be erased by lowerizer")
            }
            Rt::Match(_) => {
                panic!("Found Rt::Match: this must be erased by lowerizer")
            }
            Rt::Return(e) => {
                continues!(self.eval(e)?);
                self.w.jmpq(self.epilogue_label)?;
                None
            }
            Rt::Never => None,
            Rt::LetLocalFun(_) => {
                panic!("Found Rt::LetLocalFun: this must be erased by lowerizer")
            }
            Rt::LetVar(let_) => self.eval_let_var(&let_.0, &let_.1)?,
            Rt::LetCont(let_) => self.eval_let_cont(&let_.0, &let_.1)?,
        })
    }

    fn eval_call(&mut self, callee: &Rt, args: &[Rt]) -> io::Result<Option<Layout>> {
        let callee_ty = continues!(callee.ty());
        let Ct::Clos(callee_ty) = callee_ty.as_ref() else {
            panic!("Type error: Callee is not a closure")
        };

        let ret = self.ctx.layout(&callee_ty.1);
        let call_args = CallArg::default_args(callee_ty.0.iter().map(|a| self.ctx.layout(a)));
        let call_frame = CallFrame::new(call_args, CallRet::default(&ret), &self.stack_frame);

        self.extend_stack(&call_frame.padding_before_stack_args())?;

        for arg in args.iter() {
            let layout = continues!(self.eval(arg)?);
            self.push(&layout)?;
        }

        // {rax, rdx} <- {function-pointer, env-pointer}
        continues!(self.eval(callee)?);

        // The environment is passed by the rsi register
        self.w.movq(Rsi, Rdx)?;

        if let Some(offset) = call_frame.offset_to_ret_destination() {
            // The memory address to be written to is given by the rdi register
            self.w.leaq(Rdi, memory(Rsp + offset as i32))?;
        }

        self.w.callq(Rax)?;

        self.shrink_stack(&call_frame.remnants_after_call())?;

        Ok(Some(ret))
    }

    fn eval_c_call<F>(
        &mut self,
        c_fun: impl FnOnce(&mut Self) -> io::Result<Option<F>>,
        ret: &Ct,
        args: &[Rt],
    ) -> io::Result<Option<Layout>>
    where
        inst::Callq<F>: WriteInst<Writer>,
    {
        let mut arg_layouts = Vec::new();
        for arg in args {
            match arg.ty() {
                Some(ty) => arg_layouts.push(self.ctx.layout(&ty)),
                None => diverges!(self.eval_seq(args)?),
            }
        }

        let ret = self.ctx.layout(ret);
        let call_args = CallArg::c_args(arg_layouts, &ret);

        let (stack_args, reg_args) = args
            .iter()
            .zip(call_args.iter().copied())
            .partition::<Vec<_>, _>(|(_, c)| matches!(c, CallArg::StackRev(_)));
        let call_frame = CallFrame::new(call_args, CallRet::c(&ret), &self.stack_frame);

        self.extend_stack(&call_frame.padding_before_stack_args())?;

        // stack args -> register args
        for (arg, _) in stack_args.iter().rev().chain(reg_args.iter().rev()) {
            let layout = continues!(self.eval(arg)?);
            self.push(&layout)?;
        }

        let c_fun = continues!(c_fun(self)?);

        // Pop register args to specific registers
        for (_, c) in reg_args {
            match c {
                CallArg::Reg(a, None) => self.pop_eightbyte(a.reg)?,
                CallArg::Reg(a, Some(b)) => {
                    self.pop_eightbyte(a.reg)?;
                    self.pop_eightbyte(b.reg)?;
                }
                _ => panic!(),
            }
        }

        if let Some(offset) = call_frame.offset_to_ret_destination() {
            // The memory address to be written to is given by the rdi register
            self.w.leaq(Rdi, memory(Rsp + offset as i32))?;
        }

        self.w.callq(c_fun)?;

        self.shrink_stack(&call_frame.remnants_after_call())?;

        Ok(Some(ret))
    }

    fn eval_builtin_call(&mut self, sym: &str, ret: &Layout) -> io::Result<()> {
        // Here, we assume that the every argument is assigned to registers
        let call_frame = CallFrame::new(Vec::new(), CallRet::c(ret), &self.stack_frame);

        self.extend_stack(&call_frame.padding_before_stack_args())?;

        // Due to the above assumptions, there is no operation to put arguments on the stack here

        if let Some(offset) = call_frame.offset_to_ret_destination() {
            // The memory address to be written to is given by the rdi register
            self.w.leaq(Rdi, memory(Rsp + offset as i32))?;
        }

        let builtin = self.w.get_label(sym);
        self.w.callq(AddressTable(builtin))?;

        self.shrink_stack(&call_frame.remnants_after_call())
    }

    fn eval_cont_call(&mut self, id: RtId, args: &[Rt]) -> io::Result<Option<Layout>> {
        let local_cont = self
            .local_conts
            .get(&id)
            .expect("unknown local continuation")
            .clone();

        for (arg, param) in args.iter().zip(local_cont.params) {
            let layout = continues!(self.eval(arg)?).in_stack();
            let offset = *self.stack_frame.var_offsets.get(&param).unwrap();
            self.store(Rbp + offset, &layout)?;
        }

        assert_eq!(local_cont.depth, self.stack_frame.depth);
        self.w.jmpq(local_cont.label)?;
        Ok(None)
    }

    fn eval_seq<'e>(&mut self, es: impl IntoIterator<Item = &'e Rt>) -> io::Result<Option<()>> {
        for e in es {
            let layout = continues!(self.eval(e)?);
            self.discard(&layout)?;
        }
        Ok(Some(()))
    }

    fn eval_if(&mut self, cond: &Rt, then: &Rt, else_: &Rt) -> io::Result<Option<Layout>> {
        continues!(self.eval(cond)?);
        let else_label = self.w.issue_label();
        let cont_label = self.w.issue_label();
        self.w.cmpb(Al, 0i8)?;
        self.w.je(else_label)?;

        let init_depth = self.stack_frame.depth;
        let mut cont_l = None;

        if let Some(l) = self.eval(then)? {
            self.w.jmpq(cont_label)?;
            cont_l = Some((l, self.stack_frame.depth));
        }

        self.w.define(else_label, false);
        self.stack_frame.depth = init_depth;
        if let Some(l) = self.eval(else_)? {
            if cont_l.is_some() {
                assert_eq!(cont_l, Some((l, self.stack_frame.depth)));
            } else {
                cont_l = Some((l, self.stack_frame.depth));
            }
        }

        self.w.define(cont_label, false);
        Ok(cont_l.map(|(l, depth)| {
            self.stack_frame.depth = depth;
            l
        }))
    }

    fn eval_while(&mut self, cond: &Rt, body: &Rt) -> io::Result<Option<Layout>> {
        let init_label = self.w.issue_label();
        let cont_label = self.w.issue_label();

        self.w.define(init_label, false);
        continues!(self.eval(cond)?);
        self.w.cmpb(Al, 0i8)?;
        self.w.je(cont_label)?;

        if let Some(l) = self.eval(body)? {
            self.discard(&l)?;
            self.w.jmpq(init_label)?;
        }

        self.w.define(cont_label, false);
        Ok(Some(Layout::unit()))
    }

    fn eval_nullary(&mut self, op: &Nullary) -> io::Result<Option<Layout>> {
        use Nullary::*;
        Ok(match op {
            Uninitialized(ty) => {
                let layout = self.ctx.layout(ty);
                if layout.class == Class::Memory {
                    self.extend_stack(&layout)?;
                }
                Some(layout)
            }
            Null(_) => {
                self.w.xorl(Eax, Eax)?;
                Some(Layout::pointer())
            }
            GenId => {
                self.eval_builtin_call("llrt_string_genid", &Layout::string())?;
                Some(Layout::string())
            }
            SizeOf(ty) => {
                let size = self.ctx.layout(ty).size as u64;
                Some(self.eval_const(&Const::Integer(Ct::U(64), false, size))?)
            }
            AlignOf(ty) => {
                let align = self.ctx.layout(ty).align as u64;
                Some(self.eval_const(&Const::Integer(Ct::U(64), false, align))?)
            }
        })
    }

    fn eval_unary(&mut self, op: &Unary, a: &Rt) -> io::Result<Option<Layout>> {
        use Unary::*;

        let a_layout = continues!(self.eval(a)?);

        Ok(match op {
            Not => {
                self.w.xorb(Al, 1i8)?;
                Some(a_layout)
            }
            Load => {
                let layout = self.ctx.layout(&Ct::ptr_elem(a.ty().unwrap()));
                self.w.movq(Rdi, Rax)?;
                self.load(Rdi, &layout)?;
                Some(layout)
            }
            StructElem(_, i) => {
                let (offset, layout) = a_layout.composite.as_ref().unwrap().elems[*i].clone();
                if offset != 0 || a_layout.num_eightbytes() > layout.num_eightbytes() {
                    let diff = (a_layout.num_eightbytes() - layout.num_eightbytes()) * 8;
                    self.push(&a_layout)?;
                    self.slide_in_stack(offset, diff, &layout)?;
                    self.shrink_stack(&Layout::memory(diff, 8))?;
                    self.pop(&layout)?;
                }
                Some(layout)
            }
            Reinterpret(to) => {
                let from_layout = a_layout;
                let to_layout = self.ctx.layout(to);
                self.push(&from_layout)?;
                let from = from_layout.num_eightbytes();
                let to = to_layout.num_eightbytes();
                if from < to {
                    let diff = (to - from) * 8;
                    self.extend_stack(&Layout::memory(diff, 8))?;
                    self.slide_in_stack(diff, 0, &from_layout)?;
                } else {
                    let diff = (from - to) * 8;
                    self.slide_in_stack(0, diff, &to_layout)?;
                    self.shrink_stack(&Layout::memory(diff, 8))?;
                }
                self.pop(&to_layout)?;
                Some(to_layout)
            }
            SyntaxBody => {
                let layout = self.ctx.layout(&Ct::syntax_body(a.ty().unwrap()));
                self.w.movq(Rdi, Rax)?;
                self.load(Rdi + 8i32, &layout)?;
                Some(layout)
            }
            Panic => {
                self.w.movq(Rdi, Rax)?;
                self.w.movq(Rsi, Rdx)?;
                self.eval_builtin_call("llrt_panic", &Layout::unit())?;
                None
            }
            BitCast(_) | PtrToI | IToPtr(_) => Some(a_layout),
            IComplement | IPopCount => {
                match (op, a_layout.size) {
                    (IComplement, 1) => self.w.xorb(Al, -1i8)?,
                    (IComplement, 2) => self.w.xorw(Ax, -1i8)?,
                    (IComplement, 4) => self.w.xorl(Eax, -1i8)?,
                    (IComplement, 8) => self.w.xorq(Rax, -1i8)?,
                    (IPopCount, 1) => {
                        self.w.movzbw(Ax, Al)?;
                        self.w.popcntw(Ax, Ax)?;
                    }
                    (IPopCount, 2) => self.w.popcntw(Ax, Ax)?,
                    (IPopCount, 4) => self.w.popcntl(Eax, Eax)?,
                    (IPopCount, 8) => self.w.popcntq(Rax, Rax)?,
                    (_, s) => unsupported_op!(op, size: s),
                }
                Some(a_layout)
            }
            ITrunc(ty) | SExt(ty) | UExt(ty) | SToF(ty) | UToF(ty) | FToS(ty) | FToU(ty)
            | FTrunc(ty) | FExt(ty) => {
                let layout = self.ctx.layout(ty);
                match (op, a_layout.size, layout.size) {
                    (ITrunc(_), _, _) => {}
                    (SExt(_), 1, 2) => self.w.cbtw()?,
                    (SExt(_), 1, 4) => self.w.movsbl(Eax, Al)?,
                    (SExt(_), 1, 8) => self.w.movsbq(Rax, Al)?,
                    (SExt(_), 2, 4) => self.w.cwtl()?,
                    (SExt(_), 2, 8) => self.w.movswq(Rax, Ax)?,
                    (SExt(_), 4, 8) => self.w.cltq()?,
                    (UExt(_), 1, 2) => self.w.movzbw(Ax, Al)?,
                    (UExt(_), 1, 4) => self.w.movzbl(Eax, Al)?,
                    (UExt(_), 1, 8) => self.w.movzbq(Rax, Al)?,
                    (UExt(_), 2, 4) => self.w.movzwl(Eax, Ax)?,
                    (UExt(_), 2, 8) => self.w.movzwq(Rax, Ax)?,
                    (UExt(_), 4, 8) => self.w.movl(Eax, Eax)?,
                    (SToF(_), 1, 4) => {
                        self.w.movsbl(Eax, Al)?;
                        self.w.cvtsi2ssl(Xmm0, Eax)?;
                    }
                    (SToF(_), 1, 8) => {
                        self.w.movsbl(Eax, Al)?;
                        self.w.cvtsi2sdl(Xmm0, Eax)?;
                    }
                    (SToF(_), 2, 4) => {
                        self.w.cwtl()?;
                        self.w.cvtsi2ssl(Xmm0, Eax)?;
                    }
                    (SToF(_), 2, 8) => {
                        self.w.cwtl()?;
                        self.w.cvtsi2sdl(Xmm0, Eax)?;
                    }
                    (SToF(_), 4, 4) => self.w.cvtsi2ssl(Xmm0, Eax)?,
                    (SToF(_), 4, 8) => self.w.cvtsi2sdl(Xmm0, Eax)?,
                    (SToF(_), 8, 4) => self.w.cvtsi2ssq(Xmm0, Rax)?,
                    (SToF(_), 8, 8) => self.w.cvtsi2sdq(Xmm0, Rax)?,
                    (UToF(_), 1, 4) => {
                        self.w.movzbl(Eax, Al)?;
                        self.w.cvtsi2ssl(Xmm0, Eax)?;
                    }
                    (UToF(_), 1, 8) => {
                        self.w.movzbl(Eax, Al)?;
                        self.w.cvtsi2sdl(Xmm0, Eax)?;
                    }
                    (UToF(_), 2, 4) => {
                        self.w.movzwl(Eax, Ax)?;
                        self.w.cvtsi2ssl(Xmm0, Eax)?;
                    }
                    (UToF(_), 2, 8) => {
                        self.w.movzwl(Eax, Ax)?;
                        self.w.cvtsi2sdl(Xmm0, Eax)?;
                    }
                    (UToF(_), 4, 4) => {
                        self.w.movl(Eax, Eax)?;
                        self.w.cvtsi2ssq(Xmm0, Rax)?;
                    }
                    (UToF(_), 4, 8) => {
                        self.w.movl(Eax, Eax)?;
                        self.w.cvtsi2sdq(Xmm0, Rax)?;
                    }
                    (UToF(_), 8, s @ (4 | 8)) => {
                        let large_label = self.w.issue_label();
                        let cont_label = self.w.issue_label();
                        self.w.testq(Rax, Rax)?;
                        self.w.js(Short(large_label))?;
                        self.w.pxor(Xmm0, Xmm0)?;
                        match s {
                            4 => self.w.cvtsi2ssq(Xmm0, Rax)?,
                            8 => self.w.cvtsi2sdq(Xmm0, Rax)?,
                            _ => panic!(),
                        }
                        self.w.jmpq(Short(cont_label))?;
                        self.w.define(large_label, false);
                        self.w.movq(Rdx, Rax)?;
                        self.w.andl(Eax, 1i8)?;
                        self.w.pxor(Xmm0, Xmm0)?;
                        self.w.shrq(Rdx, 1i8)?;
                        self.w.orq(Rdx, Rax)?;
                        match s {
                            4 => {
                                self.w.cvtsi2ssq(Xmm0, Rdx)?;
                                self.w.addss(Xmm0, Xmm0)?;
                            }
                            8 => {
                                self.w.cvtsi2sdq(Xmm0, Rdx)?;
                                self.w.addsd(Xmm0, Xmm0)?;
                            }
                            _ => panic!(),
                        }
                        self.w.define(cont_label, false);
                    }
                    (FToS(_) | FToU(_), 4, 1 | 2) | (FToS(_), 4, 4) => {
                        self.w.cvttss2si(Eax, Xmm0)?;
                    }
                    (FToS(_) | FToU(_), 8, 1 | 2) | (FToS(_), 8, 4) => {
                        self.w.cvttsd2si(Eax, Xmm0)?;
                    }
                    (FToU(_), 4, 4) | (FToS(_) | FToU(_), 4, 8) => {
                        self.w.cvttss2si(Rax, Xmm0)?;
                    }
                    (FToU(_), 8, 4) | (FToS(_) | FToU(_), 8, 8) => {
                        // FIXME: More precise convertion for (FToS(_) | FToU(_), 8, 8)
                        self.w.cvttsd2si(Rax, Xmm0)?;
                    }
                    (FTrunc(_), 8, 4) => self.w.cvtsd2ss(Xmm0, Xmm0)?,
                    (FExt(_), 4, 8) => self.w.cvtss2sd(Xmm0, Xmm0)?,
                    (_, from, to) => unsupported_op!(op, from: from, to: to),
                }
                Some(layout)
            }
            RealCeil | RealFloor | RealTrunc | RealRound | MathSqrt | MathSin | MathCos
            | MathExp | MathLog => {
                match (op, a_layout.size) {
                    (RealCeil, 4) => self.eval_builtin_call("ceilf", &a_layout)?,
                    (RealCeil, 8) => self.eval_builtin_call("ceil", &a_layout)?,
                    (RealFloor, 4) => self.eval_builtin_call("floorf", &a_layout)?,
                    (RealFloor, 8) => self.eval_builtin_call("floor", &a_layout)?,
                    (RealTrunc, 4) => self.eval_builtin_call("truncf", &a_layout)?,
                    (RealTrunc, 8) => self.eval_builtin_call("trunc", &a_layout)?,
                    (RealRound, 4) => self.eval_builtin_call("roundf", &a_layout)?,
                    (RealRound, 8) => self.eval_builtin_call("round", &a_layout)?,
                    (MathSqrt, 4) => self.eval_builtin_call("sqrtf", &a_layout)?,
                    (MathSqrt, 8) => self.eval_builtin_call("sqrt", &a_layout)?,
                    (MathSin, 4) => self.eval_builtin_call("sinf", &a_layout)?,
                    (MathSin, 8) => self.eval_builtin_call("sin", &a_layout)?,
                    (MathCos, 4) => self.eval_builtin_call("cosf", &a_layout)?,
                    (MathCos, 8) => self.eval_builtin_call("cos", &a_layout)?,
                    (MathExp, 4) => self.eval_builtin_call("expf", &a_layout)?,
                    (MathExp, 8) => self.eval_builtin_call("exp", &a_layout)?,
                    (MathLog, 4) => self.eval_builtin_call("logf", &a_layout)?,
                    (MathLog, 8) => self.eval_builtin_call("log", &a_layout)?,
                    (_, s) => unsupported_op!(op, size: s),
                };
                Some(a_layout)
            }
            StringPtr | ArrayPtr => Some(Layout::pointer()),
            StringLength | ArrayLength => {
                self.w.movq(Rax, Rdx)?;
                Some(Layout::integer(8))
            }
        })
    }

    fn eval_binary(&mut self, op: &Binary, a: &Rt, b: &Rt) -> io::Result<Option<Layout>> {
        use Binary::*;

        let a_layout = continues!(self.eval(a)?);
        self.push(&a_layout)?;
        let b_layout = continues!(self.eval(b)?);

        Ok(match op {
            Store => {
                self.w.movq(Rdi, Rax)?; // right operand: pointer
                self.pop(&a_layout)?; // left operand: value
                self.store(Rdi, &a_layout)?;
                Some(Layout::unit())
            }
            Offset => {
                let elem_size = self.ctx.layout(&Ct::ptr_elem(b.ty().unwrap())).size;
                self.pop_eightbyte(Rdi)?; // left operand: index
                self.w.imulq(Rdi, Rdi, elem_size as i32)?;
                self.w.addq(Rax, Rdi)?;
                Some(b_layout)
            }
            PtrEq | PtrLt | PtrLe | PtrGt | PtrGe | IEq | SLt | SLe | SGt | SGe | ULt | ULe
            | UGt | UGe => {
                self.pop_eightbyte(Rcx)?; // left operand
                match b_layout.size {
                    1 => self.w.cmpb(Cl, Al)?,
                    2 => self.w.cmpw(Cx, Ax)?,
                    4 => self.w.cmpl(Ecx, Eax)?,
                    8 => self.w.cmpq(Rcx, Rax)?,
                    s => unsupported_op!(op, size: s),
                }
                match op {
                    IEq | PtrEq => self.w.sete(Al)?,
                    SLt => self.w.setl(Al)?,
                    SLe => self.w.setle(Al)?,
                    SGt => self.w.setg(Al)?,
                    SGe => self.w.setge(Al)?,
                    ULt | PtrLt => self.w.setb(Al)?,
                    ULe | PtrLe => self.w.setbe(Al)?,
                    UGt | PtrGt => self.w.seta(Al)?,
                    UGe | PtrGe => self.w.setae(Al)?,
                    _ => panic!(),
                }
                Some(Layout::integer(1))
            }
            IShl | IAShr | ILShr | IAnd | IOr | IXor | SAdd | SSub | SMul | SDiv | SRem | UAdd
            | USub | UMul | UDiv | URem => {
                self.w.movq(Rcx, Rax)?; // right operand
                self.pop_eightbyte(Rax)?; // left operand
                match (op, b_layout.size) {
                    (IShl, 1) => self.w.shlb(Al, _Cl)?,
                    (IShl, 2) => self.w.shlw(Ax, _Cl)?,
                    (IShl, 4) => self.w.shll(Eax, _Cl)?,
                    (IShl, 8) => self.w.shlq(Rax, _Cl)?,
                    (IAShr, 1) => self.w.sarb(Al, _Cl)?,
                    (IAShr, 2) => self.w.sarw(Ax, _Cl)?,
                    (IAShr, 4) => self.w.sarl(Eax, _Cl)?,
                    (IAShr, 8) => self.w.sarq(Rax, _Cl)?,
                    (ILShr, 1) => self.w.shrb(Al, _Cl)?,
                    (ILShr, 2) => self.w.shrw(Ax, _Cl)?,
                    (ILShr, 4) => self.w.shrl(Eax, _Cl)?,
                    (ILShr, 8) => self.w.shrq(Rax, _Cl)?,
                    (IAnd, 1) => self.w.andb(Al, Cl)?,
                    (IAnd, 2) => self.w.andw(Ax, Cx)?,
                    (IAnd, 4) => self.w.andl(Eax, Ecx)?,
                    (IAnd, 8) => self.w.andq(Rax, Rcx)?,
                    (IOr, 1) => self.w.orb(Al, Cl)?,
                    (IOr, 2) => self.w.orw(Ax, Cx)?,
                    (IOr, 4) => self.w.orl(Eax, Ecx)?,
                    (IOr, 8) => self.w.orq(Rax, Rcx)?,
                    (IXor, 1) => self.w.xorb(Al, Cl)?,
                    (IXor, 2) => self.w.xorw(Ax, Cx)?,
                    (IXor, 4) => self.w.xorl(Eax, Ecx)?,
                    (IXor, 8) => self.w.xorq(Rax, Rcx)?,
                    (SAdd | UAdd, 1) => self.w.addb(Al, Cl)?,
                    (SAdd | UAdd, 2) => self.w.addw(Ax, Cx)?,
                    (SAdd | UAdd, 4) => self.w.addl(Eax, Ecx)?,
                    (SAdd | UAdd, 8) => self.w.addq(Rax, Rcx)?,
                    (SSub | USub, 1) => self.w.subb(Al, Cl)?,
                    (SSub | USub, 2) => self.w.subw(Ax, Cx)?,
                    (SSub | USub, 4) => self.w.subl(Eax, Ecx)?,
                    (SSub | USub, 8) => self.w.subq(Rax, Rcx)?,
                    (SMul | UMul, 1) => self.w.imulb(Cl)?,
                    (SMul | UMul, 2) => self.w.imulw1(Cx)?,
                    (SMul | UMul, 4) => self.w.imull1(Ecx)?,
                    (SMul | UMul, 8) => self.w.imulq1(Rcx)?,
                    (SDiv | SRem, 1) => {
                        self.w.cbtw()?;
                        self.w.idivb(Cl)?;
                    }
                    (SDiv | SRem, 2) => {
                        self.w.cwtd()?;
                        self.w.idivw(Cx)?;
                    }
                    (SDiv | SRem, 4) => {
                        self.w.cltd()?;
                        self.w.idivl(Ecx)?;
                    }
                    (SDiv | SRem, 8) => {
                        self.w.cqto()?;
                        self.w.idivq(Rcx)?;
                    }
                    (UDiv | URem, 1) => {
                        self.w.movzbw(Ax, Al)?;
                        self.w.divb(Cl)?;
                    }
                    (UDiv | URem, 2) => {
                        self.w.xorl(Edx, Edx)?;
                        self.w.divw(Cx)?;
                    }
                    (UDiv | URem, 4) => {
                        self.w.xorl(Edx, Edx)?;
                        self.w.divl(Ecx)?;
                    }
                    (UDiv | URem, 8) => {
                        self.w.xorl(Edx, Edx)?;
                        self.w.divq(Rcx)?;
                    }
                    (_, s) => unsupported_op!(op, size: s),
                }
                match (op, b_layout.size) {
                    (SRem | URem, 1) => self.w.movb(Al, Ah)?,
                    (SRem | URem, 2) => self.w.movw(Ax, Dx)?,
                    (SRem | URem, 4) => self.w.movl(Eax, Edx)?,
                    (SRem | URem, 8) => self.w.movq(Rax, Rdx)?,
                    _ => {}
                }
                Some(b_layout)
            }
            FEq | FLt | FLe | FGt | FGe => {
                self.pop_eightbyte(Xmm1)?; // left operand
                match b_layout.size {
                    4 => self.w.comiss(Xmm1, Xmm0)?,
                    8 => self.w.comisd(Xmm1, Xmm0)?,
                    s => unsupported_op!(op, size: s),
                }
                match op {
                    FEq => self.w.sete(Al)?,
                    FLt => self.w.setb(Al)?,
                    FLe => self.w.setbe(Al)?,
                    FGt => self.w.seta(Al)?,
                    FGe => self.w.setae(Al)?,
                    _ => panic!(),
                }
                Some(Layout::integer(1))
            }
            FAdd | FSub | FMul | FDiv | FRem | MathPow => {
                self.w.movq(Xmm1, Xmm0)?; // right operand
                self.pop_eightbyte(Xmm0)?; // left operand
                match (op, a_layout.size) {
                    (FAdd, 4) => self.w.addss(Xmm0, Xmm1)?,
                    (FSub, 4) => self.w.subss(Xmm0, Xmm1)?,
                    (FMul, 4) => self.w.mulss(Xmm0, Xmm1)?,
                    (FDiv, 4) => self.w.divss(Xmm0, Xmm1)?,
                    (FAdd, 8) => self.w.addsd(Xmm0, Xmm1)?,
                    (FSub, 8) => self.w.subsd(Xmm0, Xmm1)?,
                    (FMul, 8) => self.w.mulsd(Xmm0, Xmm1)?,
                    (FDiv, 8) => self.w.divsd(Xmm0, Xmm1)?,
                    (FRem, 4) => self.eval_builtin_call("fmodf", &a_layout)?,
                    (MathPow, 4) => self.eval_builtin_call("powf", &a_layout)?,
                    (FRem, 8) => self.eval_builtin_call("fmod", &a_layout)?,
                    (MathPow, 8) => self.eval_builtin_call("pow", &a_layout)?,
                    (_, s) => unsupported_op!(op, size: s),
                }
                Some(a_layout)
            }
            StringConstruct => {
                self.w.movq(Rdx, Rax)?; // right operand (len)
                self.pop_eightbyte(Rax)?; // left operand (ptr)
                Some(Layout::string())
            }
            StringEq | StringConcat | StringCmp => {
                // {rdx, rcx} <- right operand
                self.w.movq(Rcx, Rdx)?;
                self.w.movq(Rdx, Rax)?;
                // {rdi, rsi} <- left operand
                self.pop_eightbyte(Rdi)?;
                self.pop_eightbyte(Rsi)?;

                let (sym, ret) = match op {
                    StringEq => ("llrt_string_eq", Layout::integer(1)),
                    StringCmp => ("llrt_string_cmp", Layout::integer(4)),
                    StringConcat => ("llrt_string_concat", Layout::string()),
                    _ => panic!(),
                };

                self.eval_builtin_call(sym, &ret)?;
                Some(ret)
            }
            ArrayConstruct => {
                self.w.movq(Rdx, Rax)?; // right operand (len)
                self.pop_eightbyte(Rax)?; // left operand (ptr)
                Some(Layout::array())
            }
            ArrayLoad => {
                let elem_layout = self.ctx.layout(&Ct::array_elem(b.ty().unwrap()));
                self.w.movq(Rsi, Rax)?;
                self.pop_eightbyte(Rdi)?; // left operand (index)
                self.w.imulq(Rdi, Rdi, elem_layout.size as i32)?;
                self.load(Rsi + Rdi, &elem_layout)?;
                Some(elem_layout)
            }
        })
    }

    fn eval_ternary(&mut self, op: &Ternary, a: &Rt, b: &Rt, c: &Rt) -> io::Result<Option<Layout>> {
        use Ternary::*;

        let a_layout = continues!(self.eval(a)?);
        self.push(&a_layout)?;
        let b_layout = continues!(self.eval(b)?);
        self.push(&b_layout)?;
        let _c_layout = continues!(self.eval(c)?);

        Ok(match op {
            PtrCopy | PtrMove => {
                let sym = match op {
                    PtrCopy => "memcpy",
                    PtrMove => "memmove",
                    _ => panic!(),
                };
                let elem_size = self.ctx.layout(&Ct::ptr_elem(c.ty().unwrap())).size;
                self.w.movq(Rdi, Rax)?; // third operand (dest) -> buf1
                self.pop_eightbyte(Rdx)?; // second operand (len) -> n
                self.w.imulq(Rdx, Rdx, elem_size as i32)?;
                self.pop_eightbyte(Rsi)?; // first operand (src) -> buf2
                self.eval_builtin_call(sym, &Layout::pointer())?;
                Some(Layout::unit())
            }
            ArrayStore => {
                let elem_layout = b_layout;
                self.w.movq(Rsi, Rax)?; // third operand (array)
                self.pop(&elem_layout)?; // second operand (value)
                let index = memory(Rsp + elem_layout.size_in_stack(true) as i32); // first operand (index)
                self.w.imulq(Rdi, index, elem_layout.size as i32)?;
                self.store(Rsi + Rdi, &elem_layout)?;
                self.shrink_stack(&Layout::integer(8))?;
                Some(Layout::unit())
            }
        })
    }

    fn eval_let_var(&mut self, vars: &[RtVar], body: &Rt) -> io::Result<Option<Layout>> {
        for v in vars {
            let layout = continues!(self.eval(&v.init)?).in_stack();
            let offset = *self.stack_frame.var_offsets.get(&v.id).unwrap();
            self.store(Rbp + offset, &layout)?;
        }

        self.eval(body)
    }

    fn eval_let_cont(&mut self, conts: &[RtCont], body: &Rt) -> io::Result<Option<Layout>> {
        for cont in conts {
            let label = self.w.issue_label();
            let params = cont.params.iter().map(|p| p.id).collect();
            self.local_conts.insert(
                cont.id,
                LocalCont::new(label, params, self.stack_frame.depth),
            );
        }
        let merge_label = self.w.issue_label();
        let mut merge_l = None;

        if let Some(l) = self.eval(body)? {
            self.w.jmpq(merge_label)?;
            merge_l = Some((l, self.stack_frame.depth));
        }

        for cont in conts {
            let local_cont = self.local_conts.remove(&cont.id).unwrap();
            self.w.define(local_cont.label, false);
            self.stack_frame.depth = local_cont.depth;

            if let Some(l) = self.eval(&cont.body)? {
                self.w.jmpq(merge_label)?;
                if merge_l.is_some() {
                    assert_eq!(merge_l, Some((l, self.stack_frame.depth)));
                } else {
                    merge_l = Some((l, self.stack_frame.depth));
                }
            }
        }

        self.w.define(merge_label, false);
        Ok(merge_l.map(|(l, depth)| {
            self.stack_frame.depth = depth;
            l
        }))
    }

    fn eval_const(&mut self, c: &Const) -> io::Result<Layout> {
        Ok(match c {
            Const::Integer(ty, signed, value) => match ty {
                Ct::F32 | Ct::F64 => {
                    let value = match *signed {
                        true => *value as i64 as f64,
                        false => *value as f64,
                    };
                    self.eval_const(&Const::FPNumber(ty.clone(), value.into()))?
                }
                Ct::S(_) | Ct::U(_) => {
                    self.w.movq(Rax, *value as i64)?;
                    self.ctx.layout(ty)
                }
                _ => panic!("Unsupported Integer: {}", ty),
            },
            Const::FPNumber(ty, f) => match ty {
                Ct::F32 => {
                    let value = (f.into_inner() as f32).to_bits() as i32;
                    self.w.movl(Eax, value)?;
                    self.w.movq(Xmm0, Rax)?;
                    Layout::floating_point(4)
                }
                Ct::F64 => {
                    let value = f.into_inner().to_bits() as i64;
                    self.w.movq(Rax, value)?;
                    self.w.movq(Xmm0, Rax)?;
                    Layout::floating_point(8)
                }
                _ => panic!("Unsupported FPNumber: {}", ty),
            },
            Const::String(s) => {
                if s.is_empty() {
                    // {rax, rdx} <- {null, 0}
                    self.w.xorl(Eax, Eax)?;
                    self.w.xorl(Edx, Edx)?;
                } else {
                    // {rax, rdx} <- {data-ptr, len}
                    let label = self.embed_rodata(&Rep::bytes(s.as_bytes()))?;
                    self.w.leaq(Rax, label)?;
                    self.w.movq(Rdx, s.len() as i64)?;
                }
                Layout::string()
            }
            Const::Char(c) => {
                self.w.movl(Eax, *c as i32)?;
                Layout::char()
            }
            Const::SyntaxSexp(_, sexp) => {
                let sexp = NativeSyntax::<NativeSexp>::from_host(sexp.as_ref().clone());
                let label = self.embed_rodata(&Rep::of(&sexp))?;
                self.w.movq(Rax, label)?;
                Layout::syntax()
            }
            Const::Unit => Layout::unit(),
        })
    }

    fn embed_rodata(&mut self, rep: &Rep) -> io::Result<Label> {
        self.w.rodata().align(rep.align as u64)?;
        let label = self.w.issue_label();
        let location = self.w.rodata().location();
        self.w.rodata().define(label, false);
        self.w.rodata().write_all(&rep.direct)?;
        for (offset, rep) in rep.indirect.iter() {
            let label = self.embed_rodata(rep)?;
            let location = location.offset(*offset as i64);
            self.w.r#use(location, label, 0, RelocType::Abs64);
        }
        Ok(label)
    }
}

impl<'a> StackAwareInstWriter for FunctionCodegen<'a> {
    fn w(&mut self) -> &mut Writer {
        self.w
    }

    fn depth(&mut self) -> &mut usize {
        &mut self.stack_frame.depth
    }
}

#[derive(Debug, Clone, new)]
struct LocalCont {
    label: Label,
    params: Vec<RtId>,
    depth: usize,
}
