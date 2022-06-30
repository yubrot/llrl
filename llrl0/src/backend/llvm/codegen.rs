use super::runtime;
use super::{CFunctionArtifact, ContextArtifact, FunctionArtifact, ModuleArtifact};
use crate::lowering::ir::*;
use derive_new::new;
use llvm::prelude::*;
use std::collections::HashMap;

pub fn run<'ctx: 'm, 'm>(
    function: &FunctionArtifact<'ctx, 'm>,
    def: &Function,
    module: &mut ModuleArtifact<'ctx, 'm>,
    ctx: &ContextArtifact<'ctx>,
) {
    let entry_bb = function.value.append_block("entry");
    let builder = LLVMBuilder::new(entry_bb);
    let mut values = HashMap::new();

    let (ret_pointer, env_param, params) = {
        let mut params = function.value.params();
        let ret_pointer = if function.symbol.kind.returns_by_pointer_store() {
            Some(params.remove(0))
        } else {
            None
        };
        let env_param = if function.symbol.kind.takes_env_as_argument() {
            Some(params.remove(0))
        } else {
            None
        };
        (ret_pointer, env_param, params)
    };

    // Bind arguments
    for (p, param) in def.params.iter().zip(&params) {
        values.insert(p.id, *param);
    }

    if let (Some(env_param), Some(env)) = (env_param, &def.env) {
        // Bind an environment argument
        values.insert(env.id, env_param);

        let env_param = builder.build_bit_cast(env_param, {
            let elem_tys = ctx.llvm_type_all(env.elems.iter().map(|p| &p.ty));
            llvm_type!(*ctx, (ptr (struct ...{elem_tys})))
        });

        // Bind the environment argument elements
        for (index, elem) in env.elems.iter().enumerate() {
            let param = builder.build_struct_gep(env_param, index as u32);
            let param = builder.build_load(param);
            values.insert(elem.id, param);
        }
    }

    if function.symbol.kind.is_main() {
        let llrt_init = module.capture_c_function(
            "llrt_init",
            ctx,
            || llvm_type!(*ctx, (function(i32 (ptr (ptr u8))) void)),
        );
        builder.build_call(llrt_init.value, &params);
    }

    let mut codegen = Codegen::new(ctx, module, builder, ret_pointer, values, HashMap::new());
    if let Some(ret) = codegen.eval(&def.body) {
        codegen.eval_return(ret);
    }
}

#[derive(new)]
struct Codegen<'a, 'ctx: 'm, 'm> {
    ctx: &'a ContextArtifact<'ctx>,
    module: &'a mut ModuleArtifact<'ctx, 'm>,
    builder: LLVMBox<LLVMBuilder<'ctx, 'm>>,
    ret_pointer: Option<LLVMValue<'ctx, 'm>>,
    local_values: HashMap<RtId, LLVMValue<'ctx, 'm>>,
    local_conts: HashMap<RtId, Cont<'ctx, 'm>>,
}

impl<'a, 'ctx: 'm, 'm> Codegen<'a, 'ctx, 'm> {
    fn eval(&mut self, rt: &Rt) -> Option<LLVMValue<'ctx, 'm>> {
        match rt {
            Rt::Local(id) => match self.local_values.get(id) {
                Some(value) => Some(*value),
                None => panic!("Undefined variable: {}", id),
            },
            Rt::LocalFun(_, _) => {
                panic!("Found Rt::LocalFun at Codegen, this should be erased by lowerizer")
            }
            Rt::StaticFun(Ct::Id(id), env) => {
                let fp = self.module.capture_function(*id, self.ctx).value;
                let env = match env {
                    Some(env) => self.eval(env)?,
                    None => {
                        let ty = self.ctx.llvm_type(&Ct::Env).as_type_of().unwrap();
                        llvm_constant!(*self.ctx, (nullptr { ty })).as_value()
                    }
                };
                let value = llvm_constant!(*self.ctx, (undef (struct (typeof fp) (typeof env))));
                let value = self.builder.build_insert_value(value, fp, 0);
                let value = self.builder.build_insert_value(value, env, 1);
                Some(value)
            }
            Rt::StaticFun(ct, _) => {
                panic!(
                    "Unresolved Ct: {}, this should be resolved by lowerizer",
                    ct
                )
            }
            Rt::Const(c) => Some(self.llvm_constant(c).as_value()),
            Rt::Call(call) => {
                let clos = self.eval(&call.0)?;
                let args = self.eval_all(&call.1)?;
                Some(self.eval_call(clos, &args))
            }
            Rt::CCall(c_call) => {
                let args = self.eval_all(&c_call.2)?;
                let ctx = self.ctx;
                let f = self
                    .module
                    .capture_c_function(&c_call.0, ctx, || match c_call.1 {
                        Ct::Clos(ref clos) => {
                            let params = ctx.llvm_type_all(&clos.0);
                            let ret = match clos.1 {
                                Ct::Unit => llvm_type!(*ctx, void).as_type(),
                                ref ty => ctx.llvm_type(ty),
                            };
                            llvm_type!(*ctx, (function(...{params}) {ret}))
                        }
                        ref ty => panic!("CCall type is not a function type: {}", ty),
                    });
                Some(self.eval_c_call(f, &args))
            }
            Rt::Nullary(nullary) => {
                use Nullary::*;
                Some(match nullary {
                    Uninitialized(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        llvm_constant!(*self.ctx, (undef { ty })).as_value()
                    }
                    Null(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        llvm_constant!(*self.ctx, (nullptr(ptr { ty }))).as_value()
                    }
                    GenId => runtime::build_string_genid(&self.builder, self.module),
                    SizeOf(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        let size = self.ctx.data_layout().type_alloc_size(ty);
                        llvm_constant!(*self.ctx, (u64 { size })).as_value()
                    }
                    AlignOf(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        let align = self.ctx.data_layout().abi_type_alignment(ty);
                        llvm_constant!(*self.ctx, (u64 { align })).as_value()
                    }
                })
            }
            Rt::Unary(unary) => {
                use Unary::*;
                let x = self.eval(&unary.1)?;
                Some(match &unary.0 {
                    Not => self.builder.build_not(x),
                    Load => self.builder.build_load(x),
                    StructElem(_, i) => self.builder.build_extract_value(x, *i as u32),
                    Reinterpret(a, b) => {
                        let a = self.ctx.llvm_type(a);
                        let b = self.ctx.llvm_type(b);
                        self.eval_reinterpret(a, b, x)
                    }
                    SyntaxBody(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        runtime::build_syntax_body(ty, x, &self.builder, self.module)
                    }
                    Panic => {
                        runtime::build_panic(x, &self.builder, self.module);
                        return None;
                    }
                    BitCast(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_bit_cast(x, ty)
                    }
                    PtrToI => self.builder.build_ptr_to_int(x, llvm_type!(*self.ctx, u64)),
                    IToPtr(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder
                            .build_int_to_ptr(x, llvm_type!(*self.ctx, (ptr { ty })))
                    }
                    IComplement => {
                        let int_ty = x
                            .get_type()
                            .as_type_of::<LLVMIntegerType>()
                            .expect("Integer type");
                        let y = LLVMConstantInt::get(int_ty, -1i64 as u64, true);
                        self.builder.build_xor(x, y)
                    }
                    ITrunc(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_trunc(x, ty)
                    }
                    IPopCount => {
                        let int_ty = x
                            .get_type()
                            .as_type_of::<LLVMIntegerType>()
                            .expect("Integer type");
                        let function = match int_ty.bit_width() {
                            8 => self.module.ctpop_i8(),
                            16 => self.module.ctpop_i16(),
                            32 => self.module.ctpop_i32(),
                            64 => self.module.ctpop_i64(),
                            _ => panic!("popcount is not defined for type: {}", int_ty),
                        };
                        self.builder.build_call(function, &[x])
                    }
                    SExt(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_sext(x, ty)
                    }
                    SToF(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_si_to_fp(x, ty)
                    }
                    UExt(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_zext(x, ty)
                    }
                    UToF(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_ui_to_fp(x, ty)
                    }
                    FToS(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_fp_to_si(x, ty)
                    }
                    FToU(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_fp_to_ui(x, ty)
                    }
                    FTrunc(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_fptrunc(x, ty)
                    }
                    FExt(ty) => {
                        let ty = self.ctx.llvm_type(ty);
                        self.builder.build_fpext(x, ty)
                    }
                    RealCeil | RealFloor | RealTrunc | RealRound => {
                        use llvm::TypeKind::*;
                        let function = match (&unary.0, x.get_type().kind()) {
                            (RealCeil, LLVMFloatTypeKind) => self.module.ceil_f32(),
                            (RealCeil, LLVMDoubleTypeKind) => self.module.ceil_f64(),
                            (RealFloor, LLVMFloatTypeKind) => self.module.floor_f32(),
                            (RealFloor, LLVMDoubleTypeKind) => self.module.floor_f64(),
                            (RealTrunc, LLVMFloatTypeKind) => self.module.trunc_f32(),
                            (RealTrunc, LLVMDoubleTypeKind) => self.module.trunc_f64(),
                            (RealRound, LLVMFloatTypeKind) => self.module.round_f32(),
                            (RealRound, LLVMDoubleTypeKind) => self.module.round_f64(),
                            (op, _) => panic!("{} is not defined for type: {}", op, x.get_type()),
                        };
                        self.builder.build_call(function, &[x])
                    }
                    MathSqrt | MathSin | MathCos | MathExp | MathLog => {
                        use llvm::TypeKind::*;
                        let function = match (&unary.0, x.get_type().kind()) {
                            (MathSqrt, LLVMFloatTypeKind) => self.module.sqrt_f32(),
                            (MathSqrt, LLVMDoubleTypeKind) => self.module.sqrt_f64(),
                            (MathSin, LLVMFloatTypeKind) => self.module.sin_f32(),
                            (MathSin, LLVMDoubleTypeKind) => self.module.sin_f64(),
                            (MathCos, LLVMFloatTypeKind) => self.module.cos_f32(),
                            (MathCos, LLVMDoubleTypeKind) => self.module.cos_f64(),
                            (MathExp, LLVMFloatTypeKind) => self.module.exp_f32(),
                            (MathExp, LLVMDoubleTypeKind) => self.module.exp_f64(),
                            (MathLog, LLVMFloatTypeKind) => self.module.log_f32(),
                            (MathLog, LLVMDoubleTypeKind) => self.module.log_f64(),
                            (op, _) => panic!("{} is not defined for type: {}", op, x.get_type()),
                        };
                        self.builder.build_call(function, &[x])
                    }
                    StringPtr => runtime::build_string_getptr(x, &self.builder),
                    StringLength => runtime::build_string_getlen(x, &self.builder),
                    ArrayPtr => runtime::build_array_getptr(x, &self.builder),
                    ArrayLength => runtime::build_array_getlen(x, &self.builder),
                })
            }
            Rt::Binary(binary) => {
                use Binary::*;
                let x = self.eval(&binary.1)?;
                let y = self.eval(&binary.2)?;
                Some(match &binary.0 {
                    Store => {
                        self.builder.build_store(x, y);
                        self.llvm_constant(&Const::Unit).as_value()
                    }
                    Offset => self.builder.build_gep(y, &[x]),
                    PtrEq => self.builder.build_eq(x, y),
                    PtrLt => self.builder.build_ult(x, y),
                    PtrLe => self.builder.build_ule(x, y),
                    PtrGt => self.builder.build_ugt(x, y),
                    PtrGe => self.builder.build_uge(x, y),
                    IEq => self.builder.build_eq(x, y),
                    IShl => self.builder.build_shl(x, y),
                    IAShr => self.builder.build_ashr(x, y),
                    ILShr => self.builder.build_lshr(x, y),
                    IAnd => self.builder.build_and(x, y),
                    IOr => self.builder.build_or(x, y),
                    IXor => self.builder.build_xor(x, y),
                    SLt => self.builder.build_slt(x, y),
                    SLe => self.builder.build_sle(x, y),
                    SGt => self.builder.build_sgt(x, y),
                    SGe => self.builder.build_sge(x, y),
                    SAdd => self.builder.build_nswadd(x, y),
                    SSub => self.builder.build_nswsub(x, y),
                    SMul => self.builder.build_nswmul(x, y),
                    SDiv => self.builder.build_sdiv(x, y),
                    SRem => self.builder.build_srem(x, y),
                    ULt => self.builder.build_ult(x, y),
                    ULe => self.builder.build_ule(x, y),
                    UGt => self.builder.build_ugt(x, y),
                    UGe => self.builder.build_uge(x, y),
                    UAdd => self.builder.build_add(x, y),
                    USub => self.builder.build_sub(x, y),
                    UMul => self.builder.build_mul(x, y),
                    UDiv => self.builder.build_udiv(x, y),
                    URem => self.builder.build_urem(x, y),
                    FEq => self.builder.build_feq(x, y),
                    FLt => self.builder.build_flt(x, y),
                    FLe => self.builder.build_fle(x, y),
                    FGt => self.builder.build_fgt(x, y),
                    FGe => self.builder.build_fge(x, y),
                    FAdd => self.builder.build_fadd(x, y),
                    FSub => self.builder.build_fsub(x, y),
                    FMul => self.builder.build_fmul(x, y),
                    FDiv => self.builder.build_fdiv(x, y),
                    FRem => self.builder.build_frem(x, y),
                    MathPow => {
                        let function = match x.get_type().kind() {
                            llvm::TypeKind::LLVMFloatTypeKind => self.module.pow_f32(),
                            llvm::TypeKind::LLVMDoubleTypeKind => self.module.pow_f64(),
                            _ => panic!("{} is not defined for type: {}", MathPow, x.get_type()),
                        };
                        self.builder.build_call(function, &[x, y])
                    }
                    StringConstruct => runtime::build_string_construct(x, y, &self.builder),
                    StringEq => runtime::build_string_eq(x, y, &self.builder, self.module),
                    StringCmp => runtime::build_string_cmp(x, y, &self.builder, self.module),
                    StringConcat => runtime::build_string_concat(x, y, &self.builder, self.module),
                    CharEq => runtime::build_char_eq(x, y, &self.builder),
                    ArrayConstruct => runtime::build_array_construct(x, y, &self.builder),
                    ArrayLoad => runtime::build_array_load(x, y, &self.builder),
                })
            }
            Rt::Ternary(ternary) => {
                use Ternary::*;
                let x = self.eval(&ternary.1)?;
                let y = self.eval(&ternary.2)?;
                let z = self.eval(&ternary.3)?;
                Some(match &ternary.0 {
                    PtrCopy | PtrMove => {
                        let f = match &ternary.0 {
                            PtrCopy => self.module.memcpy_i64(),
                            _ => self.module.memmove_i64(),
                        };
                        let ptr_ty = llvm_type!(*self.ctx, (ptr i8));
                        let value_ty = x
                            .get_type()
                            .as_type_of::<LLVMPointerType>()
                            .unwrap()
                            .element_type();

                        let src = self.builder.build_bit_cast(x, ptr_ty);
                        let len = self.ctx.data_layout().type_alloc_size(value_ty);
                        let len = llvm_constant!(*self.ctx, (u64 { len })).as_value();
                        let len = self.builder.build_mul(len, y);
                        let dest = self.builder.build_bit_cast(z, ptr_ty);
                        let is_volatile = llvm_constant!(*self.ctx, (bool false)).as_value();
                        self.builder.build_call(f, &[dest, src, len, is_volatile]);
                        self.llvm_constant(&Const::Unit).as_value()
                    }
                    ArrayStore => {
                        runtime::build_array_store(x, y, z, &self.builder);
                        self.llvm_constant(&Const::Unit).as_value()
                    }
                })
            }
            Rt::Alloc(alloc) => {
                let a = self.eval(&alloc.1)?;
                let ptr = self.eval_alloc(alloc.0, a.get_type());
                self.builder.build_store(a, ptr);
                Some(ptr)
            }
            Rt::AllocArray(alloc) => {
                let ty = self.ctx.llvm_type(&alloc.1);
                let len = self.eval(&alloc.2)?;
                let ptr = self.eval_array_alloc(alloc.0, ty, len);
                Some(runtime::build_array_construct(ptr, len, &self.builder))
            }
            Rt::ConstructEnv(con) => {
                let elems = self.eval_all(&con.1)?;

                let ptr = self.eval_alloc(con.0, {
                    let elem_tys = elems.iter().map(|v| v.get_type()).collect::<Vec<_>>();
                    llvm_type!(*self.ctx, (struct ...{elem_tys}))
                });

                for (index, elem) in elems.iter().enumerate() {
                    let elem_ptr = self.builder.build_struct_gep(ptr, index as u32);
                    self.builder.build_store(*elem, elem_ptr);
                }

                let env_ty = self.ctx.llvm_type(&Ct::Env);
                Some(self.builder.build_bit_cast(ptr, env_ty))
            }
            Rt::ConstructData(_) => {
                panic!("Found Rt::ConstructData at Codegen, this should be erased by lowerizer")
            }
            Rt::ConstructStruct(con) => {
                let mut result = LLVMConstant::undef(self.ctx.llvm_type(&con.0)).as_value();
                for (index, field) in con.1.iter().enumerate() {
                    let value = self.eval(field)?;
                    result = self.builder.build_insert_value(result, value, index as u32);
                }
                Some(result)
            }
            Rt::ConstructSyntax(con) => {
                let x = self.eval(&con.1)?;
                let x = runtime::build_syntax_construct(con.0, x, &self.builder, self.module);
                Some(x)
            }
            Rt::Seq(seq) => {
                for stmt in seq.0.iter() {
                    self.eval(stmt)?;
                }
                self.eval(&seq.1)
            }
            Rt::If(if_) => self.eval_if(&if_.0, &if_.1, &if_.2),
            Rt::While(while_) => self.eval_while(&while_.0, &while_.1),
            Rt::And(_) => {
                panic!("Found Rt::And at Codegen, this should be erased by lowerizer")
            }
            Rt::Or(_) => {
                panic!("Found Rt::Or at Codegen, this should be erased by lowerizer")
            }
            Rt::Match(_) => {
                panic!("Found Rt::Match at Codegen, this should be erased by lowerizer")
            }
            Rt::Return(ret) => {
                let ret = self.eval(ret)?;
                self.eval_return(ret);
                None
            }
            Rt::Cont(id, args) => {
                let args = self.eval_all(args)?;
                match self.local_conts.remove(id) {
                    Some(mut cont) => {
                        cont.enter(args, self);
                        self.local_conts.insert(*id, cont);
                        None
                    }
                    // NOTE: Continuation recursion is unsupported at the moment
                    None => panic!("Undefined continuation: {}", id),
                }
            }
            Rt::Never => {
                self.builder.build_unreachable();
                None
            }
            Rt::LetFunction(_) => {
                panic!("Found Rt::LetFunction at Codegen, this should be erased by lowerizer")
            }
            Rt::LetVar(let_) => self.eval_let_var(&let_.0, &let_.1),
            Rt::LetCont(let_) => self.eval_let_cont(&let_.0, &let_.1),
        }
    }

    fn eval_all<'b>(
        &mut self,
        rts: impl IntoIterator<Item = &'b Rt>,
    ) -> Option<Vec<LLVMValue<'ctx, 'm>>> {
        rts.into_iter().map(|arg| self.eval(arg)).collect()
    }

    fn eval_call(
        &mut self,
        clos: impl LLVMAnyValue<'ctx, 'm>,
        args: &[LLVMValue<'ctx, 'm>],
    ) -> LLVMValue<'ctx, 'm> {
        let fp = self.builder.build_extract_value(clos, 0);
        let env = self.builder.build_extract_value(clos, 1);
        let args = std::iter::once(env)
            .chain(args.iter().copied())
            .collect::<Vec<_>>();
        self.builder.build_call(fp, &args)
    }

    fn eval_c_call(
        &mut self,
        cfun: CFunctionArtifact<'ctx, 'm>,
        args: &[LLVMValue<'ctx, 'm>],
    ) -> LLVMValue<'ctx, 'm> {
        if cfun.return_by_pointer_store {
            let ret_param = cfun.value.params()[0];
            let ret_ptr = self.builder.build_entry_alloca(
                "rettmp",
                ret_param
                    .get_type()
                    .as_type_of::<LLVMPointerType>()
                    .unwrap()
                    .element_type(),
            );
            let args = std::iter::once(ret_ptr)
                .chain(args.iter().copied())
                .collect::<Vec<_>>();
            self.builder.build_call(cfun.value, &args);
            self.builder.build_load(ret_ptr)
        } else {
            let ret = self.builder.build_call(cfun.value, args);
            if ret.get_type().is_sized() {
                ret
            } else {
                self.llvm_constant(&Const::Unit).as_value()
            }
        }
    }

    fn eval_if(&mut self, cond: &Rt, then: &Rt, else_: &Rt) -> Option<LLVMValue<'ctx, 'm>> {
        let cond = self.eval(cond)?;

        let then_bb = self.builder.append_block("then");
        let else_bb = self.builder.append_block("else");
        self.builder.build_cond_br(cond, then_bb, else_bb);

        let mut merge = Cont::new("merge", |phis, _| Some(phis[0].as_value()));

        self.builder.set_insert_point(then_bb, true);
        if let Some(then) = self.eval(then) {
            merge.enter(vec![then], self);
        }

        self.builder.set_insert_point(else_bb, true);
        if let Some(else_) = self.eval(else_) {
            merge.enter(vec![else_], self);
        }

        merge.continue_(self)
    }

    fn eval_while(&mut self, cond: &Rt, body: &Rt) -> Option<LLVMValue<'ctx, 'm>> {
        let cond_bb = self.builder.append_block("cond");
        self.builder.build_br(cond_bb);

        self.builder.set_insert_point(cond_bb, true);
        let cond = self.eval(cond)?;

        let then_bb = self.builder.append_block("then");
        let else_bb = self.builder.append_block("else");
        self.builder.build_cond_br(cond, then_bb, else_bb);

        self.builder.set_insert_point(then_bb, true);
        if self.eval(body).is_some() {
            self.builder.build_br(cond_bb);
        }

        self.builder.set_insert_point(else_bb, true);
        Some(self.llvm_constant(&Const::Unit).as_value())
    }

    fn eval_let_var(&mut self, vars: &Vec<RtVar>, body: &Rt) -> Option<LLVMValue<'ctx, 'm>> {
        for var in vars {
            let init = self.eval(&var.init)?;
            self.local_values.insert(var.id, init);
        }

        self.eval(body)
    }

    fn eval_let_cont(&mut self, conts: &Vec<RtCont>, body: &Rt) -> Option<LLVMValue<'ctx, 'm>> {
        for cont in conts {
            self.local_conts.insert(cont.id, {
                let params = cont.params.iter().map(|p| p.id).collect::<Vec<_>>();
                let body = cont.body.clone();
                Cont::boxed("cont", move |args, self_| {
                    assert_eq!(args.len(), params.len());
                    for (param, arg) in params.iter().zip(args) {
                        self_.local_values.insert(*param, arg.as_value());
                    }

                    self_.eval(&body)
                })
            });
        }

        let mut merge = Cont::new("merge", |phis, _| Some(phis[0].as_value()));

        if let Some(ret) = self.eval(body) {
            merge.enter(vec![ret], self);
        }

        for cont in conts {
            let cont = self.local_conts.remove(&cont.id).unwrap();
            if let Some(ret) = cont.continue_(self) {
                merge.enter(vec![ret], self);
            }
        }

        merge.continue_(self)
    }

    fn eval_alloc(&mut self, loc: Location, ty: impl LLVMAnyType<'ctx>) -> LLVMValue<'ctx, 'm> {
        match loc {
            Location::Heap => runtime::build_heap_alloc(ty, &self.builder, self.module),
            Location::StackStatic => self.builder.build_entry_alloca("", ty),
            Location::StackDynamic => self.builder.build_alloca("", ty),
        }
    }

    fn eval_array_alloc(
        &mut self,
        loc: Location,
        ty: impl LLVMAnyType<'ctx>,
        num: impl LLVMAnyValue<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        match loc {
            Location::Heap => runtime::build_heap_array_alloc(ty, num, &self.builder, self.module),
            Location::StackStatic => match num.as_value_of::<llvm::ConstantInt>() {
                Some(c) => {
                    let _value = c.zext_value();
                    panic!("Not implemented")
                }
                None => panic!(
                    "Cannot determine the size of stackalloc: {}",
                    num.as_value()
                ),
            },
            Location::StackDynamic => self.builder.build_array_alloca("", ty, num),
        }
    }

    fn eval_reinterpret(
        &mut self,
        a: impl LLVMAnyType<'ctx>,
        b: impl LLVMAnyType<'ctx>,
        x: impl LLVMAnyValue<'ctx, 'm>,
    ) -> LLVMValue<'ctx, 'm> {
        let a_size = self.ctx.data_layout().type_alloc_size(a);
        let b_size = self.ctx.data_layout().type_alloc_size(b);

        if a_size < b_size {
            let ptr = self.builder.build_entry_alloca("reinterpret", b);
            let a_ptr = self.builder.build_bit_cast(ptr, LLVMPointerType::get(a, 0));
            self.builder.build_store(x, a_ptr);
            self.builder.build_load(ptr)
        } else {
            let ptr = self.builder.build_entry_alloca("reinterpret", a);
            self.builder.build_store(x, ptr);
            let b_ptr = self.builder.build_bit_cast(ptr, LLVMPointerType::get(b, 0));
            self.builder.build_load(b_ptr)
        }
    }

    fn eval_return(&mut self, x: impl LLVMAnyValue<'ctx, 'm>) {
        match self.ret_pointer {
            Some(ptr) => {
                self.builder.build_store(x, ptr);
                self.builder.build_ret_void();
            }
            None => {
                self.builder.build_ret(x);
            }
        }
    }

    pub fn llvm_constant(&self, c: &Const) -> LLVMConstant<'ctx, 'm> {
        match c {
            Const::Integer(ty, signed, value) => match ty {
                Ct::F32 | Ct::F64 => LLVMConstantFP::get(
                    self.ctx.llvm_type(ty).as_type_of().unwrap(),
                    if *signed {
                        *value as i64 as f64
                    } else {
                        *value as f64
                    },
                )
                .as_constant(),
                _ => LLVMConstantInt::get(
                    self.ctx.llvm_type(ty).as_type_of().unwrap(),
                    *value,
                    *signed,
                )
                .as_constant(),
            },
            Const::FPNumber(ty, value) => LLVMConstantFP::get(
                self.ctx.llvm_type(ty).as_type_of().unwrap(),
                value.into_inner(),
            )
            .as_constant(),
            Const::String(s) => runtime::string_constant(s, self.module.module()),
            Const::Char(c) => runtime::char_constant(*c, self.module.module()),
            Const::SyntaxSexp(_, s) => {
                runtime::syntax_constant::<runtime::NativeSexp>(s, self.module.module())
            }
            Const::Unit => llvm_constant!(*self.ctx, (struct)).as_constant(),
        }
    }
}

#[derive(Debug)]
enum Cont<
    'ctx: 'p,
    'p,
    F = Box<
        dyn FnOnce(
            &[LLVMPhiNode<'ctx, 'p>],
            &mut Codegen<'_, 'ctx, 'p>,
        ) -> Option<LLVMValue<'ctx, 'p>>,
    >,
> {
    Never(&'static str, F),
    Reach(
        LLVMBasicBlock<'ctx, 'p>,
        Vec<LLVMPhiNode<'ctx, 'p>>,
        Option<(LLVMBasicBlock<'ctx, 'p>, LLVMValue<'ctx, 'p>)>,
    ),
}

impl<'ctx: 'p, 'p, F> Cont<'ctx, 'p, F>
where
    F: FnOnce(&[LLVMPhiNode<'ctx, 'p>], &mut Codegen<'_, 'ctx, 'p>) -> Option<LLVMValue<'ctx, 'p>>,
{
    fn new(name: &'static str, f: F) -> Self {
        Self::Never(name, f)
    }

    fn boxed(name: &'static str, f: F) -> Cont<'ctx, 'p>
    where
        F: 'static,
    {
        Cont::Never(name, Box::new(f))
    }

    fn enter(&mut self, args: Vec<LLVMValue<'ctx, 'p>>, b: &mut Codegen<'_, 'ctx, 'p>) {
        let current_bb = b.builder.insert_point();

        if let Self::Never(name, _) = self {
            let cont_bb = current_bb.parent().append_block(name);
            b.builder.set_insert_point(cont_bb, true);

            let phis = args
                .iter()
                .map(|arg| b.builder.build_phi(arg.get_type()))
                .collect::<Vec<_>>();

            let f = match std::mem::replace(self, Self::Reach(cont_bb, phis, None)) {
                Self::Never(_, f) => f,
                _ => unreachable!(),
            };

            match self {
                Self::Reach(_, phis, ret) => {
                    *ret = f(phis, b).map(|value| (b.builder.insert_point(), value));
                }
                _ => unreachable!(),
            }

            b.builder.set_insert_point(current_bb, true);
        }

        match self {
            Self::Reach(bb, phis, _) => {
                b.builder.build_br(*bb);
                for (phi, arg) in phis.iter().zip(args) {
                    phi.add_incoming(arg, current_bb);
                }
            }
            _ => unreachable!(),
        }
    }

    fn continue_(&self, b: &mut Codegen<'_, 'ctx, 'p>) -> Option<LLVMValue<'ctx, 'p>> {
        match self {
            Self::Never(_, _) => None,
            Self::Reach(_, _, None) => None,
            Self::Reach(_, _, Some((bb, value))) => {
                b.builder.set_insert_point(*bb, true);
                Some(*value)
            }
        }
    }
}
