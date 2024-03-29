use super::codegen;
use super::runtime;
use crate::backend::native::mem_layout::LayoutResolver;
use crate::lowering::ir::*;
use derive_new::new;
use llvm::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct ContextArtifact<'ctx> {
    context: &'ctx LLVMContext,
    data_layout: LLVMBox<LLVMDataLayout>,
    layout_resolver: LayoutResolver,
    types: HashMap<CtId, LLVMType<'ctx>>,
    structs: HashMap<CtId, LLVMStructType<'ctx>>,
    unions: HashMap<CtId, LLVMArrayType<'ctx>>,
    function_symbols: HashMap<CtId, FunctionSymbol<'ctx>>,
    main_function_symbol: Option<FunctionSymbol<'ctx>>,
}

impl<'ctx> ContextArtifact<'ctx> {
    pub fn new(context: &'ctx LLVMContext, data_layout: &LLVMDataLayout) -> Self {
        Self {
            context,
            data_layout: data_layout.to_owned(),
            layout_resolver: LayoutResolver::new(),
            types: HashMap::new(),
            structs: HashMap::new(),
            unions: HashMap::new(),
            function_symbols: HashMap::new(),
            main_function_symbol: None,
        }
    }

    pub fn context(&self) -> &'ctx LLVMContext {
        self.context
    }

    pub fn data_layout(&self) -> &LLVMDataLayout {
        &self.data_layout
    }

    pub fn function_symbol(&self, id: CtId) -> Option<&FunctionSymbol<'ctx>> {
        self.function_symbols.get(&id)
    }

    pub fn main_function_symbol(&self) -> Option<&FunctionSymbol<'ctx>> {
        self.main_function_symbol.as_ref()
    }

    pub fn add_types(&mut self, defs: &HashMap<CtId, Arc<Def>>) {
        self.layout_resolver.register(defs);

        // Put type headers
        for (id, def) in defs {
            match **def {
                Def::Struct(_) => {
                    let ty = LLVMStructType::new(&id.index().to_string(), self.context);
                    self.structs.insert(*id, ty);
                    self.types.insert(*id, ty.as_type());
                }
                Def::Union(_) => {
                    let layout = self.layout_resolver.get(&Ct::Id(*id));
                    let ty = if layout.align != 0 {
                        let bw = layout.align * 8;
                        let len = layout.size / layout.align;
                        llvm_type!(*self, [(i bw); len])
                    } else {
                        llvm_type!(*self, [i1; 0])
                    };
                    self.unions.insert(*id, ty);
                    self.types.insert(*id, ty.as_type());
                }
                _ => {}
            }
        }

        // Set struct bodies
        for (id, def) in defs {
            if let Def::Struct(ref def) = **def {
                // Bacause currently we always use C-compatible structures, there is no difference depending on def.repr
                let ty = self.structs.get(id).unwrap();
                ty.set_body(&self.llvm_type_all(&def.fields), false);
            }
        }
    }

    pub fn llvm_type(&self, ty: &Ct) -> LLVMType<'ctx> {
        match ty {
            Ct::Id(id) => *self
                .types
                .get(id)
                .unwrap_or_else(|| panic!("Unknown type: {}", id)),
            Ct::GenericInst(_) => panic!("Found Ct::GenericInst at Codegen::get_type"),
            Ct::TableGet(_) => panic!("Found Ct::TableGet at Codegen::get_type"),
            Ct::Ptr(ty) => llvm_type!(*self, (ptr {self.llvm_type(ty)})).as_type(),
            Ct::Clos(clos) => {
                let env = self.llvm_type(&Ct::Env);
                let params = std::iter::once(env)
                    .chain(clos.0.iter().map(|param| self.llvm_type(param)))
                    .collect::<Vec<_>>();
                let ret = self.llvm_type(&clos.1);
                llvm_type!(*self, (struct (ptr (function(...{params}) {ret})) {env})).as_type()
            }
            Ct::S(bw) => LLVMIntegerType::get(*bw as u32, self.context).as_type(),
            Ct::U(bw) => LLVMIntegerType::get(*bw as u32, self.context).as_type(),
            Ct::F32 => LLVMFPType::float(self.context).as_type(),
            Ct::F64 => LLVMFPType::double(self.context).as_type(),
            Ct::String => runtime::string_type(self.context),
            Ct::Char => runtime::char_type(self.context),
            Ct::Array(ty) => runtime::array_type(self.llvm_type(ty)),
            Ct::CapturedUse => runtime::captured_use_type(self.context),
            Ct::Unit => llvm_type!(*self, (struct)).as_type(),
            Ct::Env => llvm_type!(*self, (ptr i8)).as_type(),
            Ct::Syntax(_) => runtime::syntax_type(self.context),
        }
    }

    pub fn llvm_type_all<'a>(&self, tys: impl IntoIterator<Item = &'a Ct>) -> Vec<LLVMType<'ctx>> {
        tys.into_iter().map(|ty| self.llvm_type(ty)).collect()
    }

    pub fn c_function_signature(
        &self,
        args: &[LLVMValue<'ctx, '_>],
        ret: &Ct,
    ) -> CFunctionSignature<'ctx> {
        let mut params = args.iter().map(|a| a.get_type()).collect::<Vec<_>>();
        let mut ret = match ret {
            Ct::Unit => llvm_type!(*self, void).as_type(),
            ty => self.llvm_type(ty),
        };

        // TODO: This adjustment is not accurate. We need to follow the System V ABI.
        let return_by_pointer_store =
            if ret.is_sized() && 2 * 8 < self.data_layout.type_alloc_size(ret) {
                params.insert(0, llvm_type!(*self, (ptr { ret })).as_type());
                ret = llvm_type!(*self, void).as_type();
                true
            } else {
                false
            };

        CFunctionSignature::new(
            llvm_type!(*self, (function(...{ params }) { ret })),
            return_by_pointer_store,
        )
    }
}

impl<'ctx> LLVMTypeBuilder<'ctx> for ContextArtifact<'ctx> {
    fn context(&self) -> &'ctx LLVMContext {
        self.context()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol<'ctx> {
    pub name: String,
    pub kind: FunctionKind,
    pub ty: LLVMFunctionType<'ctx>,
}

impl<'ctx> FunctionSymbol<'ctx> {
    pub fn new(name: String, def: &Function, ctx: &ContextArtifact<'ctx>) -> Self {
        let param_tys = (def.kind == FunctionKind::Standard)
            .then(|| ctx.llvm_type(&Ct::Env))
            .into_iter()
            .chain(def.params.iter().map(|p| ctx.llvm_type(&p.ty)))
            .collect::<Vec<_>>();
        let ret_ty = ctx.llvm_type(&def.ret);

        let ty = if def.kind == FunctionKind::Macro {
            let mut param_tys = param_tys;
            param_tys.insert(0, llvm_type!(*ctx, (ptr { ret_ty })).as_type());
            llvm_type!(*ctx, (function(...{param_tys}) void))
        } else {
            llvm_type!(*ctx, (function(...{param_tys}) {ret_ty}))
        };

        Self {
            name,
            kind: def.kind,
            ty,
        }
    }
}

#[derive(Debug, Clone, Copy, new)]
pub struct CFunctionSignature<'ctx> {
    pub ty: LLVMFunctionType<'ctx>,
    pub return_by_pointer_store: bool,
}

macro_rules! get_intrinsic_function {
    ($ctx:lifetime, $m:lifetime, $name:ident, $sym:tt, $ty:tt) => {
        pub fn $name(&mut self) -> LLVMFunction<$ctx, $m> {
            if let Some(f) = self.intrinsic_functions.$name {
                f
            } else {
                let ty = llvm_type!(*self.module, $ty);
                let function = self.module.add_function($sym, ty);
                self.intrinsic_functions.$name = Some(function);
                function
            }
        }
    };
}

#[derive(Debug)]
pub struct ModuleArtifact<'ctx: 'm, 'm> {
    module: &'m LLVMModule<'ctx>,
    runtime_library: runtime::Library<'ctx, 'm>,
    functions: HashMap<CtId, FunctionArtifact<'ctx, 'm>>,
    main_function: Option<FunctionArtifact<'ctx, 'm>>,
    c_functions: HashMap<String, CFunctionArtifact<'ctx, 'm>>,
    intrinsic_functions: IntrinsicFunctions<'ctx, 'm>,
}

impl<'ctx: 'm, 'm> ModuleArtifact<'ctx, 'm> {
    pub fn new(module: &'m LLVMModule<'ctx>) -> Self {
        Self {
            module,
            runtime_library: runtime::Library::new(module),
            functions: HashMap::new(),
            main_function: None,
            c_functions: HashMap::new(),
            intrinsic_functions: IntrinsicFunctions::default(),
        }
    }

    pub fn module(&self) -> &'m LLVMModule<'ctx> {
        self.module
    }

    pub fn runtime_library(&self) -> &runtime::Library<'ctx, 'm> {
        &self.runtime_library
    }

    #[allow(dead_code)]
    pub fn function(&self, id: CtId) -> Option<&FunctionArtifact<'ctx, 'm>> {
        self.functions.get(&id)
    }

    pub fn main_function(&self) -> Option<&FunctionArtifact<'ctx, 'm>> {
        self.main_function.as_ref()
    }

    pub fn add_functions(
        &mut self,
        defs: &HashMap<CtId, Arc<Def>>,
        ctx: &mut ContextArtifact<'ctx>,
    ) {
        // Put symbols
        for (id, def) in defs {
            if let Def::Function(ref def) = **def {
                let name = id.index().to_string();
                let symbol = FunctionSymbol::new(name, def, ctx);
                assert!(ctx.function_symbol(*id).is_none());
                ctx.function_symbols.insert(*id, symbol);
            }
        }

        // Generate function bodies
        for (id, def) in defs {
            if let Def::Function(ref def) = **def {
                let function = self.capture_function(*id, ctx).clone();
                codegen::function_body(&function, def, self, ctx);
            }
        }
    }

    pub fn add_main(&mut self, main: Function, ctx: &mut ContextArtifact<'ctx>) {
        assert_eq!(main.kind, FunctionKind::Main);
        let symbol = FunctionSymbol::new("llrl_main".to_string(), &main, ctx);
        let value = self.module.add_function("llrl_main", symbol.ty);
        let function = FunctionArtifact::new(value, symbol);
        codegen::function_body(&function, &main, self, ctx);

        assert!(ctx.main_function_symbol.is_none());
        ctx.main_function_symbol = Some(function.symbol.clone());
        self.main_function = Some(function);
    }

    pub fn add_c_main_adapter(&mut self, ctx: &ContextArtifact<'ctx>) {
        let llrl_main = ctx.main_function_symbol.as_ref().expect("main not found");
        let llrl_main = self.module.add_function(&llrl_main.name, llrl_main.ty);
        codegen::c_main_adapter(llrl_main, self);
    }

    pub fn capture_function(
        &mut self,
        id: CtId,
        ctx: &ContextArtifact<'ctx>,
    ) -> &FunctionArtifact<'ctx, 'm> {
        let module = self.module;
        self.functions.entry(id).or_insert_with(|| {
            let symbol = ctx
                .function_symbol(id)
                .unwrap_or_else(|| panic!("Unresolved function symbol: {}", id))
                .clone();
            let value = module.add_function(&symbol.name, symbol.ty);
            FunctionArtifact::new(value, symbol)
        })
    }

    pub fn capture_c_function(
        &mut self,
        name: &str,
        sig: impl FnOnce() -> CFunctionSignature<'ctx>,
    ) -> CFunctionArtifact<'ctx, 'm> {
        if let Some(function) = self.c_functions.get(name) {
            return *function;
        }
        let sig = sig();
        let function = self.module.add_function(name, sig.ty);
        let artifact = CFunctionArtifact::new(function, sig);
        self.c_functions.insert(name.to_string(), artifact);
        artifact
    }

    get_intrinsic_function!('ctx, 'm, ceil_f32, "llvm.ceil.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, ceil_f64, "llvm.ceil.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, floor_f32, "llvm.floor.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, floor_f64, "llvm.floor.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, trunc_f32, "llvm.trunc.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, trunc_f64, "llvm.trunc.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, round_f32, "llvm.round.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, round_f64, "llvm.round.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, sqrt_f32, "llvm.sqrt.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, sqrt_f64, "llvm.sqrt.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, sin_f32, "llvm.sin.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, sin_f64, "llvm.sin.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, cos_f32, "llvm.cos.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, cos_f64, "llvm.cos.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, pow_f32, "llvm.pow.f32", (function(float float) float));
    get_intrinsic_function!('ctx, 'm, pow_f64, "llvm.pow.f64", (function(double double) double));
    get_intrinsic_function!('ctx, 'm, exp_f32, "llvm.exp.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, exp_f64, "llvm.exp.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, log_f32, "llvm.log.f32", (function(float) float));
    get_intrinsic_function!('ctx, 'm, log_f64, "llvm.log.f64", (function(double) double));
    get_intrinsic_function!('ctx, 'm, ctpop_i8, "llvm.ctpop.i8", (function(i8) i8));
    get_intrinsic_function!('ctx, 'm, ctpop_i16, "llvm.ctpop.i16", (function(i16) i16));
    get_intrinsic_function!('ctx, 'm, ctpop_i32, "llvm.ctpop.i32", (function(i32) i32));
    get_intrinsic_function!('ctx, 'm, ctpop_i64, "llvm.ctpop.i64", (function(i64) i64));
    get_intrinsic_function!('ctx, 'm, memcpy_i64, "llvm.memcpy.p0i8.p0i8.i64", (function((ptr i8) (ptr i8) i64 i1) void));
    get_intrinsic_function!('ctx, 'm, memmove_i64, "llvm.memmove.p0i8.p0i8.i64", (function((ptr i8) (ptr i8) i64 i1) void));
}

impl<'ctx: 'm, 'm> LLVMTypeBuilder<'ctx> for ModuleArtifact<'ctx, 'm> {
    fn context(&self) -> &'ctx LLVMContext {
        self.module().context()
    }
}

impl<'ctx: 'm, 'm> runtime::BuildContext<'ctx, 'm> for ModuleArtifact<'ctx, 'm> {
    fn module(&self) -> &'m LLVMModule<'ctx> {
        self.module()
    }

    fn library(&self) -> &runtime::Library<'ctx, 'm> {
        self.runtime_library()
    }
}

#[derive(Debug, Clone, new)]
pub struct FunctionArtifact<'ctx: 'm, 'm> {
    pub value: LLVMFunction<'ctx, 'm>,
    pub symbol: FunctionSymbol<'ctx>,
}

#[derive(Debug, Clone, Copy, new)]
pub struct CFunctionArtifact<'ctx: 'm, 'm> {
    pub value: LLVMFunction<'ctx, 'm>,
    pub sig: CFunctionSignature<'ctx>,
}

#[derive(Debug, Clone, Default)]
pub struct IntrinsicFunctions<'ctx: 'm, 'm> {
    ceil_f32: Option<LLVMFunction<'ctx, 'm>>,
    ceil_f64: Option<LLVMFunction<'ctx, 'm>>,
    floor_f32: Option<LLVMFunction<'ctx, 'm>>,
    floor_f64: Option<LLVMFunction<'ctx, 'm>>,
    trunc_f32: Option<LLVMFunction<'ctx, 'm>>,
    trunc_f64: Option<LLVMFunction<'ctx, 'm>>,
    round_f32: Option<LLVMFunction<'ctx, 'm>>,
    round_f64: Option<LLVMFunction<'ctx, 'm>>,
    sqrt_f32: Option<LLVMFunction<'ctx, 'm>>,
    sqrt_f64: Option<LLVMFunction<'ctx, 'm>>,
    sin_f32: Option<LLVMFunction<'ctx, 'm>>,
    sin_f64: Option<LLVMFunction<'ctx, 'm>>,
    cos_f32: Option<LLVMFunction<'ctx, 'm>>,
    cos_f64: Option<LLVMFunction<'ctx, 'm>>,
    pow_f32: Option<LLVMFunction<'ctx, 'm>>,
    pow_f64: Option<LLVMFunction<'ctx, 'm>>,
    exp_f32: Option<LLVMFunction<'ctx, 'm>>,
    exp_f64: Option<LLVMFunction<'ctx, 'm>>,
    log_f32: Option<LLVMFunction<'ctx, 'm>>,
    log_f64: Option<LLVMFunction<'ctx, 'm>>,
    ctpop_i8: Option<LLVMFunction<'ctx, 'm>>,
    ctpop_i16: Option<LLVMFunction<'ctx, 'm>>,
    ctpop_i32: Option<LLVMFunction<'ctx, 'm>>,
    ctpop_i64: Option<LLVMFunction<'ctx, 'm>>,
    memcpy_i64: Option<LLVMFunction<'ctx, 'm>>,
    memmove_i64: Option<LLVMFunction<'ctx, 'm>>,
}
