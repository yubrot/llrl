use super::codegen;
use super::runtime::*;
use crate::emitter::ir::*;
use derive_new::new;
use llvm::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug)]
pub struct ContextArtifact<'ctx> {
    context: &'ctx LLVMContext,
    data_layout: LLVMBox<LLVMDataLayout>,
    type_sizes: HashMap<CtId, Option<TypeSize>>,
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
            type_sizes: HashMap::new(),
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

    pub fn add_types(&mut self, defs: &HashMap<CtId, Arc<CtDef>>) {
        // Put type headers
        for (id, def) in defs {
            match **def {
                CtDef::Struct(_) => {
                    let _ = self.type_size(&Ct::Id(*id), &defs);
                    let ty = LLVMStructType::new(&id.index().to_string(), self.context);
                    self.structs.insert(*id, ty);
                    self.types.insert(*id, ty.as_type());
                }
                CtDef::Union(_) => {
                    let type_size = self.type_size(&Ct::Id(*id), &defs);
                    let ty = if type_size.align != 0 {
                        let bw = type_size.align * 8;
                        let len = (type_size.size / type_size.align) as usize;
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
            if let CtDef::Struct(ref def) = **def {
                // Bacause currently we always use C-compatible structures, there is no difference by def.repr
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
            Ct::String => RtString::llvm_type(self.context),
            Ct::Char => RtChar::llvm_type(self.context),
            Ct::Array(ty) => RtArray::llvm_type(self.llvm_type(ty)),
            Ct::CapturedUse => RtCapturedUse::llvm_type(self.context),
            Ct::Unit => llvm_type!(*self, (struct)).as_type(),
            Ct::Env => llvm_type!(*self, (ptr i8)).as_type(),
            Ct::Syntax(_) => RtSyntax::llvm_type(self.context),
            Ct::Hole => panic!("Found Ct::Hole at Codegen::get_type"),
        }
    }

    pub fn llvm_type_all<'a>(&self, tys: impl IntoIterator<Item = &'a Ct>) -> Vec<LLVMType<'ctx>> {
        tys.into_iter().map(|ty| self.llvm_type(ty)).collect()
    }

    fn type_size(&mut self, ty: &Ct, defs: &HashMap<CtId, Arc<CtDef>>) -> TypeSize {
        match ty {
            Ct::Id(id) => match self.type_sizes.get(id) {
                Some(None) => panic!("Unsized type: {}", id),
                Some(Some(size)) => *size,
                None => {
                    self.type_sizes.insert(*id, None);
                    let ts = match defs.get(&id).map(|def| &**def) {
                        Some(CtDef::Struct(ty)) => {
                            TypeSize::struct_(self.type_size_all(&ty.fields, defs))
                        }
                        Some(CtDef::Union(ty)) => {
                            TypeSize::union(self.type_size_all(&ty.tys, defs))
                        }
                        Some(_) => panic!("Not a type: {}", id),
                        None => panic!("Unknown type: {}", id),
                    };
                    self.type_sizes.insert(*id, Some(ts));
                    ts
                }
            },
            Ct::GenericInst(_) => panic!("Found Ct::GenericInst on TypeSizeCache::type_size"),
            Ct::TableGet(_) => panic!("Found Ct::GenericInst on TypeSizeCache::type_size"),
            Ct::Ptr(_) => TypeSize::pointer(),
            Ct::Clos(_) => {
                let ptr = TypeSize::pointer();
                TypeSize::struct_(vec![ptr, ptr])
            }
            Ct::S(s) | Ct::U(s) => {
                // TODO: To be precise, we need to follow the integer type specification of the DataLayout
                let size = ((*s + 7) / 8).next_power_of_two();
                TypeSize::new(size, size)
            }
            Ct::F32 => TypeSize::new(4, 4),
            Ct::F64 => TypeSize::new(8, 8),
            Ct::String => RtString::size_align().into(),
            Ct::Char => RtChar::size_align().into(),
            Ct::Array(_) => RtArray::size_align().into(),
            Ct::CapturedUse => RtCapturedUse::size_align().into(),
            Ct::Unit => TypeSize::new(0, 0),
            Ct::Env => TypeSize::pointer(),
            Ct::Syntax(_) => RtSyntax::size_align().into(),
            Ct::Hole => panic!("Found Ct::Hole on TypeSizeCache::type_size"),
        }
    }

    fn type_size_all(&mut self, tys: &[Ct], defs: &HashMap<CtId, Arc<CtDef>>) -> Vec<TypeSize> {
        tys.iter().map(|ty| self.type_size(ty, defs)).collect()
    }
}

impl<'ctx> LLVMTypeBuilder<'ctx> for ContextArtifact<'ctx> {
    fn context(&self) -> &'ctx LLVMContext {
        self.context()
    }
}

#[derive(Debug, Clone, Copy, new)]
struct TypeSize {
    size: usize,
    align: usize,
}

impl TypeSize {
    fn pointer() -> Self {
        Self::new(std::mem::size_of::<usize>(), std::mem::align_of::<usize>())
    }

    fn struct_(tss: Vec<Self>) -> Self {
        let align = tss.iter().map(|a| a.align).max().unwrap_or(0);
        let mut size = 0;
        for ts in tss {
            if ts.align != 0 {
                size += (ts.align - (size % ts.align)) % ts.align;
            }
            size += ts.size;
        }
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        Self::new(size, align)
    }

    fn union(tss: Vec<Self>) -> Self {
        let align = tss.iter().map(|a| a.align).max().unwrap_or(0);
        let mut size = tss.iter().map(|a| a.size).max().unwrap_or(0);
        if align != 0 {
            size += (align - (size % align)) % align;
        }
        Self::new(size, align)
    }
}

impl From<(usize, usize)> for TypeSize {
    fn from((size, align): (usize, usize)) -> Self {
        Self::new(size, align)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol<'ctx> {
    pub name: String,
    pub kind: FunctionSymbolKind,
    pub ty: LLVMFunctionType<'ctx>,
}

impl<'ctx> FunctionSymbol<'ctx> {
    pub fn new(name: String, def: &Function, ctx: &ContextArtifact<'ctx>) -> Self {
        let kind = FunctionSymbolKind::new(def);

        let param_tys = (if kind.takes_env_as_argument() {
            Some(ctx.llvm_type(&Ct::Env))
        } else {
            None
        })
        .into_iter()
        .chain(def.params.iter().map(|p| ctx.llvm_type(&p.ty)))
        .collect::<Vec<_>>();
        let ret_ty = ctx.llvm_type(&def.ret);

        let ty = if kind.returns_by_pointer_store() {
            let mut param_tys = param_tys;
            param_tys.insert(0, llvm_type!(*ctx, (ptr { ret_ty })).as_type());
            llvm_type!(*ctx, (function(...{param_tys}) void))
        } else {
            llvm_type!(*ctx, (function(...{param_tys}) {ret_ty}))
        };

        Self { name, kind, ty }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionSymbolKind {
    Standard(Vec<Ct>, Ct), // (-> _ ... _)
    Macro,                 // (-> Sexp (Result Sexp String))
    Main(Ct),              // (-> _)
}

impl FunctionSymbolKind {
    fn new(def: &Function) -> Self {
        match def.kind {
            FunctionKind::Standard | FunctionKind::Transparent => Self::Standard(
                def.params.iter().map(|p| p.ty.clone()).collect(),
                def.ret.clone(),
            ),
            FunctionKind::Macro => {
                assert!(
                    def.env.is_none(),
                    "Macro function cannot have an environment parameter"
                );
                assert!(
                    matches!(def.params.as_slice(), [_]),
                    "Macro function must take exactly one parameter"
                );
                // TODO: assert that the argument type is (Syntax Sexp)
                // TODO: assert that the return type is (Result (Syntax Sexp) btring)
                Self::Macro
            }
            FunctionKind::Main => {
                assert!(
                    def.params.is_empty() && def.env.is_none(),
                    "Main function cannot have parameters"
                );
                Self::Main(def.ret.clone())
            }
        }
    }

    pub fn takes_env_as_argument(&self) -> bool {
        match self {
            Self::Main(_) => false,
            _ => true,
        }
    }

    pub fn returns_by_pointer_store(&self) -> bool {
        // Macros are called from Rust code with C-compatible ABI, so the result of the macro functions
        // must be returned through pointers. The main function is also called from Rust in tests,
        // but the type of the return value used in tests is bool (i8), which can be returned
        // directly as C-compatible form.
        match self {
            Self::Macro => true,
            _ => false,
        }
    }
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
    library: RtLibrary<'ctx, 'm>,
    functions: HashMap<CtId, FunctionArtifact<'ctx, 'm>>,
    main_function: Option<FunctionArtifact<'ctx, 'm>>,
    c_functions: HashMap<String, CFunctionArtifact<'ctx, 'm>>,
    intrinsic_functions: IntrinsicFunctions<'ctx, 'm>,
}

impl<'ctx: 'm, 'm> ModuleArtifact<'ctx, 'm> {
    pub fn new(module: &'m LLVMModule<'ctx>) -> Self {
        Self {
            module,
            library: RtLibrary::new(module),
            functions: HashMap::new(),
            main_function: None,
            c_functions: HashMap::new(),
            intrinsic_functions: IntrinsicFunctions::default(),
        }
    }

    pub fn module(&self) -> &'m LLVMModule<'ctx> {
        self.module
    }

    pub fn library(&self) -> &RtLibrary<'ctx, 'm> {
        &self.library
    }

    pub fn function(&self, id: CtId) -> Option<&FunctionArtifact<'ctx, 'm>> {
        self.functions.get(&id)
    }

    pub fn main_function(&self) -> Option<&FunctionArtifact<'ctx, 'm>> {
        self.main_function.as_ref()
    }

    pub fn add_functions(
        &mut self,
        defs: &HashMap<CtId, Arc<CtDef>>,
        ctx: &mut ContextArtifact<'ctx>,
    ) {
        // Put symbols
        for (id, def) in defs {
            if let CtDef::Function(ref def) = **def {
                let name = id.index().to_string();
                let symbol = FunctionSymbol::new(name, def, ctx);
                assert!(ctx.function_symbol(*id).is_none());
                ctx.function_symbols.insert(*id, symbol);
            }
        }

        // Generate function bodies
        for (id, def) in defs {
            if let CtDef::Function(ref def) = **def {
                let function = self.capture_function(*id, ctx).clone();
                codegen::run(&function, def, self, &ctx);
            }
        }
    }

    pub fn add_main(&mut self, mut main: Vec<Init>, ctx: &mut ContextArtifact<'ctx>) {
        let def = {
            let ret = main
                .pop()
                .unwrap_or(Init::new(Ct::Unit, Rt::Const(Const::Unit)));
            let stmts = main.into_iter().map(|init| init.expr).collect();

            Function::new(
                None,
                Vec::new(),
                ret.ty,
                Rt::seq(stmts, ret.expr),
                FunctionKind::Main,
            )
        };

        let symbol = FunctionSymbol::new("main".to_string(), &def, ctx);
        let value = self.module.add_function("main", symbol.ty);
        let function = FunctionArtifact::new(symbol, value);
        codegen::run(&function, &def, self, &ctx);

        assert!(ctx.main_function_symbol.is_none());
        ctx.main_function_symbol = Some(function.symbol.clone());
        self.main_function = Some(function);
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
            FunctionArtifact::new(symbol, value)
        })
    }

    pub fn capture_c_function(
        &mut self,
        name: &str,
        ctx: &ContextArtifact<'ctx>,
        function_ty: impl FnOnce() -> LLVMFunctionType<'ctx>,
    ) -> CFunctionArtifact<'ctx, 'm> {
        if let Some(function) = self.c_functions.get(name) {
            return *function;
        }

        let function_ty = function_ty();

        // TODO: This adjustment is not enough. We need to follow the System V ABI.
        let (function_ty, return_by_pointer_store) = if {
            function_ty.return_type().is_sized()
                && 4 * 8 < ctx.data_layout().type_alloc_size(function_ty.return_type())
        } {
            let mut params = function_ty.param_types();
            let ret = function_ty.return_type();
            params.insert(0, llvm_type!(*ctx, (ptr { ret })).as_type());
            (llvm_type!(*ctx, (function(...{params}) void)), true)
        } else {
            (function_ty, false)
        };

        let function = self.module.add_function(name, function_ty);
        let artifact = CFunctionArtifact::new(function, return_by_pointer_store);
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

impl<'ctx: 'm, 'm> RtContext<'ctx, 'm> for ModuleArtifact<'ctx, 'm> {
    fn module(&self) -> &'m LLVMModule<'ctx> {
        self.module()
    }

    fn library(&self) -> &RtLibrary<'ctx, 'm> {
        self.library()
    }
}

#[derive(Debug, Clone, new)]
pub struct FunctionArtifact<'ctx: 'm, 'm> {
    pub symbol: FunctionSymbol<'ctx>,
    pub value: LLVMFunction<'ctx, 'm>,
}

#[derive(Debug, Clone, Copy, new)]
pub struct CFunctionArtifact<'ctx: 'm, 'm> {
    pub value: LLVMFunction<'ctx, 'm>,
    pub return_by_pointer_store: bool,
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
