use super::{runtime::*, FunctionSymbol, FunctionSymbolKind, Options};
use crate::emitter::{self, ir::*};
use crate::string;
use llvm::prelude::*;
use std::fmt;
use std::sync::Once;

#[derive(Debug)]
pub struct Executor<'ctx> {
    engine: LLVMExecutionEngine<'ctx>,
}

impl<'ctx> Executor<'ctx> {
    pub fn new(context: &'ctx LLVMContext, options: Options) -> Self {
        static INIT_EXECUTION_CONTEXT: Once = Once::new();

        INIT_EXECUTION_CONTEXT.call_once(|| unsafe {
            // NOTE: Only native targets are supported. For more details, see
            // assert_data_layout_matches_native_environment
            llvm::initialize_native_target().expect("Failed to initialize native target");
            llvm::initialize_native_asm_printer().expect("Failed to initialize native ASM printer");
            LLVMExecutionEngine::link_in_mcjit();
        });

        let engine = LLVMExecutionEngine::new_mcjit(
            context,
            options.opt_level,
            options.code_model,
            None,
            None,
        )
        .unwrap_or_else(|e| panic!("Failed to create execution engine: {}", e));

        assert_data_layout_matches_native_environment(engine.data_layout());

        Self { engine }
    }

    pub fn data_layout(&self) -> &LLVMDataLayout {
        self.engine.data_layout()
    }

    pub fn target_machine(&self) -> &LLVMTargetMachine {
        self.engine
            .target_machine()
            .expect("Failed to get TargetMachine")
    }

    pub fn add_module(&mut self, module: LLVMBox<LLVMModule<'ctx>>) {
        self.engine.add_module(module);
    }

    pub fn remove_modules(&mut self) -> Vec<LLVMBox<LLVMModule<'ctx>>> {
        self.engine.remove_modules()
    }

    pub fn call(&self, f: &FunctionSymbol, args: Vec<Value>) -> Result<Value, String> {
        // TODO: Can we use utilities like libffi to support a wider variety of functions?
        let matcher = match f.kind {
            FunctionSymbolKind::Standard(_, _) => Err("Unsupported".to_string())?,
            FunctionSymbolKind::Macro => {
                executor_fun::<(Env, Syntax<Sexp>), Result<Syntax<Sexp>, String>>
            }
            FunctionSymbolKind::Main(Ct::U(1)) => executor_fun::<(i32, Argv), bool>,
            FunctionSymbolKind::Main(_) => Err("Unsupported".to_string())?,
        };
        let address = self.engine.get_function_address(&f.name) as usize;
        assert!(address != 0, "Function not found: {}", f.name);
        let handler = unsafe { matcher(address as _) };
        handler(args)
    }
}

pub fn assert_data_layout_matches_native_environment(dl: &LLVMDataLayout) {
    // NOTE: llrl assumes that the native environment (i.e., at the time of executing
    // the JIT function for macro expansion) and the target environment are the same.
    // The entire implementation of the LLVM backend is based on this assumption, so it is
    // likely that major changes will be required if we wish to support cross-compilation.
    use std::mem::{align_of, size_of};
    let ctx = LLVMContext::new();

    let ty = llvm_type!(&ctx, i1);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<bool>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<bool>());

    let ty = llvm_type!(&ctx, i8);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i8>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i8>());

    let ty = llvm_type!(&ctx, i16);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i16>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i16>());

    let ty = llvm_type!(&ctx, i32);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i32>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i32>());

    let ty = llvm_type!(&ctx, i64);
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<i64>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<i64>());

    let ty = llvm_type!(&ctx, (ptr i8));
    assert_eq!(dl.type_alloc_size(ty) as usize, size_of::<*const i8>());
    assert_eq!(dl.abi_type_alignment(ty) as usize, align_of::<*const i8>());
    assert_eq!(dl.pointer_size(None) as usize, size_of::<*const i8>());
}

#[derive(Debug)]
pub enum Value {
    I(i64),
    SyntaxSexp(Syntax<Sexp>),
    String(String),
    Result(Result<Box<Value>, Box<Value>>),
    Null,
    EmptyArgv,
    Unknown,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I(value) => write!(f, "{}", value),
            Self::SyntaxSexp(s) => write!(f, "'{}", s),
            Self::String(s) => write!(f, "\"{}\"", string::escape(s)),
            Self::Result(Ok(value)) => write!(f, "(ok {})", value),
            Self::Result(Err(value)) => write!(f, "(err {})", value),
            Self::Null => write!(f, "<null>"),
            Self::EmptyArgv => write!(f, "<empty-argv>"),
            Self::Unknown => write!(f, "<unknown>"),
        }
    }
}

impl emitter::BackendValue for Value {
    fn as_bool(&self) -> Option<bool> {
        match self {
            Self::I(v) => Some(*v != 0),
            _ => None,
        }
    }

    fn from_macro_src(sexp: &Syntax<Sexp>) -> Self {
        Self::SyntaxSexp(sexp.clone())
    }

    fn into_macro_dest(mut self) -> Result<Syntax<Sexp>, String> {
        match self {
            Value::Result(Ok(ref mut value)) => match **value {
                Value::SyntaxSexp(ref mut s) => {
                    return Ok(std::mem::replace(s, Syntax::<Sexp>::dummy()))
                }
                _ => {}
            },
            Value::Result(Err(ref mut value)) => match **value {
                Value::String(ref mut s) => return Err(std::mem::take(s)),
                _ => {}
            },
            _ => {}
        }
        Err(format!(
            "Cannot treat {} as Result<Syntax<Sexp>, String>",
            self
        ))
    }
}

/// Gives bindings for the specified function pointer.
///
/// # Safety
/// The caller must ensure that the pointer points to a function and conforms to the specified type.
unsafe fn executor_fun<Args, Ret>(f: *const ()) -> Box<dyn Fn(Vec<Value>) -> Result<Value, String>>
where
    Args: ExecutorFunArgs<Ret>,
    Ret: ExecutorValue,
{
    Box::new(move |args| Args::call(f, args))
}

trait ExecutorFunArgs<Ret> {
    unsafe fn call(f: *const (), args: Vec<Value>) -> Result<Value, String>;
}

macro_rules! impl_executor_fun_args {
    ($l:literal, $( $name:ident: $tp:ident, )*) => {
        impl<Ret: ExecutorValue, $( $tp: ExecutorValue ),*> ExecutorFunArgs<Ret> for ($( $tp, )*) {
            unsafe fn call(f: *const (), mut args: Vec<Value>) -> Result<Value, String> {
                let f = std::mem::transmute::<_, extern "C" fn($( $tp::Rt, )*) -> Ret::Rt>(f);

                if args.len() != $l {
                    Err(format!("Expected {} arguments but got {} arguments", $l, args.len()))?;
                }
                args.reverse();
                $( let $name = $tp::into_rt(args.pop().unwrap())?; )*
                let ret = f($( $name, )*);
                Ok(Ret::from_rt(ret))
            }
        }
    };
}

impl_executor_fun_args!(0,);
impl_executor_fun_args!(1, a: A,);
impl_executor_fun_args!(2, a: A, b: B,);
impl_executor_fun_args!(3, a: A, b: B, c: C,);
impl_executor_fun_args!(4, a: A, b: B, c: C, d: D,);
impl_executor_fun_args!(5, a: A, b: B, c: C, d: D, e: E,);

trait ExecutorValue {
    type Rt;

    fn into_rt(value: Value) -> Result<Self::Rt, String>;

    fn from_rt(rt: Self::Rt) -> Value;
}

impl ExecutorValue for bool {
    type Rt = bool;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::I(n) => Ok(if n == 0 { false } else { true }),
            v => Err(format!("Cannot treat {} as bool", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        Value::I(if rt { 1 } else { 0 })
    }
}

impl ExecutorValue for i32 {
    type Rt = i32;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::I(n) => Ok(n as i32),
            v => Err(format!("Cannot treat {} as i32", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        Value::I(rt as i64)
    }
}

#[derive(Debug, Clone, Copy)]
struct Argv;

const EMPTY_ARGV: &[*const u8] = &[std::ptr::null()];

impl ExecutorValue for Argv {
    type Rt = *const *const u8;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::EmptyArgv => Ok(EMPTY_ARGV.as_ptr()),
            v => Err(format!("Cannot treat {} as argv", v)),
        }
    }

    fn from_rt(_: Self::Rt) -> Value {
        Value::Unknown
    }
}

#[derive(Debug, Clone, Copy)]
struct Env;

impl ExecutorValue for Env {
    type Rt = *const u8;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::Null => Ok(std::ptr::null_mut()),
            v => Err(format!("Cannot treat {} as env", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        if rt.is_null() {
            Value::Null
        } else {
            Value::Unknown
        }
    }
}

impl ExecutorValue for Syntax<Sexp> {
    type Rt = EeSyntax<EeSexp>;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::SyntaxSexp(syntax_sexp) => Ok(Self::Rt::from_host(syntax_sexp)),
            v => Err(format!("Cannot treat {} as Syntax<Sexp>", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        Value::SyntaxSexp(rt.into_host())
    }
}

impl ExecutorValue for String {
    type Rt = EeString;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::String(s) => Ok(Self::Rt::from_host(s)),
            v => Err(format!("Cannot treat {} as String", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        Value::String(rt.into_host())
    }
}

impl<T: ExecutorValue, E: ExecutorValue> ExecutorValue for Result<T, E> {
    type Rt = EeResult<T::Rt, E::Rt>;

    fn into_rt(value: Value) -> Result<Self::Rt, String> {
        match value {
            Value::Result(Ok(t)) => Ok(EeResult::ok(T::into_rt(*t)?)),
            Value::Result(Err(e)) => Ok(EeResult::err(E::into_rt(*e)?)),
            v => Err(format!("Cannot treat {} as Result<_, _>", v)),
        }
    }

    fn from_rt(rt: Self::Rt) -> Value {
        match rt.into_inner() {
            Ok(t) => Value::Result(Ok(Box::new(T::from_rt(t)))),
            Err(e) => Value::Result(Err(Box::new(E::from_rt(e)))),
        }
    }
}
