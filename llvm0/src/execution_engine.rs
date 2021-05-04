use super::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;
use std::ffi::CString;
use std::marker::PhantomData;
use std::os::raw::c_char;

#[derive(Debug)]
pub struct ExecutionEngine<'ctx> {
    inner: LLVMExecutionEngineRef,
    modules: Vec<LLVMModuleRef>,
    _context: PhantomData<&'ctx Context>,
}

impl<'ctx> ExecutionEngine<'ctx> {
    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn link_in_mcjit() {
        LLVMLinkInMCJIT();
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn link_in_interpreter() {
        LLVMLinkInInterpreter();
    }

    pub unsafe fn from_ptr(ptr: LLVMExecutionEngineRef) -> Self {
        Self {
            inner: ptr,
            modules: Vec::new(),
            _context: PhantomData,
        }
    }

    pub fn as_ptr(&self) -> LLVMExecutionEngineRef {
        self.inner
    }

    fn new_with(
        ctx: &'ctx Context,
        create: impl FnOnce(*mut LLVMExecutionEngineRef, LLVMModuleRef, *mut *mut c_char) -> LLVMBool,
    ) -> Result<Self, Message> {
        // Dummy module for ExecutionEngine creation. consumed by `create`.
        let dummy_module = Oo::into_ptr(Module::new("dummy", ctx.context()));

        let mut ee = std::mem::MaybeUninit::uninit();
        let mut err = std::mem::MaybeUninit::uninit();
        match create(ee.as_mut_ptr(), dummy_module, err.as_mut_ptr()) {
            0 => Ok(unsafe { Self::from_ptr(ee.assume_init()) }),
            _ => Err(unsafe { Message::from_ptr(err.assume_init()) }),
        }
    }

    pub fn new(ctx: &'ctx Context) -> Result<Self, Message> {
        Self::new_with(ctx, |out, m, err| unsafe {
            LLVMCreateExecutionEngineForModule(out, m, err)
        })
    }

    pub fn new_interpreter(ctx: &'ctx Context) -> Result<Self, Message> {
        Self::new_with(ctx, |out, m, err| unsafe {
            LLVMCreateInterpreterForModule(out, m, err)
        })
    }

    pub fn new_jit(ctx: &'ctx Context, opt_level: OptLevel) -> Result<Self, Message> {
        Self::new_with(ctx, |out, m, err| unsafe {
            LLVMCreateJITCompilerForModule(out, m, opt_level.into(), err)
        })
    }

    pub fn new_mcjit(
        ctx: &'ctx Context,
        opt_level: Option<OptLevel>,
        code_model: Option<CodeModel>,
        no_frame_pointer_elim: Option<bool>,
        enable_fast_isel: Option<bool>,
    ) -> Result<Self, Message> {
        let options_size = std::mem::size_of::<LLVMMCJITCompilerOptions>();
        let mut options = unsafe {
            let mut o = std::mem::MaybeUninit::uninit();
            LLVMInitializeMCJITCompilerOptions(o.as_mut_ptr(), options_size);
            o.assume_init()
        };
        if let Some(opt_level) = opt_level {
            options.OptLevel = opt_level.into();
        }
        if let Some(code_model) = code_model {
            options.CodeModel = code_model.into();
        }
        if let Some(a) = no_frame_pointer_elim {
            options.NoFramePointerElim = if a { 1 } else { 0 };
        }
        if let Some(a) = enable_fast_isel {
            options.EnableFastISel = if a { 1 } else { 0 };
        }
        Self::new_with(ctx, |out, m, err| unsafe {
            LLVMCreateMCJITCompilerForModule(out, m, &mut options, options_size, err)
        })
    }

    pub fn add_module(&mut self, module: Oo<Module<'ctx>>) {
        let module_ref = Oo::into_ptr(module);
        unsafe { LLVMAddModule(self.as_ptr(), module_ref) };
        self.modules.push(module_ref);
    }

    pub fn remove_modules(&mut self) -> Vec<Oo<Module<'ctx>>> {
        let modules = self
            .modules
            .iter()
            .map(|module_ref| {
                let mut out = std::mem::MaybeUninit::uninit();
                let mut e = std::mem::MaybeUninit::uninit();
                match unsafe {
                    LLVMRemoveModule(self.as_ptr(), *module_ref, out.as_mut_ptr(), e.as_mut_ptr())
                } {
                    0 => unsafe { Oo::from_ptr(out.assume_init()) },
                    _ => panic!("{}", unsafe { Message::from_ptr(e.assume_init()) }),
                }
            })
            .collect();
        self.modules.clear();
        modules
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn run_static_constructors(&self) {
        LLVMRunStaticConstructors(self.as_ptr());
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn run_static_destructors(&self) {
        LLVMRunStaticDestructors(self.as_ptr());
    }

    #[allow(clippy::missing_safety_doc)]
    pub unsafe fn run_function<'e, 'g>(
        &'e self,
        f: Function<'ctx, 'e>,
        args: impl IntoIterator<Item = &'g GenericValue>,
    ) -> Oo<GenericValue> {
        let mut args = args.into_iter().collect::<Vec<_>>();
        let result = LLVMRunFunction(
            self.as_ptr(),
            f.as_ptr(),
            args.len() as u32,
            args.as_mut_ptr() as *mut LLVMGenericValueRef,
        );
        Oo::from_ptr(result)
    }

    pub fn get_function_address<'e>(&self, name: &str) -> u64 {
        let name = CString::new(name).unwrap();
        unsafe { LLVMGetFunctionAddress(self.as_ptr(), name.as_ptr()) }
    }

    pub fn find_function<'e>(&'e self, name: &str) -> Option<Function<'ctx, 'e>> {
        let mut out = std::mem::MaybeUninit::uninit();
        let name = CString::new(name).unwrap();
        match unsafe { LLVMFindFunction(self.as_ptr(), name.as_ptr(), out.as_mut_ptr()) } {
            0 => Some(unsafe { Function::from_ptr(out.assume_init()) }),
            _ => None,
        }
    }

    pub fn data_layout(&self) -> &DataLayout {
        unsafe { DataLayout::from_ptr(LLVMGetExecutionEngineTargetData(self.as_ptr())) }
    }

    pub fn target_machine(&self) -> Option<&TargetMachine> {
        let machine = unsafe { LLVMGetExecutionEngineTargetMachine(self.as_ptr()) };
        if machine.is_null() {
            None
        } else {
            Some(unsafe { TargetMachine::from_ptr(machine) })
        }
    }
}

impl<'ctx> Drop for ExecutionEngine<'ctx> {
    fn drop(&mut self) {
        unsafe { LLVMDisposeExecutionEngine(self.inner) };
    }
}
