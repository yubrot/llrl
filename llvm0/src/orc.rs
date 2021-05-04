//! Not tested at all!

use super::*;
use llvm_sys::orc2::*;
use std::ffi::{c_void, CStr, CString};
use std::os::raw::c_char;

pub struct OrcContext {
    _refs: LLVMOrcOpaqueThreadSafeContext,
}

impl OrcContext {
    pub fn new() -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMOrcCreateNewThreadSafeContext()) }
    }

    pub fn context(&self) -> &Context {
        unsafe { Context::from_ptr(LLVMOrcThreadSafeContextGetContext(self.as_ptr())) }
    }
}

impl_opaque_eq!(OrcContext);

impl_opaque_debug!(OrcContext);

unsafe_impl_opaque!(OrcContext(LLVMOrcOpaqueThreadSafeContext));

impl OpaqueOwn for OrcContext {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMOrcDisposeThreadSafeContext(a);
    }
}

pub struct OrcModule {
    _refs: LLVMOrcOpaqueThreadSafeModule,
}

impl OrcModule {
    pub fn new<'ctx>(module: Oo<Module<'ctx>>, tsc: &'ctx OrcContext) -> Oo<Self> {
        assert_eq!(module.context(), tsc.context());
        unsafe {
            Oo::from_ptr(LLVMOrcCreateNewThreadSafeModule(
                Oo::into_ptr(module),
                tsc.as_ptr(),
            ))
        }
    }
}

impl_opaque_eq!(OrcModule);

impl_opaque_debug!(OrcModule);

unsafe_impl_opaque!(OrcModule(LLVMOrcOpaqueThreadSafeModule));

impl OpaqueOwn for OrcModule {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMOrcDisposeThreadSafeModule(a);
    }
}

pub struct OrcJIT {
    _refs: LLVMOrcOpaqueLLJIT,
}

impl OrcJIT {
    pub fn new(builder: Option<Oo<OrcJITBuilder>>) -> Result<Oo<Self>, ErrorMessage> {
        let builder = match builder {
            Some(builder) => Oo::into_ptr(builder),
            None => std::ptr::null_mut(),
        };
        let mut out = std::mem::MaybeUninit::uninit();
        match unsafe { Error::from_ptr(LLVMOrcCreateLLJIT(out.as_mut_ptr(), builder)) } {
            None => Ok(unsafe { Oo::from_ptr(out.assume_init()) }),
            Some(error) => Err(error.into()),
        }
    }

    // TODO: ExecutionSession

    fn main_dylib(&self) -> LLVMOrcJITDylibRef {
        unsafe { LLVMOrcLLJITGetMainJITDylib(self.as_ptr()) }
    }

    pub fn triple(&self) -> &CStr {
        unsafe { CStr::from_ptr(LLVMOrcLLJITGetTripleString(self.as_ptr())) }
    }

    pub fn global_prefix(&self) -> c_char {
        unsafe { LLVMOrcLLJITGetGlobalPrefix(self.as_ptr()) }
    }

    pub fn install_process_symbol_definition_generator(&self) -> Result<(), ErrorMessage> {
        let mut generator = std::mem::MaybeUninit::uninit();
        match unsafe {
            Error::from_ptr(LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess(
                generator.as_mut_ptr(),
                self.global_prefix(),
                orc_symbol_predicate,
                std::ptr::null_mut(),
            ))
        } {
            None => {
                unsafe { LLVMOrcJITDylibAddGenerator(self.main_dylib(), generator.assume_init()) };
                Ok(())
            }
            Some(error) => Err(error.into()),
        }
    }

    // TODO: add_object_file (this also requires MemoryBuffer support)

    pub fn add_module(&self, tsm: Oo<OrcModule>) -> Result<(), (Oo<OrcModule>, ErrorMessage)> {
        let tsm = Oo::into_ptr(tsm);
        match unsafe {
            Error::from_ptr(LLVMOrcLLJITAddLLVMIRModule(
                self.as_ptr(),
                self.main_dylib(),
                tsm,
            ))
        } {
            None => Ok(()),
            Some(error) => Err((unsafe { Oo::from_ptr(tsm) }, error.into())),
        }
    }

    pub fn lookup(&self, name: &str) -> Result<u64, ErrorMessage> {
        let name = CString::new(name).unwrap();
        let mut out = std::mem::MaybeUninit::uninit();
        match unsafe {
            Error::from_ptr(LLVMOrcLLJITLookup(
                self.as_ptr(),
                out.as_mut_ptr(),
                name.as_ptr(),
            ))
        } {
            None => Ok(unsafe { out.assume_init() }),
            Some(error) => Err(error.into()),
        }
    }
}

impl_opaque_eq!(OrcJIT);

impl_opaque_debug!(OrcJIT);

unsafe_impl_opaque!(OrcJIT(LLVMOrcOpaqueLLJIT));

impl OpaqueOwn for OrcJIT {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMOrcDisposeLLJIT(a);
    }
}

pub struct OrcJITBuilder {
    _refs: LLVMOrcOpaqueLLJITBuilder,
}

impl OrcJITBuilder {
    pub fn new() -> Oo<Self> {
        unsafe { Oo::from_ptr(LLVMOrcCreateLLJITBuilder()) }
    }

    pub fn set_target_machine_builder(&self, tmb: Oo<OrcJITTargetMachineBuilder>) {
        unsafe { LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(self.as_ptr(), Oo::into_ptr(tmb)) }
    }
}

impl_opaque_eq!(OrcJITBuilder);

impl_opaque_debug!(OrcJITBuilder);

unsafe_impl_opaque!(OrcJITBuilder(LLVMOrcOpaqueLLJITBuilder));

impl OpaqueOwn for OrcJITBuilder {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMOrcDisposeLLJITBuilder(a);
    }
}

pub struct OrcJITTargetMachineBuilder {
    _refs: LLVMOrcOpaqueJITTargetMachineBuilder,
}

impl OrcJITTargetMachineBuilder {
    pub fn detect_host() -> Result<Oo<Self>, ErrorMessage> {
        let mut out = std::mem::MaybeUninit::uninit();
        match unsafe { Error::from_ptr(LLVMOrcJITTargetMachineBuilderDetectHost(out.as_mut_ptr())) }
        {
            None => Ok(unsafe { Oo::from_ptr(out.assume_init()) }),
            Some(error) => Err(error.into()),
        }
    }

    pub fn from_target_machine(tm: Oo<TargetMachine>) -> Oo<Self> {
        unsafe {
            Oo::from_ptr(LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine(
                Oo::into_ptr(tm),
            ))
        }
    }
}

impl_opaque_eq!(OrcJITTargetMachineBuilder);

impl_opaque_debug!(OrcJITTargetMachineBuilder);

unsafe_impl_opaque!(OrcJITTargetMachineBuilder(
    LLVMOrcOpaqueJITTargetMachineBuilder
));

impl OpaqueOwn for OrcJITTargetMachineBuilder {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMOrcDisposeJITTargetMachineBuilder(a);
    }
}

extern "C" fn orc_symbol_predicate(_: LLVMOrcSymbolStringPoolEntryRef, _: *mut c_void) -> i32 {
    1
}
