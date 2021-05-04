use super::*;
use llvm_sys::target_machine::*;
use std::ffi::CString;

pub struct TargetMachine {
    _refs: LLVMOpaqueTargetMachine,
}

impl TargetMachine {
    pub fn target(&self) -> &'static Target {
        unsafe { Target::from_ptr(LLVMGetTargetMachineTarget(self.as_ptr())) }
    }

    pub fn target_triple(&self) -> Message {
        unsafe { Message::from_ptr(LLVMGetTargetMachineTriple(self.as_ptr())) }
    }

    pub fn target_cpu(&self) -> Message {
        unsafe { Message::from_ptr(LLVMGetTargetMachineCPU(self.as_ptr())) }
    }

    pub fn target_feature(&self) -> Message {
        unsafe { Message::from_ptr(LLVMGetTargetMachineFeatureString(self.as_ptr())) }
    }

    pub fn create_data_layout(&self) -> Oo<DataLayout> {
        unsafe { Oo::from_ptr(LLVMCreateTargetDataLayout(self.as_ptr())) }
    }

    pub fn set_asm_verbosity(&self, verbose: bool) {
        unsafe { LLVMSetTargetMachineAsmVerbosity(self.as_ptr(), if verbose { 1 } else { 0 }) };
    }

    pub fn emit_to_file(
        &self,
        module: &Module,
        filename: &str,
        filetype: FileType,
    ) -> Result<(), Message> {
        let filename = CString::new(filename).unwrap();
        let mut error_message = std::mem::MaybeUninit::uninit();
        match unsafe {
            LLVMTargetMachineEmitToFile(
                self.as_ptr(),
                module.as_ptr(),
                filename.as_ptr() as *mut i8,
                filetype.into(),
                error_message.as_mut_ptr(),
            )
        } {
            0 => Ok(()),
            _ => Err(unsafe { Message::from_ptr(error_message.assume_init()) }),
        }
    }
}

impl_opaque_eq!(TargetMachine);

impl_opaque_debug!(TargetMachine);

unsafe_impl_opaque!(TargetMachine(LLVMOpaqueTargetMachine));

impl OpaqueOwn for TargetMachine {
    unsafe fn drop(a: *mut Self::Type) {
        LLVMDisposeTargetMachine(a);
    }
}
