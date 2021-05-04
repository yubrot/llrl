use super::*;
use llvm_sys::target_machine::*;
use std::borrow::Cow;
use std::ffi::{CStr, CString};
use std::fmt;

pub fn get_default_target_triple() -> Message {
    unsafe { Message::from_ptr(LLVMGetDefaultTargetTriple()) }
}

pub fn all_targets() -> Vec<&'static Target> {
    let mut r = Vec::new();
    let mut t = unsafe { LLVMGetFirstTarget() };
    while !t.is_null() {
        r.push(unsafe { Target::from_ptr(t) });
        t = unsafe { LLVMGetNextTarget(t) };
    }
    r
}

pub struct Target {
    _refs: LLVMTarget,
}

impl Target {
    pub fn from_name(name: &str) -> Option<&'static Self> {
        let name = CString::new(name).unwrap();
        let target_ref = unsafe { LLVMGetTargetFromName(name.as_ptr()) };
        if target_ref.is_null() {
            None
        } else {
            Some(unsafe { Target::from_ptr(target_ref) })
        }
    }

    pub fn from_triple(triple: &CStr) -> Result<&'static Self, Message> {
        let mut target_ref = std::mem::MaybeUninit::uninit();
        let mut error_message = std::mem::MaybeUninit::uninit();
        match unsafe {
            LLVMGetTargetFromTriple(
                triple.as_ptr(),
                target_ref.as_mut_ptr(),
                error_message.as_mut_ptr(),
            )
        } {
            0 => Ok(unsafe { Target::from_ptr(target_ref.assume_init()) }),
            _ => Err(unsafe { Message::from_ptr(error_message.assume_init()) }),
        }
    }

    pub fn name(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetTargetName(self.as_ptr())) }.to_string_lossy()
    }

    pub fn description(&self) -> Cow<str> {
        unsafe { CStr::from_ptr(LLVMGetTargetDescription(self.as_ptr())) }.to_string_lossy()
    }

    pub fn has_jit(&self) -> bool {
        (unsafe { LLVMTargetHasJIT(self.as_ptr()) }) != 0
    }

    pub fn has_target_machine(&self) -> bool {
        (unsafe { LLVMTargetHasTargetMachine(self.as_ptr()) }) != 0
    }

    pub fn has_asm_backend(&self) -> bool {
        (unsafe { LLVMTargetHasAsmBackend(self.as_ptr()) }) != 0
    }

    pub fn create_target_machine(
        &self,
        target_triple: &CStr,
        cpu: Option<&str>,
        features: Option<&str>,
        opt_level: Option<OptLevel>,
        reloc_mode: Option<RelocMode>,
        code_model: Option<CodeModel>,
    ) -> Option<Oo<TargetMachine>> {
        let cpu = CString::new(cpu.unwrap_or("")).unwrap();
        let features = CString::new(features.unwrap_or("")).unwrap();
        let machine = unsafe {
            LLVMCreateTargetMachine(
                self.as_ptr(),
                target_triple.as_ptr(),
                cpu.as_ptr(),
                features.as_ptr(),
                opt_level.unwrap_or_default().into(),
                reloc_mode.unwrap_or_default().into(),
                code_model.unwrap_or_default().into(),
            )
        };
        if machine.is_null() {
            None
        } else {
            Some(unsafe { Oo::from_ptr(machine) })
        }
    }
}

impl_opaque_eq!(Target);

impl fmt::Debug for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Target::from_name({:?})", self.name())
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.name(), f)
    }
}

unsafe_impl_opaque!(Target(LLVMTarget));

// Because targets are tracked by LLVM TargetRegistry,
// they are never instantiated as `OO<Target>` on Rust layer.
