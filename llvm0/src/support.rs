#![allow(clippy::missing_safety_doc)]

use llvm_sys::prelude::*;
use llvm_sys::support::*;
use llvm_sys::target::*;
use std::ffi::{c_void, CString};

type InitializeResult = Result<(), ()>;

fn to_initialize_result(result: LLVMBool) -> InitializeResult {
    match result {
        1 => Err(()),
        _ => Ok(()),
    }
}

pub unsafe fn initialize_native_target() -> InitializeResult {
    to_initialize_result(LLVM_InitializeNativeTarget())
}

pub unsafe fn initialize_native_asm_parser() -> InitializeResult {
    to_initialize_result(LLVM_InitializeNativeAsmParser())
}

pub unsafe fn initialize_native_asm_printer() -> InitializeResult {
    to_initialize_result(LLVM_InitializeNativeAsmPrinter())
}

pub unsafe fn initialize_native_disassembler() -> InitializeResult {
    to_initialize_result(LLVM_InitializeNativeDisassembler())
}

pub unsafe fn initialize_all_targets() {
    LLVM_InitializeAllTargets()
}

pub unsafe fn initialize_all_asm_parsers() {
    LLVM_InitializeAllAsmParsers()
}

pub unsafe fn initialize_all_asm_printers() {
    LLVM_InitializeAllAsmPrinters()
}

pub unsafe fn initialize_all_disassemblers() {
    LLVM_InitializeAllDisassemblers()
}

pub unsafe fn load_library_permanently(filename: &str) {
    let filename = CString::new(filename).unwrap();
    LLVMLoadLibraryPermanently(filename.as_ptr());
}

pub unsafe fn add_symbol<T>(name: &str, value: *mut T) {
    let name = CString::new(name).unwrap();
    LLVMAddSymbol(name.as_ptr(), value as *mut c_void);
}

pub unsafe fn search_for_address_of_symbol<T>(name: &str) -> *mut T {
    let name = CString::new(name).unwrap();
    LLVMSearchForAddressOfSymbol(name.as_ptr()) as *mut T
}
