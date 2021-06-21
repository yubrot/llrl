#include <llvm-c/Target.h>

LLVMBool llrl_LLVMInitializeNativeTarget() {
  return LLVMInitializeNativeTarget();
}

LLVMBool llrl_LLVMInitializeNativeAsmPrinter() {
  return LLVMInitializeNativeAsmPrinter();
}

LLVMBool llrl_LLVMInitializeNativeAsmParser() {
  return LLVMInitializeNativeAsmParser();
}

LLVMBool llrl_LLVMInitializeNativeDisassembler() {
  return LLVMInitializeNativeDisassembler();
}
