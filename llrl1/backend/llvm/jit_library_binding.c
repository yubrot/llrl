#include <llvm-c/Support.h>
#include <rt.h>

void llrt_initialize_llvm_jit_library_binding() {
  LLVMAddSymbol("llrt_args", llrt_args);
  LLVMAddSymbol("llrt_panic", llrt_panic);
  LLVMAddSymbol("llrt_exit", llrt_exit);
  LLVMAddSymbol("llrt_spawn_process", llrt_spawn_process);
  LLVMAddSymbol("llrt_execute_process", llrt_execute_process);
  LLVMAddSymbol("llrt_wait", llrt_wait);
  LLVMAddSymbol("llrt_time", llrt_time);
  LLVMAddSymbol("llrt_getcwd", llrt_getcwd);
  LLVMAddSymbol("llrt_string_genid", llrt_string_genid);
  LLVMAddSymbol("llrt_string_eq", llrt_string_eq);
  LLVMAddSymbol("llrt_string_cmp", llrt_string_cmp);
  LLVMAddSymbol("llrt_string_concat", llrt_string_concat);
  LLVMAddSymbol("llrt_f32_to_string", llrt_f32_to_string);
  LLVMAddSymbol("llrt_f64_to_string", llrt_f64_to_string);
  LLVMAddSymbol("llrt_i64_to_string", llrt_i64_to_string);
  LLVMAddSymbol("llrt_u64_to_string", llrt_u64_to_string);
  LLVMAddSymbol("llrt_string_to_i64", llrt_string_to_i64);
  LLVMAddSymbol("llrt_string_to_u64", llrt_string_to_u64);
  LLVMAddSymbol("llrt_string_to_f32", llrt_string_to_f32);
  LLVMAddSymbol("llrt_string_to_f64", llrt_string_to_f64);
  LLVMAddSymbol("llrt_readdir", llrt_readdir);
  LLVMAddSymbol("llrt_stdin", llrt_stdin);
  LLVMAddSymbol("llrt_stdout", llrt_stdout);
  LLVMAddSymbol("llrt_stderr", llrt_stderr);
  LLVMAddSymbol("llrt_current_errno", llrt_current_errno);
  LLVMAddSymbol("llrt_xxh_seed", llrt_xxh_seed);
}
