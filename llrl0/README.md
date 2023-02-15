# llrl0: llrl compiler in Rust

In this repository, each directory with `0` as a suffix in its name is a component of Rust's compiler implementation. This directory is the primary implementation of the llrl compiler in Rust.

## Caveats

llrl0 does not use threads in the entire implementation. Based on this assumption, this implementation uses [`LLLVMContext::global`](../llvm0/src/context.rs) and Boehm GC's `GC_malloc`.
