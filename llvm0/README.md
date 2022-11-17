# llvm0: High-level LLVM API for Rust

- [API design description (Japanese)](https://zenn.dev/yubrot/articles/37b6724e41fd3c)

This crate provides a higher-level abstract interface to [llvm-sys](https://crates.io/crates/llvm-sys).

Although there are already crates that provide such a higher-level abstract interface to LLVM, such as [llvm-ir](https://crates.io/crates/llvm-ir), `llvm0` is implemented on its own with minimal abstraction, considering that [it will be reimplemented in llrl](../llvm1) itself during self-hosting.
