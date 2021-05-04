# llrl: Lisp-like programming language powered by Rust + LLVM

llrl is an experimental/toy **L**isp-**l**ike programming language powered by **R**ust and **L**LVM.

## Features

llrl is mainly influenced by OCaml, Rust, Haskell, and Scheme.

- **Statically-typed**
  - Hindley-Milner based type system
  - Supports type classes
- **Lisp-like syntax + macros**
  - Uses S-expressions to write programs
  - Macros are compiled and executed by LLVM JIT
- Modules
- Closures
- Algebraic data types
- Pattern matching
- ...

## Goals and Non-goals

llrl was originally started by learning [LLVM Kaleidoscope Tutorial](https://llvm.org/docs/tutorial/index.html) in Rust. This tutorial is great for learning LLVM frontend basics, but as [the tutorial conclusion suggests](https://llvm.org/docs/tutorial/OCamlLangImpl8.html), there are a lot of things to do to make our compiler more practical.

The goal of llrl is not to create a modern, practical programming language processor. Instead, llrl focuses to make a compiler [self-hosted](<https://en.wikipedia.org/wiki/Self-hosting_(compilers)>). To achieve this with [LLVM-C API](https://llvm.org/doxygen/group__LLVMC.html), we need to implement more language features like strings, pointers, etc. On the other hand, Implementing self-hosting compiler does not require a rich runtime system including garbage collections, exception handling, etc.

### Roadmap

- [x] Language Design
- [x] llrl0: llrl compiler by Rust
  - [x] S-expression parser
  - [x] Syntax analysis
  - [x] Loading mechanism for a set of source codes
  - [x] Semantic analysis
    - [x] Import/export resolution
    - [x] Design and construction of ASTs
    - [x] Name resolution
    - [x] Kind and type inference and unification algorithm
    - [x] Pattern matching analysis
  - [x] Code generation
    - [x] Monomorphization
    - [x] Closure conversion
    - [x] Pattern matching expansion
    - [x] A very simple heap2stack
    - [x] LLVM backend
    - [x] Macro expansion
  - [x] Driver
- [ ] std: llrl standard library
  - [x] Macro helpers (`gensym`, `quasiquote`, `s/match`)
  - [x] S-expression
  - [x] Common macros (`let*`, `let1`, ...)
  - [x] Common type classes (`Default`, `Eq`, `Ord`, `Hash`, `Display`, `Cast`, ...)
  - [x] Common data types (`Bool`, Integers, Floating-point numbers, `Ptr`, `Option`, `Result`, `Char`, `String`, Tuples, ...)
  - [ ] Aggregate data types
    - [x] Array
    - [x] Vector
    - [x] Ordered map (B-tree)
    - [ ] Ordered set
    - [x] Hash map
    - [ ] Hash set
    - [x] Persistent ordered map (Red-black tree)
    - [ ] Persistent ordered set
    - [ ] Persistent hash map (HAMT)
    - [ ] Persistent hash set
    - [ ] Persistent sequence (rrb-vector)
  - [x] Arithmetic operations
  - [x] Bit operations
  - [x] [xxHash](https://github.com/Cyan4973/xxHash)
  - [x] Iterators
  - [ ] Derive macro
  - [ ] System
    - [x] I/O
    - [x] Path
    - [x] File
    - [ ] Directory
    - [ ] Command line arguments
    - [ ] Process
- [ ] llrl1: llrl compiler by llrl
  - [ ] LLVM-C API porting
  - [ ] S-expression parser
  - [ ] Syntax analysis
  - [ ] Loading mechanism for a set of source codes
  - [ ] Semantic analysis
  - [ ] Code generation
  - [ ] Driver

## Usage

TODO: add more description

```shell
cargo run -- examples/fibonacci-numbers
```

### Requirements

- Linux AMD64
  - llrl does not take into account support for other platforms
- glibc
  - Not tested other C libraries but llrl depends a few implementation-details of glibc (plase see [rt/rt.c](./rt/rt.c))
- clang
- LLVM 11.0
  - I use [llvmenv](https://github.com/llvmenv/llvmenv) for building LLVM
- Boehm GC 8.0
  - On Arch Linux, you can install Boehm GC by simply running `pacman -S gc`
