# llrl programming language

llrl is an experimental Lisp-like programming language.

<p align="center">
<img src="./examples/images/1.png">
</p>

- [How I created a self-hosting compiler with Rust + LLVM (Japanese)](https://zenn.dev/yubrot/articles/eaaeeab742b4a1)

## Features

llrl mainly borrows its design from OCaml, Rust, Haskell, and Scheme.

- **Statically-typed**
  - Hindley-Milner based type system
  - Supports type classes
- **Lisp-like syntax + macros**
  - Uses S-expressions to write programs
  - Macros are compiled and executed at compile-time (JIT)
- **Self-hosting AOT compiler implementation**
  - There are [Rust implementation (llrl0)](./llrl0) and [llrl implementation (llrl1)](./llrl1)
- **Multiple backends**
  - `llvm` backend (works with [The LLVM Compiler Infrastructure](https://llvm.org/))
  - `chibi` backend (works with [xten](./xten0); xten is a low-level `x86_64` library including assembler and JIT linker, made for llrl)

llrl supports several well-known high-level language features:

- Modules
- Closures
- Algebraic data types
- Pattern matching
- ...

## History, Goals, and Non-goals

llrl was originally started by learning [LLVM Kaleidoscope Tutorial](https://llvm.org/docs/tutorial/index.html) in Rust. This tutorial is great for learning LLVM frontend basics, but as [the tutorial conclusion suggests](https://llvm.org/docs/tutorial/OCamlLangImpl8.html), there are a lot of things to do to make our compiler more practical.

The first goal of llrl was **not** to create a modern, feature-rich practical programming language. Instead, llrl focuses to make a compiler [self-hosted](<https://en.wikipedia.org/wiki/Self-hosting_(compilers)>). To achieve this with [LLVM-C API](https://llvm.org/doxygen/group__LLVMC.html), we need to implement more language features like strings, pointers, etc. On the other hand, Implementing self-hosting compiler does not require a rich runtime system including garbage collections, exception handling, etc. llrl also uses [Boehm garbage collector](https://en.wikipedia.org/wiki/Boehm_garbage_collector) and does not support any exception mechanism to simplify the implementation. Error handling is mainly done with the `Result` type.
This goal has been achieved and can be tested by `make self-hosting1lll` in `llrl1/`.

After achieving self-hosting, I set my next goal to remove the LLVM dependency from llrl implementation. With this goal, I sought to gain a better understanding of how the compiler backend does its job.
Since llrl has Lisp-like macros as a language feature, I made my own assembler [xten](./xten0) (which can be used in Rust code) and used it to implement a code generator targeting x86_64. The design of the code generator implementation is based on the pattern used in [chibicc](https://github.com/rui314/chibicc) and [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf).

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
- [x] llstd: llrl standard library
  - [x] Macro helpers (`gensym`, `quasiquote`, `s/match`)
  - [x] S-expression
  - [x] Common macros (`let*`, `let1`, ...)
  - [x] Common type classes (`Default`, `Eq`, `Ord`, `Hash`, `Display`, `Conv`, ...)
  - [x] Common data types (`Bool`, Integers, Floating-point numbers, `Ptr`, `Option`, `Result`, `Char`, `String`, Tuples, ...)
  - [x] Aggregate data types
    - [x] Array
    - [x] Vector
    - [x] Ordered map (B-tree)
    - [x] Ordered set
    - [x] Hash map
    - [x] Hash set
    - [x] Persistent ordered map (Red-black tree)
    - [ ] ~Persistent ordered set~
    - [x] Persistent hash map (HAMT)
    - [ ] ~Persistent hash set~
    - [ ] ~Persistent sequence (rrb-vector)~
  - [x] Arithmetic operations
  - [x] Bit operations
  - [x] [xxHash](https://github.com/Cyan4973/xxHash)
  - [x] Iterators
  - [x] Derive macro
  - [x] I/O
    - [x] Path
    - [x] File
    - [x] Directory
  - [x] System
    - [x] Command line arguments
    - [x] Process
- [x] llrl1: llrl compiler by llrl
  - [x] LLVM-C API porting
  - [x] S-expression parser
  - [x] Syntax analysis
  - [x] Loading mechanism for a set of source codes
  - [x] Semantic analysis
  - [x] Code generation
  - [x] Driver
  - [x] Self-hosting
  - [x] chibi backend
- [x] xten0: JIT/AOT compilation tools for llrl0
  - [x] Assembler
  - [x] ELF executable producer
  - [x] JIT linker
- [x] xten1: re-implementation of xten0 for llrl
- [x] chibi backend (x86_64 targeting backend with xten)
  - [x] for llrl0
  - [x] for llrl1

## Usage

Since `llrl0` is a standalone executable, you can simply install it with `cargo install`.

```shell
$ cargo install --path llrl0 --offline
$ llrl0 --help
$ llrl0 -O examples/fibonacci-numbers

# or

$ cargo run -- -O examples/fibonacci-numbers
```

Each backend is enabled by a feature flag and is enabled by default.

- `chibi-backend` - Enables `chibi` backend, a x86_64 native backend
- `llvm-backend` - Enables `llvm` backend, a backend built on top of [LLVM](https://llvm.org/)

For example, to build llrl0 without LLVM, do the following:

```shell
$ cargo install --path llrl0 --offline \
    --no-default-features -F chibi-backend
```

### Requirements

- Linux x64
  - llrl does not take into account support for other platforms
- glibc
  - Not tested other C libraries but llrl depends a few implementation-details of glibc (check at [rt/rt.c](./rt/rt.c))
- clang
- (optional) LLVM 11
  - Enabled by default in [llrl0](./llrl0) with `llvm-backend` feature
  - I use [llvmenv](https://github.com/llvmenv/llvmenv) for building LLVM
- Boehm GC 8
  - On Arch Linux, you can install Boehm GC by simply running `pacman -S gc`

### Editor support

VSCode language support is available at [yubrot/llrl-vscode](https://github.com/yubrot/llrl-vscode).

## Language Overview

### Syntax

The syntax of llrl is basically a subset of the Scheme syntax. There are several syntax sugar that are unique to this language.

| Syntax sugar  | Desugared             | Description                                                                  |
| ------------- | --------------------- | ---------------------------------------------------------------------------- |
| `~expr`       | `(load expr)`         | Call of the [load](./std/access.llrl) function                               |
| `\expr`       | `(capture expr)`      | Capture of the use (See [Macros](#macros) section)                           |
| `(...)?`      | `(try? (...))`        | Call of the [try?](./std/boot/1-try.llrl) macro (Early returns for `Option`) |
| `(...)!`      | `(try! (...))`        | Call of the [try!](./std/boot/1-try.llrl) macro (Early returns for `Result`) |
| `expr {...}`  | `(annotate expr ...)` | Type signatures (See [Functions](#functions) section)                        |
| `a... @ b...` | `a... (b...)`         | Used to reduce nesting of S-expressions                                      |

The last syntax sugar allows nested S-expressions like `(let1 foo 123 (let1 bar 456 (+ foo bar)))` to be written as `@let1 foo 123 @let1 bar 456 (+ foo bar)`.

### Packages and Modules

Modules are identified by a string in the form of a path separated by a slash `/`.
The first part of the path points to the name of the package, and the rest of the parts correspond to the file path on the package. If the file path on the package is omitted, it is treated as equivalent to `<package-name>/prelude`.

There are several predefined package names:

- `~`: The special pakcage name that refers to the current package.
- `builtin`: a set of language built-in definitions used directly by numeric literals, etc.
- `std`: the llrl language standard library. `std/prelude` is implicitly imported in all modules (this behavior can be disabled with `(no-implicit-std)` declaration)

```llrl
(import "std/hash-map" HashMap)

; Import all matching definitions by using `_` as a prefix or postfix
(import "std/hash-map" HashMap hash-map/_)

; Import everything
(import "std/hash-map" _)

; Export works as well
(export HashMap)
```

### Functions

```llrl
(function pi
  3.1415)

(function (square x)
  (* x x))

(function (squared-length a b)
  (+ (* a a) (* b b)))

pi                   ; => 3.1415
(square 10)          ; => 100
(squared-length 3 4) ; => 25

; With type signatures
(function pi {F64}
  3.1415)

(function (square x) {(-> I32 I32)}
  (* x x))

; Generalization
(function (identity x) {(forall A) (-> A A)}
  x)

; Generalization with constraints
(function (generic-square x) {(forall A) (-> A A) (where (Mul A))}
  (* x x))
```

### Types

Primitive types are defined in [builtin.llrl](./llrl0/src/ast/builtin.llrl#L61-L81).

```llrl
unit      ; Synonym for empty tuple (:)
Bool
I8
I16
I32
I64
U8
U16
U32
U64
F32
F64
String
Char

; Tuples
(: I32 I32)
(: String Char I32)

; Functions
(-> I32)
(-> I32 I32 I32)

; Option/Result
(Option I32)
(Result (Option I32) String)
```

### Expressions

llrl does not have a `main` function, and the expressions written at the top level are executed in order.

```llrl
; Literals
123
3.14
"Hello, World!\n"
#\a
#t
#f

; Function/macro application
(sqrt 2)
(+ 12 (* 34 56) 78)

; Conditional branching
(if #f
  (println! "then")
  (println! "else))

; Sequencing
(begin
  (println! "a")
  (println! "b")
  (println! "c"))

; Local variable binding
(let ([a 12]
      [b 34])
  (+ a b))

; Local function binding
(let ([(square x) (* x x)])
  (square 3))

; Loop
(let1 i (ref 0)
  (while (< ~i 5)
    (println! "Hello")
    (set! i (+ ~i 1))))

; Tuple creation
(: 12 "hello" #\x)

; Early return
(when (< x y) (return))
```

[`std/control`](./std/control.llrl) contains general purpose macros that may be frequently used in expressions.

### Data types

```llrl
(data Answer
  answer:yes
  answer:no)

; Construction
answer:yes
answer:no

; Deconstruction
(match ans
  [answer:yes "yes"]
  [answer:no "no"])

; Constructors can have fields
(data Vec2
  (vec2: F32 F32))

(function (vec2/new x y)
  (vec2: x y))

(function (vec2/squared-length vec) {(-> Vec2 F32)}
  (match vec
    [(vec2: (let x) (let y))
      (+ (* x x) (* y y))]))

; Data types can be parameterized
(data (MyOption A)
  myopt:none
  (myopt:some A))

(function (myopt/or a b) ; inferred as {(forall A) (-> (MyOption A) (MyOption A) (MyOption A))}
  (match a
    [(myopt:some (let x))
      (myopt:some x)]
    [myopt:none
      b]))
```

`builtin` contains [`Option`](./llrl0/src/ast/builtin.llrl#L86-L89) and [`Result`](./llrl0/src/ast/builtin.llrl#L91-L94) declaration. These types are re-exported by [`std/option`](./std/option.llrl) and [`std/result`](./std/result.llrl) with utility functions and common type class instances.

### Type classes

llrl type classes are almost same as Haskell 2010 type classes + `MultiParamTypeClasses` + `FlexibleContexts` `FlexibleInstances` + `UndecidableInstances`. Orphan checks, fundeps, and associated types are not implemented.

```llrl
(class (Semigroup A)
  (function (<> x y) {(-> A A A)}))
```

Each class instance has its own name. Instances are automatically resolved when using methods of classes, but instances that are resolved automatically must exist in the current scope by import/export.

```llrl
(instance Semigroup.I32 (Semigroup I32)
  (function (<> x y)
    (+ x y)))

(instance Semigroup.String (Semigroup String)
  (function (<> x y)
    (string x y)))

(println! (<> 12 34))
(println! (<> "foo" "bar"))
```

`std` provides several type classes that express frequently appearing operations like [`Eq`](./std/eq.llrl), [`Ord`](./std/ord.llrl), [`Display`](./std/display.llrl), etc.

`std` also supports automatic derivation of frequently used instances via [`derive` macro](./std/derive.llrl).

```llrl
(derive (Default Eq Ord DebugDisplay Hash)
  value-data (Vec2 A)
    (vec2: A A))
```

### Macros

The definition of macros have the same form as functions, but the types of macros are always `(-> (Syntax Sexp) (Result (Syntax Sexp) String))`. [`(Syntax A)`](./llrl0/src/ast/builtin.llrl#L114-L116) is the internal representation type used for embedding context information, and [`Sexp`](./llrl0/src/ast/builtin.llrl#L118-L128) is the structure of S-expressions itself. This means that macros take the S-expression of the macro application (with context information) as an argument and either return the result of the macro expansion or return an expansion error. Since it is hard to deconstruct and construct S-expressions manually, there is `s/match` to deconstruct S-expressions and quoting to construct S-expressions.

For example, `lambda` syntax is defined as a macro in [std/boot/5-lambda](./std/boot/5-lambda.llrl):

```llrl
; Example use: (lambda (x y) (+ x y))
(macro (lambda s)
  (s/match s                                ; Matching with (lambda (x y) (+ x y))
    [(_ ,args ,@body)                       ; args := (x y), body := ((+ x y))
      (ok
        (let ([tmp-f (gensym)])             ; Generate a non-overlapping symbol
          `(let ([(,tmp-f ,@args) ,@body])  ; Construct a S-expression:
            ,tmp-f)))]                      ; (let ([(<tmp-f> x y) (+ x y)]) <tmp-f>)
    [_
      (err "Expected (lambda (arg ...) body ...)")]))
```

`s/match` and `quasiquote` are defined as macros in [std/boot/2-s-match](./std/boot/2-s-match.llrl), [std/boot/3-quasiquote](./std/boot/3-quasiquote.llrl).

`'`(quote), `` ` `` (quasiquote), `,` (unquote), `,@` (unquote-splicing) work just like any other Lisp, but there is a llrl-specific quoting, `\` (capture). This captures the "use" of the definition in the scope.
For example, `and` macro (defined in [std/bool](./std/bool.llrl)) uses the function `&&` in the result of the macro expansion. Thanks to the capture, this points to the intended `&&` even if `&&` does not exist in the scope of the macro caller.

```llrl
(macro (and s)
  (s/match s
    [(_)
      (ok '#t)]
    [(_ ,a ,@bs)
      (s/foldr (lambda (a b) `(,\&& ,a ,b)) a bs)]
    [_
      (err "Expected (and cond ...)")]))
```

To simplify the compilation and the JIT execution order, macros are not usable in the defined module.

llrl does not support hygienic macros.

### Preprocess

llrl pre-processes several directives on S-expressions before semantic analysis.
Directives and metavariables handled by the preprocessor are represented by identifiers starting with `$`.

```llrl
($symbol "foo" bar "baz")
;=> foobarbaz

($let ([$foo 12]
       [$bar 34 56])
  (println! $foo)
  (println! $bar))
;=> (println! 12)
;   (println! 34 56)

($let1 $foo bar (array $foo $foo))
;=> (array bar bar)

($for [$a $b] ([I8 U8] [I16 U16] [I32 U32] [I64 U64])
  (instance ($symbol SameSize. $a "." $b) (SameSize $a $b)))
;=> (instance SameSize.I8.U8 (SameSize I8 U8))
;   (instance SameSize.I16.U16 (SameSize I16 U16))
;   (instance SameSize.I32.U32 (SameSize I32 U32))
;   (instance SameSize.I64.U64 (SameSize I64 U64))

($for1 $ty [I8 I16 I32 I64]
  (instance ($symbol Signed. $ty) (Signed $ty)))
;=> (instance Signed.I8 (Signed I8))
;   (instance Signed.I16 (Signed I16))
;   (instance Signed.I32 (Signed I32))
;   (instance Signed.I64 (Signed I64))

(array ($not #t) ($not #f))
;=> (array #f #t)

(array ($when #t 12 34) ($when #f 56 78))
;=> (array 12 34)

($feature "llvm-backend")
;=> #t  ; if "llvm-backend" feature is enabled
;   #f  ; otherwise
```

### Standard library

Many functionalities are implemented and provided in the standard library.

TODO: std overview?
