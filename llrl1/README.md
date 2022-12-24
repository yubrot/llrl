# llrl1: llrl compiler in llrl

llrl1 is a re-implementation of llrl compiler in llrl.

```shell
$ cd ..
$ make -C llrl1 llrl1l
$ ./llrl1/llrl1l examples/mandelbrot
```

## Build

![](../examples/images/2.png)

Since llrl has multiple backends and is also self-hosted, builds are identified by the compiler and backend used. [llrl0](../llrl0) is required for the first llrl1 build. See [Makefile](./Makefile) for more details.

```shell
# Build llrl1 compiler with llrl0 + LLVM backend
$ make llrl1l

# Build llrl1 compiler with llrl0 + chibi backend
$ make llrl1c

# Build llrl1 compiler with llrl1c + LLVM backend
$ make llrl1cl
```

Each backend can be disabled by environment variables.

- `NO_LLVM_BACKEND` - Disables `llvm` backend
- `NO_CHIBI_BACKEND` - Disables `chibi` backend

For example, to build llrl1 without LLVM, do the following:

```shell
$ NO_LLVM_BACKEND=1 make llrl1c
```

## Run tests

As with the build, we need to specify the build to be used to run the tests. We can also check for self-hosted binary matches.

```shell
# Run llrl1 tests with llrl0 + LLVM backend
$ make test0l

# Run llrl1 tests with llrl1c + LLVM backend
$ make test1cl

# Assert that llrl1cl and llrl1cll match the executable binary
$ make self-hosting1cll
```

Note that `llrl1l` does not match its binary with `llrl1ll`, since `llrl0` (used to build `llrl1l`) is implemented in a way that does not always output the same result. This is due to parallelization, hash value computation, etc.
