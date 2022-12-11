FLAGS=-pxten:../xten1 -pllvm:../llvm1 -c../llvm1/compat.c -c-I../rt `llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

all: llrl1l

install: llrl1l
	cp llrl1l /usr/local/bin/llrl1

# The name of the llrl1 compiler executable consists of:
# * `llrl1`
# * The backend id used to build it (`l` for LLVM, `c` for chibi), repeated by the number of generations
# For example, `llrl1cl` is built by the LLVM backend of `llrl1c`, where
# `llrl1c` is built by the chibi backend of `llrl0`.

.PHONY: test self-hosting clean

test: test0l test0c test1ll test1cl test1lc test1cc self-hosting

self-hosting: self-hosting1lll self-hosting1lcc self-hosting1cll self-hosting1ccc

clean:
	rm -f llrl1*

# 1st gen
llrl1l: ../rt/libllrt.a
	llrl0     -b llvm  -o llrl1l   $(FLAGS) -O main
llrl1c: ../rt/libllrt.a
	llrl0     -b chibi -o llrl1c   $(FLAGS) -O main

# 2nd gen
llrl1ll: llrl1l ../rt/libllrt.a
	./llrl1l  -b llvm  -o llrl1ll  $(FLAGS) -O main
llrl1lc: llrl1l ../rt/libllrt.a
	./llrl1l  -b chibi -o llrl1lc  $(FLAGS) -O main
llrl1cl: llrl1c ../rt/libllrt.a
	./llrl1c  -b llvm  -o llrl1cl  $(FLAGS) -O main
llrl1cc: llrl1c ../rt/libllrt.a
	./llrl1c  -b chibi -o llrl1cc  $(FLAGS) -O main

# 3rd gen
llrl1lll: llrl1ll ../rt/libllrt.a
	./llrl1ll -b llvm  -o llrl1lll $(FLAGS) -O main
llrl1llc: llrl1ll ../rt/libllrt.a
	./llrl1ll -b chibi -o llrl1llc $(FLAGS) -O main
llrl1lcl: llrl1lc ../rt/libllrt.a
	./llrl1lc -b llvm  -o llrl1lcl $(FLAGS) -O main
llrl1lcc: llrl1lj ../rt/libllrt.a
	./llrl1lc -b chibi -o llrl1lcc $(FLAGS) -O main
llrl1cll: llrl1cl ../rt/libllrt.a
	./llrl1cl -b llvm  -o llrl1cll $(FLAGS) -O main
llrl1clc: llrl1cl ../rt/libllrt.a
	./llrl1cl -b chibi -o llrl1clc $(FLAGS) -O main
llrl1ccl: llrl1cc ../rt/libllrt.a
	./llrl1cc -b llvm  -o llrl1ccl $(FLAGS) -O main
llrl1ccc: llrl1cc ../rt/libllrt.a
	./llrl1cc -b chibi -o llrl1ccc $(FLAGS) -O main

../rt/libllrt.a:
	make -C ../rt

# 0th gen test
.PHONY: test0l test0c
test0l:
	llrl0     -b llvm  $(FLAGS) tests/all
test0c:
	llrl0     -b chibi $(FLAGS) tests/all

# 1st gen test
.PHONY: test1ll test1lc test1cl test1cc
test1ll: llrl1l
	./llrl1l  -b llvm  $(FLAGS) tests/all
test1lc: llrl1l
	./llrl1l  -b chibi $(FLAGS) tests/all
test1cl: llrl1c
	./llrl1c  -b llvm  $(FLAGS) tests/all
test1cc: llrl1c
	./llrl1c  -b chibi $(FLAGS) tests/all

# 2nd gen test
.PHONY: test1lll test1llc test1lcl test1lcc test1cll test1clc test1ccl test1ccc
test1lll: llrl1ll
	./llrl1ll -b llvm  $(FLAGS) tests/all
test1llc: llrl1ll
	./llrl1ll -b chibi $(FLAGS) tests/all
test1lcl: llrl1lc
	./llrl1lc -b llvm  $(FLAGS) tests/all
test1lcc: llrl1lc
	./llrl1lc -b chibi $(FLAGS) tests/all
test1cll: llrl1cl
	./llrl1cl -b llvm  $(FLAGS) tests/all
test1clc: llrl1cl
	./llrl1cl -b chibi $(FLAGS) tests/all
test1ccl: llrl1cc
	./llrl1cc -b llvm  $(FLAGS) tests/all
test1ccc: llrl1cc
	./llrl1cc -b chibi $(FLAGS) tests/all

# 3rd gen self-hosting test (llrl1 2nd and 3rd gen match the executable binary)
.PHONY: self-hosting1lll self-hosting1lcc self-hosting1cll self-hosting1ccc
self-hosting1lll: llrl1ll llrl1lll
	diff llrl1ll llrl1lll
self-hosting1lcc: llrl1lc llrl1lcc
	diff llrl1lc llrl1lcc
self-hosting1cll: llrl1cl llrl1cll
	diff llrl1cl llrl1cll
self-hosting1ccc: llrl1cc llrl1ccc
	diff llrl1cc llrl1ccc
