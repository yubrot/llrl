FLAGS=-c-ldl

.PHONY: test test0l test0c test1ll test1lc test1cl test1cc

# TODO: Implement chibi backend in llrl1
test: test0l test0c test1ll test1cl

test0l:
	llrl0           -b llvm  $(FLAGS) tests/all
test0c:
	llrl0           -b chibi $(FLAGS) tests/all
test1ll: ../llrl1/llrl1l
	../llrl1/llrl1l -b llvm  $(FLAGS) tests/all
test1lc: ../llrl1/llrl1l
	../llrl1/llrl1l -b chibi $(FLAGS) tests/all
test1cl: ../llrl1/llrl1c
	../llrl1/llrl1c -b llvm  $(FLAGS) tests/all
test1cc: ../llrl1/llrl1c
	../llrl1/llrl1c -b chibi $(FLAGS) tests/all

../llrl1/llrl1l:
	make -C ../llrl1 llrl1l
../llrl1/llrl1c:
	make -C ../llrl1 llrl1c
