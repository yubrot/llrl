FLAGS=-pllvm:../llvm1 -c../llvm1/support.c -c-I../rt -cbackend/llvm/jit_library_binding.c `llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

llrl1:
	make -C ../rt
	llrl0 $(FLAGS) -O -o llrl1 main

test:
	make -C ../rt
	llrl0 $(FLAGS) tests/all

