FLAGS=-ccompat.c `llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

test: test-llrl0-llvm test-llrl0-chibi test-llrl1

test-llrl0-llvm:
	llrl0 -b llvm $(FLAGS) tests/all

test-llrl0-chibi:
	llrl0 -b chibi $(FLAGS) tests/all

test-llrl1: ../llrl1/llrl1
	../llrl1/llrl1 $(FLAGS) tests/all

../llrl1/llrl1:
	make -C ../llrl1 llrl1
