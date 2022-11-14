FLAGS=-ccompat.c `llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

test: test-llrl0 test-llrl1

test-llrl0:
	llrl0 $(FLAGS) tests/all

test-llrl1: ../llrl1/llrl1
	../llrl1/llrl1 $(FLAGS) tests/all

../llrl1/llrl1:
	make -C ../llrl1 llrl1

