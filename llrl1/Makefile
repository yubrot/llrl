FLAGS=-pllvm:../llvm1 -c../llvm1/support.c -c-I../rt `llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

all: llrl1

llrl1:
	make -C ../rt
	llrl0 $(FLAGS) -O -o llrl1 main

llrl2: llrl1
	./llrl1 $(FLAGS) -O -o llrl2 main

llrl3: llrl2
	./llrl2 $(FLAGS) -O -o llrl3 main

install: llrl1
	cp llrl1 /usr/local/bin

test: test0 test1

test0:
	make -C ../rt
	llrl0 $(FLAGS) tests/all

test1: llrl1
	./llrl1 $(FLAGS) tests/all

self-hosting: llrl2 llrl3
	diff llrl2 llrl3

clean:
	rm -f llrl1 llrl2 llrl3

