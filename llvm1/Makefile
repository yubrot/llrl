LLVM_EXTRAS=-csupport.c
LLVM_CONFIG=`llvm-config --cppflags --ldflags --libs --system-libs | sed -e 's/-\([^ ]*\)/-c-\1/g'` -c-lstdc++

test:
	llrl0 $(LLVM_EXTRAS) $(LLVM_CONFIG) tests/all
