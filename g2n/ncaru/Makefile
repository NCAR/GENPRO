
include make.inc

all : libncaru testsncaru utilsncaru

libncaru : 
	mkdir -p build;cd build ; $(MAKE) -f ../src/Makefile clean ; $(MAKE) -f ../src/Makefile

testsncaru :
	cd build ; $(MAKE) -f ../tests/Makefile clean ; $(MAKE) -f ../tests/Makefile

utilsncaru :
	mkdir -p bin; cd build ; $(MAKE) -f ../utils/Makefile clean ; $(MAKE) -f ../utils/Makefile

clean:
	rm -fr ./lib ./bin ./build
