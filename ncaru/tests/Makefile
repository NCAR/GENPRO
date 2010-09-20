
include ../make.inc

SRCT=craytest.f readtest.f packtest.f unpacktest.f
EXES=$(subst .f,.exe,$(SRCT))
EXERUN=$(subst .exe,.exe;,$(EXES))

VPATH=../tests

all : run

run : $(EXES)
	ln -s ../tests/cray_blocked_data cray_blocked_data; 
	ln -s ../tests/cray_packed_data cray_packed_data; 
	for pgm in $(EXES) ;\
	do                 \
	        echo "--------------------------------------------"; \
	        echo "--------- Executing $${pgm} ----------------"; \
		./$${pgm} ;  \
	        echo "--------------------------------------------"; \
	done

%.exe : %.f
	$(FCOMP) -o $@ $< -L../lib -lncaru

clean :
	rm -f $(EXES)
