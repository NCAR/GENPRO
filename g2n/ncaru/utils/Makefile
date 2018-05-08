
include ../make.inc

SRCS := cosconvert.c cosfile.c cossplit.c
LIBDIR := ../lib
INCLDIR := ../include
LIB := ncaru
BINS := $(subst .c,,$(SRCS))
OBJS := $(subst .c,.o,$(SRCS))
VPATH := ../utils

all : $(BINS)



$(BINS) : $(OBJS)
	$(CCOMP) -o $@ $@.o -L$(LIBDIR) -l$(LIB);mv -f $@ ../bin

%.o : %.c
	$(CCOMP) -I$(INCLDIR) -c $<

clean:
	rm -f $(OBJS) $(BINS)
