
include ../make.inc

SRCS := cray.c conversion.c packf.c
#LIBDIR := ../lib
LIBDIR := ../../lib
LIBFILE := libncaru.a
LIB := $(LIBDIR)/$(LIBFILE)
VPATH := ../src
TSRC := 

all : $(LIB)

OBJS := $(subst .c,.o,$(SRCS))


$(LIB) : $(OBJS)
	mkdir -p $(LIBDIR);rm -f $(LIB); $(AR) cq $(LIB) $(OBJS)

%.o : %.c
	$(CCOMP) -c $<

clean:
	rm -f $(LIB) $(OBJS)
