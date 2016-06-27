
FC = gfortran
FFLAGS = -g
# -std=f95
# arithmetic if statements are obsolescent in F95 and newer

TARGET=dumpgp1
FREE_FORM_OBJS = RDGEN1.o swap.o
FIXED_FORM_OBJS = bytes.o
OBJS = $(FREE_FORM_OBJS) $(FIXED_FORM_OBJS)

all: $(TARGET)

$(FIXED_FORM_OBJS): %.o: %.FOR
	$(FC) $(FFLAGS) -ffixed-form -c $<

$(FREE_FORM_OBJS): %.o: %.FOR
	$(FC) $(FFLAGS) -ffree-form -c $<

$(TARGET): %: %.FOR $(OBJS)
	$(FC) $(FFLAGS) -ffree-form $< $(OBJS) -o $@
