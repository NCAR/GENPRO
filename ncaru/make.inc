
UNAMES := $(shell uname -s)

ifeq ($(UNAMES),AIX)

    CC := /usr/vac/bin/xlc_r
    AR := /usr/bin/ar
    CPPFLAGS := -DArchIBM -DSTRICT
    CCOMP := $(CC) $(CPPFLAGS)
    FCOMP := xlf
    MAKE := gmake

endif

ifeq ($(UNAMES),SunOS)

    CC := /opt/SUNWspro/bin/cc
    AR := /usr/ccs/bin/ar
    CPPFLAGS := -DArchSun -DSTRICT
    CCOMP := $(CC) $(CPPFLAGS)
    FCOMP := /opt/SUNWspro/bin/f90
    MAKE := /fs/local/bin/make

endif

ifeq ($(UNAMES),IRIX64)

    CC := /usr/bin/cc
    AR := /usr/bin/ar
    CPPFLAGS := -DArchSGI -DSTRICT
    CCOMP := $(CC) $(CPPFLAGS)
    FCOMP := /usr/bin/f90
    MAKE := /usr/local/bin/gmake

endif

ifeq ($(UNAMES),Linux)

    CC := /usr/bin/gcc
    AR := /usr/bin/ar
    CPPFLAGS := -DArchLinux -DSTRICT
    CCOMP := $(CC) $(CPPFLAGS)
    FCOMP := pgf90
    MAKE := make

endif
