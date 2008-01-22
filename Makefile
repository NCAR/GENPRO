#  Make file for g2n -- GENPRO to netCDF converter   RLR <19941114.1305>
#                                                    rev <19980903.1545>
#                                              Linux rev <20040318.1545>
#
#  Note:  To create an executable version of this program, library files are
#          required in addition to the files in this directory.  These include
#          the netCDF library (-lnetcdf) as well as the COS-block utility
#          library (-lncaru).
#
#
#    Compiler options:
#     -g -- generate symbol table for use by debuggers
#     -c -- do not invoke the linker
#
#CC=  cc
CC=  gcc
#
# Linux
#DESTDIR=  /net/local_lnx/
# Solaris
#DESTDIR=  /usr/local/
BINDIR=    bin
MANDIR=    man/man1
LIB_DIRS= -L${JLOCAL}/lib
LIBS=    -lraf -Wl,-Bstatic -lnetcdf -Wl,-Bdynamic -lncaru
#LINTLIBS=  ${LIBS}
LINTLIBS=  
LOCAL_INC = ${JLOCAL}/include/
INCLUDES=  -I${LOCAL_INC}
CFLAGS=    -g ${INCLUDES}
#CFLAGS=    -O2 ${INCLUDES}

SRCS=  get_comment.c get_datlog.c get_datsiz.c get_exd.c get_ext.c \
    get_filenames.c get_jobid.c get_lkey.c get_logbit.c get_lparms.c \
    get_names.c get_oskeys.c get_prd.c get_proj.c name_idx.c \
    get_hrec.c get_phrec.c par_preset.c par_validate.c read_hdr.c \
    nprint.c GpOpen.c get_pdrec.c get_ldrec.c d_convert.c time_var.c \
    CreateNetCDF.c CloseNetCDF.c Plow.c to_hms.c cleanup.c \
    setup_pms1d.c d_P_convert.c \
    TrimTrailingBlanks.c \
    g2n.c

HDRS=  g2n.h

OBJS=  get_comment.o get_datlog.o get_datsiz.o get_exd.o get_ext.o \
    get_filenames.o get_jobid.o get_lkey.o get_logbit.o get_lparms.o \
    get_names.o get_oskeys.o get_prd.o get_proj.o name_idx.o  \
    get_hrec.o get_phrec.o par_preset.o par_validate.o read_hdr.o \
    nprint.o GpOpen.o get_pdrec.o get_ldrec.o d_convert.o time_var.o \
    CreateNetCDF.o CloseNetCDF.o Plow.o to_hms.o cleanup.o \
    setup_pms1d.o d_P_convert.o \
    TrimTrailingBlanks.o \
    g2n.o
PROG=  g2n

.c.o:
	umask 2
	${CC} ${CFLAGS} -c $*.c

${PROG}: ${OBJS}
	umask 2
#	${CC} ${CFLAGS} ${OBJS} ${LIBS} -o $@
	${CC} ${CFLAGS} ${LIB_DIRS} ${OBJS} ${LIBS} -o $@

install: ${PROG}
#	install -m 755 -s ${PROG} ${DESTDIR}${BINDIR}
#	install -m 444 ${PROG}.1 ${DESTDIR}${MANDIR}
	install -m 755 -g local -s ${PROG} ${DESTDIR}${BINDIR}
	install -m 444 -g local ${PROG}.1 ${DESTDIR}${MANDIR}

clean:
	rm -f core* ${OBJS} ${PROG}

lint:
#	splint ${INCLUDES} ${SRCS} ${LINTLIBS}
#	splint -nullpass +boolint -unrecog -usedef -redef -exitarg -dependenttrans -exportlocal -bufferoverflowhigh -nullassign -warnposix  -compdef -nullderef -globstate ${INCLUDES} ${SRCS} ${LINTLIBS}
	splint -nullpass +boolint -unrecog -usedef -redef -exitarg -dependenttrans -exportlocal -bufferoverflowhigh -nullassign -warnposix  -compdef -nullderef -globstate -type -realcompare ${INCLUDES} ${SRCS} ${LINTLIBS}

print:
	enscript -2Gr -b${PROG} ${HDRS} ${SRCS}

${OBJS}:  g2n.h
