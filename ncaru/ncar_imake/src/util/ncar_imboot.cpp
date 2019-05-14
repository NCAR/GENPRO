XCOMM!/bin/sh

XCOMM
XCOMM	$Id: ncar_imboot.cpp,v 1.1.1.1 1999/05/14 20:57:11 morreale Exp $
XCOMM

XCOMM  ncar_imboot - imake bootstrapper (bootstrap Makefile from Imakefile)

XCOMM Usage:  ncar_imboot [ flags ] [ topdir [ curdir ] ]

XCOMM flags can be:
XCOMM	-i path		Specify the path the the ncar_imake config files. If
XCOMM			'path' is the keyword 'topdir' then the path will
XCOMM			be optained by concatenating the value of 'topdir',
XCOMM			followed by a '/', followed by 'cf'.
XCOMM
XCOMM	-p path 	Specify the path to private (project) config files
XCOMM
XCOMM	-Danything	pass -Danything to imake
XCOMM
XCOMM	-Ianything	pass -Ianything to imake
XCOMM
XCOMM	-d		force UseInstalled on
XCOMM
XCOMM	-u		force UseInstalled off

XCOMM topdir = path to top of project tree (default ".")
XCOMM curdir = path to current directory from top of project tree (default ".")

XCOMM If no -P options is given, ncar_imake config files are searched
XCOMM for in PUBCONFIGDIR

XCOMM You can also specify other -D and -I flags, to allow arbitrary
XCOMM symbols to be defined or undefined, or other search directories to
XCOMM be given.

XCOMM The -d and -u options may be used to force UseInstalled on or off,
XCOMM overriding the default behavior. UseInstalled is always on unless
XCOMM the the -p or -i argument is 'topdir'

XCOMM
XCOMM Most of this code was shamelessly stolen from Paul DuBois, the author
XCOMM of the Oreilly book, "Software Portability with imake"
XCOMM

usage="usage: $0 [-p <topdir>|path] [-i <topdir>|path] [-d] [-u] [-Dsym] [-Idir] [topdir [curdir]]"

niconfigdir=PUBCONFIGDIR
privconfigdir=
useinstalled=-DUseInstalled
topdir=.
curdir=.
dopt=n
uopt=n
args=

XCOMM check for -p, -i, -D, -I, -d, -u options

while [ $# -gt 0 ]; do
	case "$1" in
		-d)			# force on unconditionally
			dopt=y
			uopt=n
			shift
			;;
		-u)			# force off unconditionally
			dopt=n
			uopt=y
			shift
			;;
		-p)			# private config file directory
                        if [ $# -lt 2 ]; then
                                echo "$usage" 1>&2
                                exit 1
                        fi
                        privconfigdir=$2
                        shift;shift
                        ;;
                -i)			# ncar_imake config file directory
                        if [ $# -lt 2 ]; then
                                echo "$usage" 1>&2
                                exit 1
                        fi
                        niconfigdir=$2
                        shift;shift
                        ;;

		-[DI]*)			# pass through -D, -U, -I options
			args="$args $1"
			shift
			;;
		-*)			# unrecognized flag
			echo "$usage" 1>&2
			exit 1
			;;
		*)			# non-flag; quit flag processing
			break
			;;
	esac
done

if [ $# -gt 0 ]; then
	topdir="$1"
	shift
fi
if [ $# -gt 0 ]; then
	curdir="$1"
fi

if [ "$niconfigdir" = "topdir" ]; then
	niconfigargs="-I$topdir/cf -DPubConfigDir=ConfigRootDir"
	useinstalled=
else
	niconfigargs="-I$niconfigdir -DPubConfigDir=$niconfigdir"
fi

if [ "$privconfigdir" = "" ]; then
	privconfigargs=
else if [ "$privconfigdir" = "topdir" ]; then
		privconfigargs="-I$topdir/cf"
		useinstalled=
else
		privconfigargs="-I$privconfigdir"
fi
fi

XCOMM force UseInstalled on/off if -d/-u were given
if [ "$dopt" = "y" ]; then
	useinstalled=-DUseInstalled
fi
if [ "$uopt" = "y" ]; then
	useinstalled=
fi

XCOMM backup current Makefile if it exists
if [ -f Makefile ]; then 
	echo mv Makefile Makefile.bak
	mv Makefile Makefile.bak
fi

XCOMM echo command, then run it
echo ncar_imake $args $useinstalled $privconfigargs $niconfigargs -DTOPDIR=$topdir -DCURDIR=$curdir

ncar_imake $args $useinstalled $privconfigargs $niconfigargs -DTOPDIR=$topdir -DCURDIR=$curdir
