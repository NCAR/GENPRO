#!/bin/csh -f

if ($#argv != 2) then
	echo "Usage: $0 f1 f2"
	exit 1
endif

set uname	= "./uname -r"
set v		= (`$uname | tr '.' ' '`)

if ($#v == 1) then
	set v = ($v[1] "0" "0")
else if ($#v == 2) then
	set v = ($v[1] $v[2] "0")
if ($#v != 3) then
	set v = ("0" "0" "0")
endif
endif

sed -e "s#MAJOR#$v[1]#" \
	-e "s#MINOR#$v[2]#" \
	-e "s#TEENY#$v[3]#" < $argv[1] >! $argv[2]

exit 0
