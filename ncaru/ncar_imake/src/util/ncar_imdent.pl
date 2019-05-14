XCOMM!/usr/local/perl

XCOMM	imdent - imake #-directive indenting utility

XCOMM	Not recommended for use with rules files!

XCOMM	Syntax: imdent [ -n ] [ file ]

XCOMM	-n	Specify indent increment (n is a number). Default is 2 spaces.
XCOMM		"imdent -0" removes all indenting, so no "imundent" program
XCOMM		is necessary.

XCOMM	10 Mar 93	Paul DuBois	dubois@primate.wisc.edu

XCOMM	10 Mar 93 V1.00.  Created.
XCOMM	18 Mar 93 V1.01.  Added warning for extraneous #else/#endif.
XCOMM	21 Mar 93 V1.02.  Detect non-zero indent at EOF (missing #endif).

$line = 0;
$indent = 0;
$increment = 2;

XCOMM proccess indent-increment-specifying flag if present

if ($#ARGV >= 0 && $ARGV[0] =~ /^-/)
{
	die "imdent: bad flag: " . $ARGV[0] . "\n"
		unless ($ARGV[0] =~ /^-[0-9]+$/);
	$increment = int (substr ($ARGV[0], 1));
	shift (@ARGV);
}

XCOMM read input

while (<>)
{
	$line++;
	unless (/^# *(\S+)(.*)$/)	# skip non-directives
	{
		print;
		next;
	}
	chop;
	$directive = $1; $rest = $2;

	$before = $after = 0;
	$before = -$increment if $directive eq "else";
	$before = -$increment if $directive eq "elif";
	$before = -$increment if $directive eq "endif";
	$after = $increment if substr ($directive, 0, 2) eq "if";
	$after = $increment if $directive eq "else";
	$after = $increment if $directive eq "elif";

	$indent += $before;
	if ($indent < 0)
	{
		print stderr "extraneous \"$directive\" at line $line\n";
		$indent = 0;
	}
	print "#";
	for ($i = 0; $i < $indent; $i++)
	{
		print " ";
	}
	print "$directive$rest\n";
	$indent += $after;
	if ($indent < 0)
	{
		print stderr "extraneous \"$directive\" at line $line\n";
		$indent = 0;
	}
}

if ($indent > 0)
{
	print stderr "missing #endif after line $line\n";
	$indent = 0;
}

exit (0);
