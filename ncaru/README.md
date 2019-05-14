
This is the source distribution for the Cray file conversion 
library "ncaru" (NCAR Utilities).  This library contains Fortran and C
bindings to routines for reading and writing Cray style datasets
from non-Cray platforms.  

Description
-----------
The library contains the following user entry points:

       crayblocks()  	- Sets the size of the library buffer.
       crayback()	- Moves back a record in a Cray file.
       crayclose()	- Closes a Cray file.
       crayopen()	- Opens a Cray blocked or unblocked file.
       crayread()	- Reads data from a Cray file.
       crayrew()	- Rewinds a Cray file.
       crayweof()	- Writes an EOF to a Cray file
       crayweod()	- Writes an EOD to a Cray file.
       craywrite()	- Writes data to a Cray file.
       crayseek()	- Seek to a point in a Cray file.

In addition, a number of numeric conversion routines are also included.
These can be used indirectly through the crayread() and craywrite()
routines, or directly from Fortran/C.  These routines convert to/from Cray
64bit values only. 

       ctodpf()		- Cray to IEEE double precision.
       ctospf()		- Cray to IEEE single precision. (floating point)
       ctospi()		- Cray to IEEE single precision. (integer)
       dptocf()		- IEEE double precision (floating) to Cray. 
       dptoci()		- IEEE double precision (integer) to Cray. 
       sptocf()		- IEEE single precision (floating) to Cray.
       sptoci()		- IEEE single precision (integer) to Cray.


Finally, three Cray blocked file utilities are included.  These are:
	
	cosfile 	- Examines a blocked file and reports record sizes.
	cosconvert	- Strips Cray blocking from a file.
	cossplit	- Splits a multifile Cray blocked dataset into 
			  separate files.


Finally, for historical (hysterical?) reasons, there are several other 
routines in the library.  The "packing" routines pack a number of words
into a single 64bit space losing all kinds of precision.  Be forewarned,
this stuff was written in the mid 70's....  Need I say more? :-)


Features
--------
The library has the following features:

    1) Ability to read and write both Cray unblocked or blocked
       datasets.

    2) Automatic numeric conversion between Cray single precision data 
       formats and IEEE (both single and double precision) formats 
       during crayread() or craywrite().  You can also call the numeric
       conversion routines directly yourself.

    3) Ability to specify a library buffer of an appropriate size. 
       This can save a significant amount of system CPU time during I/O
       to large files.

    4) Full BACKSPACE and REWIND implementations which mimic Cray Fortran.

    5) The ability to append to the end of the last file in a 
       Cray dataset.  (See crayopen(3))

    6) Full and partial record operations for blocked files.  Regardless 
       of the requested number of words, crayread always returns at the 
       next record.  
       
       This means that you can read a single word, and move the record 
       pointer to the next record.  Useful to skip records without
       having a large unused buffer lying about.  

       You can also request a larger number of words than the actual 
       record contains, and only the actual number of words in the record
       are transferred.   Usful for 'mystery' files.

    7) Both Fortran and C interfaces are documented in the man pages. 



Porting
-------

This version of the library has been ported to the following OS's:

	IRIX-6.5.+
	Solaris 5.5+
	Linux RedHat 6.1
	OSF1	4.0 and 5.0 


See the acknowledgements section below.



Building
--------


The Makefiles for the distribution are created using something akin to 
X11's imake.  You will need to build and install this package first.

1) cd ncar_imake.  Follow the README and build/install.

2) Ensure that ncar_imake is in your search path, then from this 
   directory:


   a) cd cf, verify the debug flags set in the site.p-def file are 
      correct for your cc.

      Also verify the ProjectRoot definition, this is where you 
      want the NCARU package installed.

	
   b) From this directory:

	% make -f Makefile.ini
	% make World


   c) Resolve any problems you encounter.  This may mean modifying the 
      configuration files for ncar_imake.



Testing
-------
cd to the tests directory and look at the README.
A test program for the Cray I/O routines is built when you build the
distribution.  There are shells available for testing the numeric
conversion routines.


Acknowledgements
----------------
This distribution is the result of several people's efforts.  Tom Parker
wrote the original COS utilities, cosfile, cosconvert, and cossplit for
the Cray.  Chuck D'ambra write the original numeric conversion routines
for a utility his division uses.  Craig Ruff wrote the original Cray blocking
routines.  Dan Anderson and Greg Woods originally combined both the 
conversions and Cray routines into a single package.  John Clyne is
responsible for the ncar_imake distribution.

The author took the whole mess and added backspacing, wrote man pages,
optimized, added other enhancements, and rewrote the COS utilities using
the new library. Comments, enhancements, etc, are welcome.

We have moved gigabytes of data using these routines on our machines. 
Hope you find them useful at your site.

-PWM
------------------------------------------------------------------
Peter W. Morreale                  email:  morreale@ncar.ucar.edu
Nat'l Center for Atmos Research    voice:  (303) 497-1293
Scientific Computing Division      Boulder, Colorado
Consulting Office
------------------------------------------------------------------

(c) Copyright 1994 University Corporation for Atmospheric Research (UCAR).
	All rights reserved

Permission to use, copy, and modify this software and its documentation 
for any non-commercial purpose is hereby granted without fee, provided 
that the above copyright notice appear in all copies and that both that 
copyright notice and this permission notice appear in supporting 
documentation. UCAR makes no representations about the suitability of 
this software for any purpose.   It is provided "as is" without express
or implied warranty.

