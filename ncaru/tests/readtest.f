
C Copyright 1994 University Corporation for Atmospheric Research (UCAR).
C	All rights reserved
C 
C Permission to use, copy, and modify this software and its documentation 
C for any non-commercial purpose is hereby granted without fee, provided 
C that the above copyright notice appear in all copies and that both that 
C copyright notice and this permission notice appear in supporting 
C documentation. UCAR makes no representations about the suitability of 
C this software for any purpose.   It is provided "as is" without express
C or implied warranty.

	program readcray

	real			a(100)
	double precision	b(100)
	integer			i(100)
	character*(40)		string
	integer			crayopen, crayread, crayclose
	integer			crayback

	icf = crayopen("cray_blocked_data", 0, o'660')
	if (icf .lt. 0) then
	   print*, "Unable to open cray_blocked_data, icf = ", icf
	   stop
	endif

C Read the character string.  Note: no conversion is used

	nwds = crayread(icf, string, 10, 0)
	if (nwds .ne. 10) then
	    print*, "Unable to read character record"
	    stop
	endif
	print*, string

C
C Read in the reals.  The record contains 100 words, read into 
C 32bit reals.
C 
C THis record is packed with 100 reals of value: 3.14, check it.
C
	nwds = crayread(icf, a, 100, 1)
	if (nwds .ne. 100) then
	    print*, "Unable to read real record, nwds = ", nwds
	    stop
	endif
	write(6,100) a(1), a(100)


C
C
C 	backspace one record and re-read the reals as doubles...
C
	ier = crayback(icf)
	
	
	nwds = crayread(icf, b, 100, 2)
	if (nwds .ne. 100) then
	    print*, "Unable to read real record, nwds = ", nwds
	    stop
	endif
	write(6,100) a(1), a(100)

100	format("a(1) = ", f8.5, " a(100) = ", f8.5)


C
C Read in the integers.  The conversion flag is 3 for 4 byte integers
C or set it to 4 for 8 byte integers.
C
	nwds = crayread(icf, i, 100, 3)
	if (nwds .ne. 100) then
	    print*, "Unable to read integer record, nwds = ", nwds
	    stop
	endif
	print*, "I(1) = ", i(1), " I(50) = ", i(50), " I(100) = ", i(100)

	ier = crayclose(icf)

	end

