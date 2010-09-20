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

	program tstcray

	implicit none

C 	test out the Cray routines.

	character*80 flnm


	flnm = "/tmp/data"
	
C  Make a Cray blocked file.

	print*, "*** MakeCray test ****"
	call MakeCray(flnm)
	print*, " "


C Test out short and long record reads.

	print*, "*** ReadCray test ****"
	call ReadCray(flnm)
	print*, " "


	print*, "*** RewindRead test *** "
	call RewindRead(flnm)
	print*, " "

	print*, "*** BackRead test *** "
	call BackRead(flnm)
	print*, " "

	print*, "*** ShortLong test *** "
	call ShortLong(flnm)
	print*, " "

	print*, "*** Append test *** "
	call Append(flnm)
	print*, " "

	print*, "*** WriteBack test *** "
	call WriteBack(flnm)
	print*, " "

        call unlink("/tmp/test.data")

	print*, "*****  All Tests Complete ******"

	end

C***********************************************************************
	subroutine MakeCray(flnm)

	implicit 	none
	character*(*)	flnm
	integer		icf, crayopen, craywrite, crayclose
	integer		crayweof
	integer		nwds, i
	real		x(2000)


	icf = crayopen(flnm, 1, o'644')
	if (icf .lt. 0) then
	   print*, "FATAL - Unable to open: ", flnm
	   stop
	endif


C  Write a short record. 100 words

	nwds = craywrite(icf, x, 100, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - writing 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Wrote ", 100, " words"

C Write out  several long records.

	do 20 i = 1, 5
	    nwds = craywrite(icf, x, i*200, 0)
	    if (nwds .lt. 0) then
	       print*, "FATAL - writing 1000 words, nwds = ", nwds
	       stop
	    endif
	    print*, "  Wrote ", i*200, " words"
20	continue

C write an eof and another long record.

	i = crayweof(icf)
	if (i .ne. 0) then
	    print*, "FATAL - Unable to write EOF, ier = " , i
	endif
	print*, "  Wrote an EOF"

	nwds = craywrite(icf, x, 1000, 2)
	if (nwds .lt. 0) then
	    print*, "FATAL - writing 1000 words, nwds = ", nwds
	    stop
	endif
	print*, "  Wrote ", 1000, " words"

c close the file

	i = crayclose(icf)
	if (i .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , i
	endif

	print*, "Successfully wrote a blocked file"
	return
	end
	    
C***********************************************************************

	subroutine ReadCray(flnm)

	implicit	none
	character*(*)	flnm
	integer		icf, crayopen, crayread, crayclose
	integer		nwds, i
	real		x(2000)




	icf = crayopen(flnm, 0, o'644')
	if (icf .le. 0) then
	   print*, "Unable to open: ", flnm
	   stop
	endif


C  Read a short record. 100 words

	nwds = crayread(icf, x, 600, 0)
	if (nwds .ne. 100) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"


C read long records

	do 20 i = 1, 5
	    nwds = crayread(icf, x, 1500, 0)
	    if (nwds .ne. i*200) then
	       print*, "FATAL - read ",  nwds, " expected ", i*200
	       stop
	    endif
	    print*, "  Read ", nwds, " words"
20	continue

C  Read the eof....

	nwds = crayread(icf, x, 100, 1)
	if (nwds .ne. -4) then
	    print*, "FATAL - Unable to read EOF, nwds = " , nwds
	    stop
	endif
	print*, "  Read EOF"

C  Read the final record

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. 1000) then
	       print*, "ReadCray - read of 1000 word record ", nwds
	       stop
	endif
	print*, "  Read ", nwds, " words"

C read the final EOF and EOD

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. -4) then
	    print*, "FATAL - Unable to read EOF, nwds = " , nwds
	    stop
	endif
	print*, "  Read EOF"

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. -5) then
	    print*, "FATAL - Unable to read EOD, nwds = " , nwds
	    stop
	endif
	print*, "  Read EOD"

	i = crayclose(icf)
	if (i .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , i
	    stop
	endif

	print*, "Successfully read a blocked file"
	return
	end


C***********************************************************************

	subroutine RewindRead(flnm)

	implicit	none
	character*(*)	flnm
	integer		icf, crayopen, crayread, crayclose
	integer		crayrew
	integer		nwds, i, ier
	real		x(2000)


	icf = crayopen(flnm, 0, o'644')
	if (icf .le. 0) then
	   print*, "Unable to open: ", flnm
	   stop
	endif


C  Read a short record. 100 words

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"


C  Rewind.

	ier = crayrew(icf)
	if (ier .ne. 0) then
	   print*, "FATAL - Unable to rewind, ier = ", ier
	   stop
	endif
	print*,"  Rewind complete"


C  Read a short record. 100 words

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .lt. 0) then
	   print*, "reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"

C read the long records

	do 20 i = 1, 5
	    nwds = crayread(icf, x, 1200, 2)
	    if (nwds .lt. 0) then
	       print*, "FATAL - reading  words, nwds = ", nwds
	       stop
	    endif
	    print*, "  Read ", nwds, " words"
20	continue

C  Rewind.

	ier = crayrew(icf)
	if (ier .ne. 0) then
	   print*, "FATAL - Unable to rewind, ier = ", ier
	   stop
	endif
	print*,"  Rewind complete"

C  Read a short record. 100 words

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"

	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	    stop
	endif

	print*, "Successfully rewound and read a blocked file"

	return
	end

C***********************************************************************

	subroutine BackRead(flnm)

	implicit	none
	character*(*)	flnm
	integer		icf, crayopen, crayread, crayclose
	integer		crayback
	integer		nwds, i, ier
	real		x(2000)


	icf = crayopen(flnm, 0, o'644')
	if (icf .le. 0) then
	   print*, "Unable to open: ", flnm
	   stop
	endif



C  Read a short record. 100 words

	nwds = crayread(icf, x, 100, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"


C  Backspace.

	ier = crayback(icf)
	if (ier .ne. 0) then
	   print*, "Unable to backspace, ier = ", ier
	   stop
	endif
	print*,"  Backspace complete"

C  Read a short record. 100 words

	nwds = crayread(icf, x, 600, 0)
	if (nwds .ne. 100 ) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"


c read 2 long records

	do 20 i = 1, 2
	    nwds = crayread(icf, x, 600, 0)
	    if (nwds .lt. 0) then
	       print*, "FATAL - reading, nwds = ", nwds
	       stop
	    endif
	    print*, "  Read ", nwds, " words"
20	continue


C  backspace again.

	ier = crayback(icf)
	if (ier .ne. 0) then
	   print*, "FATAL - Unable to backspace, ier = ", ier
	   stop
	endif
	print*,"  Backspace complete"

c reread a record

	nwds = crayread(icf, x, 600, 0)
	if (nwds .ne. 400) then
	    print*, "FATAL - reading 400 words, nwds = ", nwds
	    stop
	endif
	print*, "  Read ", nwds, " words"


	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	    stop
	endif

	print*, "Successfully backspaced a blocked file"

	return
	end

C***********************************************************************

	subroutine ShortLong(flnm)

	implicit 	none
	character*(*)	flnm
	integer		icf, crayopen, crayread, crayclose
	real		x(10000)
	integer		nwds, ier


	icf = crayopen(flnm, 0, o'644')
	if (icf .le. 0) then
	   print*, "Unable to open: ", flnm
	   stop
	endif


C  Read a short integer record. 1 word

	nwds = crayread(icf, x, 1, 0)
	if (nwds .ne. 1) then
	   print*, "FATAL reading 1 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read 1   word  (100  word record), nwds = ", nwds

C Read in a long record

	nwds = crayread(icf, x, 500, 0)
	if (nwds .lt. 0) then
	    print*, "FATAL reading 10000 words, ndws = ", nwds
	    stop
	endif
	print*, "  Read 500 words (200 word record), nwds = ", nwds


	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	    stop
	endif

	print*, "Successfully read short and long requests"

	return
	end
****************************************************************
	subroutine Append(flnm)

	implicit	none
	character*(*)	flnm
	integer		icf, crayopen, craywrite, crayclose
	integer		crayread
	real		x(2000)
	integer		nwds, i, ier


C open for appending....


	icf = crayopen(flnm, 3, o'644')
	if (icf .le. 0) then
	   print*, "FATAL - Unable to open: ", flnm
	   stop
	endif
	print*, "  Blocked file opened for appending"


C Write out a short record

	nwds = craywrite(icf, x, 100, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - adding 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Appended 100 words"

C Close and reopen for appending....

	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	    stop
	endif

	icf = crayopen(flnm, 0, o'644')
	if (icf .le. 0) then
	   print*, "FATAL - Unable to open for re-reading: ", flnm
	   stop
	endif
	print*, "  Blocked file reopened."

	do 20 i = 1, 20
	    nwds = crayread(icf, x, 1000, 0)
	    if (nwds .eq. -4) then
	       print*, "  Read EOF"
	    else if (nwds .eq. -5) then
		print*, "  Read EOD"
		goto 400
	    else if (nwds .lt. 0 ) then
		print*, "FATAL - I/O error: ", nwds
	    else
	        print*, "  Read ", nwds, " words"
	    endif
20	continue 

400	continue

	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	endif

	print*, "Successfully appended file"
	end

C***********************************************************************
	subroutine WriteBack(flnm)

	implicit	none
	character*(*)	flnm
	integer		icf, crayopen, craywrite, crayclose
	integer		crayback, crayread
	real		x(2000)
	integer		nwds, ier


	icf = crayopen(flnm, 2, o'644')
	if (icf .le. 0) then
	   print*, "FATAL - Unable to open: ", flnm
	   stop
	endif


C  Write a short record. 100 words

	nwds = craywrite(icf, x, 100, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - writing 100 integers, nwds = ", nwds
	   stop
	endif
	print*, "  Wrote 100 words"

C  Backspace.

	ier = crayback(icf)
	if (ier .ne. 0) then
	   print*, "FATAL - Unable to backspace, ier = ", ier
	   stop
	endif
	print*,"  Backspace complete"

C  Read a record. 

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. 100) then
	   print*, "FATAL - reading 100 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"

C  Write a long record. 1000 words

	nwds = craywrite(icf, x, 1000, 0)
	if (nwds .lt. 0) then
	   print*, "FATAL - writing 1000 words, nwds = ", nwds
	   stop
	endif
	print*, "  Wrote ", nwds, " words"

C  Backspace.

	ier = crayback(icf)
	if (ier .ne. 0) then
	   print*, "FATAL - Unable to backspace, ier = ", ier
	   stop
	endif
	print*,"  Backspace complete"

C  Read a record. 

	nwds = crayread(icf, x, 1500, 0)
	if (nwds .ne. 1000) then
	   print*, "FATAL - reading 1000 words, nwds = ", nwds
	   stop
	endif
	print*, "  Read ", nwds, " words"

C  Read (what should be) an EOF and an EOD. 

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. -4) then
	   print*, "FATAL - EOF not found", nwds
	   stop
	endif
	print*, "  Read EOF"

	nwds = crayread(icf, x, 1000, 0)
	if (nwds .ne. -5) then
	   print*, "FATAL - EOD not found ", nwds
	   stop
	endif
	print*, "  Read EOD"

	ier = crayclose(icf)
	if (ier .ne. 0) then
	    print*, "FATAL - Unable to close, ier = " , ier
	endif

	print*, "Successfully backspaced write file"
	end
