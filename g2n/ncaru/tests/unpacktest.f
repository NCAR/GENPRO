	program unpacktest

c
c	Test unpacking of Cray data set named "cray_packed_data" 
c
c	The dataset contains 2 records.  These are
c	
c	1)  7 words, packed 4 values per word
c	2)  12 words, packed 2 values per word.
c	
c	Every value is 3.14


	double precision 	raw(20)
	integer			cfd
	integer 		crayopen, crayread

	do 10 i = 1, 20
		raw(i) = 0.0
10	continue


	cfd = crayopen("cray_packed_data", 0, 23)
	if (cfd .lt. 0) then
		print*, "crayopen: ", cfd
		stop
	endif

	nwds = crayread(cfd, raw, 7, 0)
	if (nwds .ne. 7) then
		print*, "crayread: ", nwds
		stop
	endif

	call unpackf(raw, 20, 4)

	nwds = crayread(cfd, raw, 12, 0)
	if (nwds .ne.12) then
		print*, "crayread: ", nwds
		stop
	endif

	call unpackf(raw, 20, 2)

	end


	subroutine unpackf(raw, num_words, iwd_pack)

	double precision 	raw(*)
	integer			num_words
	integer 		iwd_pack
	double precision	unpacked(200)
	double precision	amn, amx
	integer			i


C Pack the data.


	write(6,101) num_words, iwd_pack, amn, amx
101	format("Num words = ", i2, " Pack count = ", i1," Min = ", 
     +         f6.3, " Max = ", f6.3)

	call unpkaf(unpacked, num_words, raw, iwd_pack, amn, amx)

	print*, " "
	print*, "I      Raw(i)      Unpk(i)     Error(i)"
	print*, "======================================="


	do 30 i = 1, num_words
		write(6,100) i, 3.14,  unpacked(i),
     +                       (abs(3.14 - unpacked(i)))
30	continue

100	format(i2, " ", f12.5, " ", f12.5, " ", f12.5)

	end

