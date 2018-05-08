C
C  test routine for testing the pack code.


	program pack_test

	parameter 		(num = 30)
	parameter 		(iwords = 4)
	real*8 			raw(100)



	do 10 i = 1, num
		raw(i) = 0.0
10	continue

	raw(1) = 1.e-300



	call pack_unpack(raw, num, 4)

	end


	subroutine pack_unpack(raw, num_words, iwd_pack)

	double precision	raw(*)
	integer			num_words
	integer 		iwd_pack
	real*8	packed(100)
	real*8	unpacked(100)
	real*8	amn, amx
	integer			i


C Pack the data.


	call packaf(raw, num_words, packed, iwd_pack, amn, amx)

	write(6,101) num_words, iwd_pack, amn, amx
101	format("Num words = ", i2, " Pack count = ", i1," Min = ", 
     +         E22.14, " Max = ", E22.14)

	call unpkaf(unpacked, num_words, packed, iwd_pack, amn, amx)

	print*, " "
	print*, "I      Raw(i)      Unpk(i)     Error(i)"
	print*, "======================================="


	do 30 i = 1, num_words
		write(6,100) i, raw(i),  unpacked(i),
     +                       (abs(raw(i) - unpacked(i)))
30	continue

100	format(i2, f12.5, "     ", f12.5, "     ", f12.5)

	end

