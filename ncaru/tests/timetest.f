	program timetest

	parameter (ii = 100000)
	real	x(ii)
	real	second
	real	t

	do 10 i = 1, ii
		x(i) = 3.14 * sin(float(i))
10	continue


	t = second()

	print*, t

	end

