
C Copyright 1994 University Corporation for Atmospheric Research (UCAR).
	All rights reserved
C 
C Permission to use, copy, and modify this software and its documentation 
C for any non-commercial purpose is hereby granted without fee, provided 
C that the above copyright notice appear in all copies and that both that 
C copyright notice and this permission notice appear in supporting 
C documentation. UCAR makes no representations about the suitability of 
C this software for any purpose.   It is provided "as is" without express
C or implied warranty.

	program writecray

	real		a(100)
	integer		i(100)
	character*(80)	string

	string = "Hello World"
	a = 3.14
	i = 2

	open(unit=23, form='unformatted')

	write(23) string
	write(23) a
	write(23) i

	close(23)
	end
