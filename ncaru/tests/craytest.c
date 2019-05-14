
/*
**      $Id: craytest.c,v 1.1 2000/02/23 19:19:50 morreale Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  2000			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		craytest.c
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Wed Feb 23 10:09:18 MST 2000
**
**	Description:	Implements a test of the Cray IO functions.
*/
#include <stdio.h>
#include <errno.h>
#include <math.h>
#include <string.h>

#include <ncaru/cray.h>


static const char	*flnm = "/tmp/cray_test_data";

/*
** Data for reads and writes...
*/
#define MAX_WDS		1000
static float	reals[MAX_WDS] 	= {0.0};
static double	doubles[MAX_WDS]= {0.0};
static int	ints[MAX_WDS] 	= {0};


/*
** ERROR()
*/
static
void	ERROR(
	int		line,
	const char	*label,
	const char	*routine,
	int		rc,
	const char	*msg
) {

	(void)fprintf(stderr, "line=%d function=%s routine=%s rc=%d msg=%s\n", 
		line,
		label,
		routine,
		rc, msg
	);

	return;
}





/*
** make_cray
*/
static 
int make_cray(
	void
) {

	int	icf;
	int	i;


	icf = CrayOpen(flnm, 1, 0644);
	if (icf < 0) {
		ERROR(__LINE__, "make_cray", "CrayOpen", icf, strerror(errno));
		return(-1);
	}


	/*
	** Init...
	*/
	for(i = 0; i < MAX_WDS; i++) {
		reals[i] = 3.14;
		doubles[i] = 2.12;
		ints[i] = i;
	}


	i = CrayWrite(icf, reals, MAX_WDS, 1);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	i = CrayWrite(icf, doubles, MAX_WDS, 2);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}


	i = CrayWrite(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	/*
	** close...
	*/
	i = CrayClose(icf);
	if (i != 0) {
		ERROR(__LINE__, "make_cray", "CrayClose", i, strerror(errno));
		return(-1);
	}

	return(0);

}


/*
** read_cray()
*/
static
int read_cray(
	void
) {
	int	icf;
	int	i;


	icf = CrayOpen(flnm, 0, 0644);
	if (icf < 0) {
		ERROR(__LINE__, "make_cray", "CrayOpen", icf, strerror(errno));
		return(-1);
	}


	/*
	** Init...
	*/
	for(i = 0; i < MAX_WDS; i++) {
		reals[i] = 0.0;
		doubles[i] = 0.0;
		ints[i] = 0;
	}


	i = CrayRead(icf, reals, MAX_WDS, 1);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	for(i = 0; i < MAX_WDS; i++) {
		if ( fabs((double) reals[i] - 3.14) > .0001) {
		ERROR(__LINE__, "read_cray", "CrayRead", i, "Numeric error");
			return(-1);
		}
	}

	i = CrayRead(icf, doubles, MAX_WDS, 2);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	for(i = 0; i < MAX_WDS; i++) {
		if (fabs(doubles[i] - 2.12) > .0001) {
		ERROR(__LINE__, "read_cray", "CrayRead", i, "Numeric error");
			return(-1);
		}
	}

	i = CrayRead(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "make_cray", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	for(i = 0; i < MAX_WDS; i++) {
		if (ints[i] != i) {
		ERROR(__LINE__, "read_cray", "CrayRead", i, "Numeric error");
			return(-1);
		}
	}

	/*
	** close...
	*/
	i = CrayClose(icf);
	if (i != 0) {
		ERROR(__LINE__, "read_cray", "CrayClose", i, strerror(errno));
		return(-1);
	}

	return(0);
}


/*
** rewind_back_append()  - Test backspace reqind and append
*/
static
int rewind_back_append(
	void
) {
	int	icf;
	int	i;


	icf = CrayOpen(flnm, 2, 0644);
	if (icf < 0) {
		ERROR(__LINE__, "rewind_etc", "CrayOpen", icf, strerror(errno));
		return(-1);
	}


	/*
	** Init...
	*/
	for(i = 0; i < MAX_WDS; i++) {
		reals[i] = 0.0;
		doubles[i] = 0.0;
		ints[i] = 0;
	}


	i = CrayRead(icf, reals, MAX_WDS, 1);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	i = CrayRead(icf, doubles, MAX_WDS, 2);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	i = CrayRead(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	/*
	** Best hit eof here...
	*/
	i = CrayRead(icf, ints, MAX_WDS, 3);
	if (i != CRAY_EOF) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, "expected EOF");
		return(-1);
	}


	/*
	** rewind...
	*/
	i = CrayRew(icf);
	if (i != 0) {
		ERROR(__LINE__, "rewind_etc", "CrayRewind", i, "Bad rewind");
		return(-1);
	}


	/*
	** read again...
	*/

	i = CrayRead(icf, reals, MAX_WDS, 1);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	i = CrayRead(icf, doubles, MAX_WDS, 2);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	i = CrayRead(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	/*
	** Now append...
	*/
	i = CrayWrite(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayWrite", i, strerror(errno));
		return(-1);
	}

	/*
	** Finally backspace..
	*/
	i = CrayBack(icf);
	if (i != 0) {
		ERROR(__LINE__, "rewind_etc", "CrayBack", i, strerror(errno));
		return(-1);
	}

	i = CrayRead(icf, ints, MAX_WDS, 3);
	if (i != MAX_WDS) {
		ERROR(__LINE__, "rewind_etc", "CrayRead", i, strerror(errno));
		return(-1);
	}

	/*
	** close...
	*/
	i = CrayClose(icf);
	if (i != 0) {
		ERROR(__LINE__, "rewind_etc", "CrayClose", i, strerror(errno));
		return(-1);
	}

	return(0);
}


/*
** main...
*/
main() 
{
	
	if (make_cray()) exit(1);

	if (read_cray()) exit(1);

	if (rewind_back_append()) exit(1);


	printf("all tests completed sucessfully.\n");

	(void)unlink(flnm);


	return(0);
}
