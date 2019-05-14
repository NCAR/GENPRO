
/*
**      $Id: packf.c,v 1.2 2000/02/18 18:04:18 morreale Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		packf.c
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Wed Aug 27 10:44:52 MDT 1997
**
**	Description:	Implements the packing routines...
*/
#include <stdio.h>
#include <stdlib.h>

#include "conversion.h"


/*****************************************************************************/
/*                                                                           */
/*            P R I V A T E   R O U T I N E S                                */
/*                                                                           */
/*****************************************************************************/

/*
** double_abs() - Return the absolute value of a double.
*/

static 
double	double_abs(
	double	value
) {
	
	if (value < 0.0)  return(value * -1.0);

	return(value);
		
}


/*
** pack4f() pack 4 words into 1.
*/
static
void pack4f(
	double	*arn,
	int	*nrn,
	double	*apn 
) {
	int npn;  /* number packed values */
	union {
		double temp;
		unsigned short int it[4]; /* 16 bit unsigned integer packed  */
	} data;
	int i, j; 

	npn = *nrn/4;

	for(j=0; j < npn; j++){
		
		i = 4 * j;
		data.it[0] = (arn[i] - apn[0]) * apn[1] + 0.5;
		data.it[1] = (arn[i+1] - apn[0]) * apn[1] + 0.5;
		data.it[2] = (arn[i+2] - apn[0]) * apn[1] + 0.5;
		data.it[3] = (arn[i+3] - apn[0]) * apn[1] + 0.5;
		apn[ j+2 ] = data.temp;
	}


	if ( 4 * npn != *nrn){
		i = 4 * j;
		j++;

		data.it[0] = (arn[i] - apn[0]) * apn[1] + 0.5;
		i++;
		data.it[1] = (i < *nrn) ? (arn[i] - apn[0]) * apn[1] + 0.5: 0;
		i++;
		data.it[2] = (i < *nrn) ? (arn[i] - apn[0]) * apn[1] + 0.5: 0;
		data.it[3] = 0;

		apn[j+2] = data.temp;
	}
	return;
}


/*
** unpack4f() - Unpack 1 word into 4.
*/
static
void unpack4f(
	double	*arn,
	int	*nrn,
	double	*apn
) {
	union {
	    double      	temp;   /* working storage for packed word */
	    unsigned short int  it[4];  /* packed integer reference */
	} data;
	int npn; 			/* number of packed "words"  */
	int j, i;

	npn = *nrn/4;  	/* get number of packed values */

	for(j=0; j<npn; j++){
	
		i = 4 * j;

		data.temp = apn[j+2];

		arn[i]   = data.it[0] / apn[1] + apn[0];
		arn[i+1] = data.it[1] / apn[1] + apn[0];
		arn[i+2] = data.it[2] / apn[1] + apn[0];
		arn[i+3] = data.it[3] / apn[1] + apn[0];

	}


	if( 4 * npn != *nrn){
		i = 4 * j;
		j++;
		data.temp = apn[j+2];
		arn[i] = data.it[0] / apn[1] + apn[0];
		i++;
		if (i >= *nrn) return;
		arn[i] = data.it[1] / apn[1] + apn[0];
		i++;
		if (i >= *nrn) return;
		arn[i] = data.it[2] / apn[1] + apn[0];
	}

	return;
}


/*
** pack2f() - pack 2 words into 1.
*/
static
void pack2f(
	double	*arn,
	int	*nrn,
	double	*apn
) {
	int npn;  /* number packed values */

	union {
		double 		temp;
		unsigned int 	it[2]; 
	} data;
	int 		i, j; 
	double		xmin	= apn[0];
	double		xsf 	= apn[1];

	npn = *nrn/2;

	for(j=0; j < npn; j++){
		i = 2 * j;
		data.it[0] = (unsigned int) ((arn[i]   - xmin) * xsf);
		data.it[1] = (unsigned int) ((arn[i+1] - xmin) * xsf);
		apn[j+2] = data.temp;
	}

	if ( 2 * npn != *nrn){
		i = 2 * j;
		j++;
		data.it[0] = (unsigned int) ( arn[i] - xmin) * xsf;
		data.it[1] = (unsigned int) 0;
		apn[j+2] = data.temp;
	}
	return;
}

/*
** unpack2f() - unpack 1 word into 2.
*/
static
void unpack2f(
	double	*arn,
	int	*nrn,
	double	*apn
) {
	union {
		double 		temp;   /* working storage for packed word */
		unsigned int 	it[2];  /* packed integer reference */
	} data;
	int 	npn; /* number of packed "words"  */
	int 	j, i;

	double	xmin, xsf;

	npn = *nrn/2;  /* get number of packed values */

	xmin = apn[0];	/* The min. */
	xsf = apn[1];	/* The scale factor */


	for(j=0; j < npn; j++){
	
		i = 2*j;

		data.temp = apn[j+2];

		arn[i]   =  ((double) data.it[0]) / xsf + xmin;
		arn[i+1] =  ((double) data.it[1]) / xsf + xmin;

	}


	if( 2 * npn != *nrn){

		i = 2 * j;
		j++;
		data.temp = apn[j+2];
		arn[i]   = data.it[0] / xsf + xmin;
	}

	return;
}
/*
** unpack() - same for all unpacks...
*/
static
void	unpack(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {

	double	po2;
	double	x[2];
	int	i;




	if (*npk == 2) 
		po2 = 4294967295.;
	
	if (*npk == 4) 
		po2 = 65535.;

	/*
	** Error checks...
	*/
	if (*nrn <= 0) {
		(void)fprintf(stderr, "packaf - array length .le. 0 - stop\n");
		exit(1);
	}

	if (*npk != 2 && *npk != 4) {
		(void)fprintf(stderr, "packaf - packing density .ne. 2  or 4 - stop\n");
		exit(1);
	}


	x[0] = apn[0];
	x[1] = apn[1];
	i = 2;

	(void)ctodpf(x, apn, &i);


	*amn = apn[0];
	*amx = apn[0] + (po2 / apn[1]);

	if (*npk == 2) 
		unpack2f(arn, nrn, apn);
	else
		unpack4f(arn,nrn, apn);

	apn[0] = x[0];
	apn[1] = x[1];

	return;



	
}

/*****************************************************************************/
/*                                                                           */
/*            P U B L I C   R O U T I N E S                                  */
/*                                                                           */
/*****************************************************************************/



/*
** Function:		packaf()
**
** Description:		
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/

void packaf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	double		po2;
	double		xmn = arn[0];
	double		xmx = arn[0];
	double		max_rcp = 1.0;
	double		x[2];
	int		i;


	if (*npk == 2) 
		po2 = 4294967295.;
	
	if (*npk == 4) 
		po2 = 65535.;


	/*
	** Error checks...
	*/
	if (*nrn < 2) {
		(void)fprintf(stderr, "packaf - array length .le. 0 - stop\n");
		exit(1);
	}

	if (*npk != 2 && *npk != 4) {
		(void)fprintf(stderr, "packaf - packing density .ne. 2  or 4 - stop\n");
		exit(1);
	}


	for(i = 0; i < 300; i++) {
		max_rcp  = (max_rcp / 10.0);
	}

	/*
	** find the max and min
	*/
	for(i = 0; i < *nrn; i++) {
		xmn = xmn < arn[i] ? xmn : arn[i];
		xmx = xmx > arn[i] ? xmx : arn[i];

	}


	/* 
	** make sure they are not equal.
	*/
	if ((double_abs((xmx-xmn))/po2) >  max_rcp) goto DONE;
	/*if (xmn != xmx) goto DONE;*/

	xmn = xmn - .5 * double_abs(xmn);
	xmx = xmx + .5 * double_abs(xmx);

	if ((double_abs((xmx-xmn))/po2) >  max_rcp) goto DONE;

	/*if (xmn != xmx) goto DONE;*/

	xmn = -1.0;
	xmx = 1.0;

	DONE:

	/*
	** if xmn and xmx differ in sign, further adjust them 
	** so that zero is a possible output from the unpacker.
	*/

	if (xmn * xmx > 0.0) goto DONE2;

	if (double_abs(xmn) < double_abs(xmx)) {
		xmn = xmx - xmx*po2/ (double) ((int) (xmx*po2/(xmx-xmn)));
	}
	else {
		xmx = xmn - xmn*po2/(double) ((int) (-xmn*po2/(xmx-xmn)));
	}

	DONE2:



	/*
	** Return the values to the user.
	*/
	*amn = xmn;
	*amx = xmx;

	/*
	** Store them
	*/
	apn[0] = xmn;
	apn[1] = po2/(xmx-xmn);


	/*
	** pack them
	*/
	if (*npk == 2) {
		pack2f(arn, nrn, apn);
	}
	else {
		pack4f(arn, nrn, apn);
		
	}

	/*
	** Convert the first two args to Cray floating point format.
	*/
	x[0] = apn[0];
	x[1] = apn[1];
	i = 2;
	(void)dptocf(x, apn, &i);

	return;


}


/*
** Function:	packbf()
**
** Description:
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void	packbf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	double		po2;
	double		xmn = arn[0];
	double		xmx = arn[0];
	double		x[2];
	int		i;

	if (*npk == 2) 
		po2 = 4294967295.;
	
	if (*npk == 4) 
		po2 = 65535.;

	/*
	** Error checks...
	*/
	if (*nrn < 2) {
		(void)fprintf(stderr, "packaf - array length .le. 0 - stop\n");
		exit(1);
	}

	if (*npk != 2 && *npk != 4) {
		(void)fprintf(stderr, "packaf - packing density .ne. 2  or 4 - stop\n");
		exit(1);
	}

	for(i = 0; i < *nrn; i++) {
		xmx = (xmx > arn[i]) ? xmx : arn[i];
	}

	xmn = -xmx;

	if (xmn != xmx) goto DONE;
	xmn = -1.0;
	xmx = 1.0;

	DONE:

	xmx = xmn - xmn*po2/(double) ((int) -xmn*po2/(xmx-xmn));

	*amn = xmn;
	*amx = xmx;

	apn[0] = xmn;
	apn[1] = po2/(xmx-xmn);


	if (*npk == 2) {
		pack2f(arn, nrn, apn);
	}
	else {
		pack4f(arn, nrn, apn);
	}

	x[0] = apn[0];
	x[1] = apn[1];
	i = 2;

	(void)dptocf(x, apn, &i);

	return;
}



/*
** Function:	packcf()
**
** Description:
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void	packcf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	double		po2;
	double		x[2];
	int		i;

	if (*npk == 2) 
		po2 = 4294967295.;
	
	if (*npk == 4) 
		po2 = 65535.;

	/*
	** Error checks...
	*/
	if (*nrn < 2) {
		(void)fprintf(stderr, "packaf - array length .le. 0 - stop\n");
		exit(1);
	}

	if (*npk != 2 && *npk != 4) {
		(void)fprintf(stderr, "packaf - packing density .ne. 2  or 4 - stop\n");
		exit(1);
	}

	apn[0] = *amn;
	apn[1] = po2/(*amx - *amn);

	if (*npk == 2) {
		pack2f(arn, nrn, apn);
	}
	else {
		pack4f(arn, nrn, apn);
	}

	x[0] = apn[0];
	x[1] = apn[1];
	i = 2;
	(void)dptocf(x,apn,&i);

	return;


}


/*
** Function:	unpk[abc]
**
** Description:
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/

void unpkaf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {

	unpack(arn, nrn, apn, npk, amn, amx);

	return;
}
void unpkbf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {

	unpack(arn, nrn, apn, npk, amn, amx);

	return;
}
void unpkcf(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {

	unpack(arn, nrn, apn, npk, amn, amx);

	return;
}


/*
**  Fortran entry points for Suns and SGIs...
*/
#if defined(ArchSun) || defined(ArchSGI) || defined(ArchLinux)

void packaf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	packaf(arn, nrn, apn, npk, amn, amx);
	return;
}

void packbf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	packbf(arn, nrn, apn, npk, amn, amx);
	return;
}

void packcf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	packcf(arn, nrn, apn, npk, amn, amx);
	return;
}

void unpkaf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	unpkaf(arn, nrn, apn, npk, amn, amx);
	return;
}

void unpkbf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	unpkbf(arn, nrn, apn, npk, amn, amx);
	return;
}

void unpkcf_(
	double		*arn,
	int		*nrn,
	double		*apn,
	int		*npk,
	double		*amn,
	double		*amx
) {
	
	unpkcf(arn, nrn, apn, npk, amn, amx);
	return;
}
#endif

/*
** 	Comments from the original Fortran version of packf.
**
**
** latest revision       January, 1994.  See "purpose" section
**                       immediately below, and "revision history"  
**                       comments at the end of these routines for 
**                       more details.
**
** purpose               This version of the packing/unpacking routines
**                       was written in Fortran 77, to be used on CRAY
**                       machines, including XMP, YMP, and CRAY-2 series.
**                       It is intended that this version will replace
**                       the mixed Fortran/Cal version that has been
**                       traditional on NCAR's XMP/YMP machines.
**
**                       This all-Fortran version produces slightly
**                       different results than the FORTRAN/CAL version
**                       unless rounding is turned off on the YMP compiler 
**                       cf77 (option"r").  This option is not available 
**                       on CRAY-2 machines, which is the reason that the 
**                       two sets of routines produce different results.
**
**                       Historically, the purpose of these routines is 
**                       to reduce the data volume and possibly the core 
**                       requirements associated with a user program by 
**                       packing/unpacking several values per Cray word.  
**                       See the "algorithm" section below for details.
**
** usage                 call packaf (arn,nrn,apn,npk,amn,amx)
**                       call packbf (arn,nrn,apn,npk,amn,amx)
**                       call packcf (arn,nrn,apn,npk,amn,amx)
**                       call unpkaf (arn,nrn,apn,npk,amn,amx)
**                       call unpkbf (arn,nrn,apn,npk,amn,amx)
**                       call unpkcf (arn,nrn,apn,npk,amn,amx)
**
** arguments             All of the packing and unpacking routines
**                       have the same calling sequence, with
**                       arguments arn, nrn, apn, npk, amn, and amx,
**                       the last two of which may be omitted
**                       except in calls to packcf.  The arguments
**                       may be described as follows:
**
** on input              arn(i),i=1,nrn
**
**                       an array of real numbers - input to the
**                       packers, output from the unpacker
**
**                       nrn     the length of arn - input
**
**                       apn(1)		minimum value - amn
**                       apn(2)		scale factor -
**					    (2**(64/npk)-1)/(amx-amn)
**
**
**                       npk
**
**                       packing density - number of data to be packed
**                       in one output word (2, 3, or 4) - input
**
**                       amn
**
**                       minimum - input to packcf, output from others
**                       (may be omitted from all except packcf calls)
**
**                       amx
**
**                       maximum - input to packcf, output from others
**                       (may be omitted from all except packcf calls)
**
** on output             apn(j) , j=3,4,...n
**
**                       the packed data - npk integers per word -
**                       n = 2+(nrn+npk-1)/npk - output from the
**                       packers, input to the unpacker
**
** special conditions    There are three packing routines to
**                       choose from - packaf, packbf, and packcf.
**                       They differ only in the way in which
**                       amn and amx are computed.
**
**                       Packaf is the safest, but the slowest.
**                       amn and amx are computed by the routine
**                       itself.  Initially, amn is the actual
**                       minimum and amx is the actual maximum.
**                       then, if amn = amx, the values are
**                       adjusted outwards to avoid division by zero.
**                       further, if amn amx differ in sign,
**                       amx is adjusted outwards a little to
**                       ensure that zeroes in the original array
**                       will be returned by the unpacker as zeroes
**                       (mathematically, at least - hardware
**                       round-off makes it impossible to ensure
**                       the return of exact zeroes without
**                       seriously degrading the speed of the
**                       routine - however, the value returned should
**                       never be greater than 1.e-14*abs(amn) ).
**
**                       Packbf is the intermediate choice, faster
**                       but not as safe. amn and amx are computed
**                       by the routine itself.
**                       Initially, amx is set equal to the maximum
**                       absolute value and amn to the negative
**                       of that.  Then, amn and amx are adjusted
**                       as for packaf.  This routine should probably
**                       be used only for data which is known to have
**                       a distribution which is roughly symmetric
**                       around zero.
**
**                       Packcf is the fastest, but most dangerous,
**                       of the three. the user must supply the
**                       initial values of amn and amx.  These values
**                       are not adjusted in any way.
**                       No test is made to ensure that data
**                       values lie between amn and amx - errors may
**                       result.
**
**                       Routines 'unpkaf', 'unpkbf', and 'unpkcf'
**                       are used for the unpacking process.
**                       Actually, they are all synonymns, provided
**                       for the sake of consistency if and when
**                       planned extensions are added to this package.
**
**                       Note that, in any case, these routines
**                       must be used with care.  One wildly
**                       out-of-range datum can destroy the precision
**                       of the rest.  Moreover, data sets with
**                       unlike ranges should be packed separately.
**
** i/o                   none
**
** precision             see algorithm discussion below
**
** required library      none
** files
**
** language              fortran
**
** history               1979: Written by Vince Wayland of Cray Research, 
**                       Inc, and made available to NCAR in the same year.
**			1991: (September) Re-written entirely in fortran
**                       for use on CRAY-2 machines, and also XMP/YMP
**                       machines if desired.  The two versions produce
**                       different results on the XMP/YMP series, as
**                       explained above in the "purpose" section.
**
** algorithm             Given an array of real numbers
**                       (arn(i), i=1,nrn), a packing density
**                       npk (with value 2, 3, or 4), and real numbers
**                       amn and amx (the former less than or equal
**                       to any datum in arn and the latter greater
**                       than or equal to any datum in arn), one can
**                       reduce each element of arn to a positive
**                       integer less than or equal to
**                       2**(64/npk)-1 by means of the
**                       following formula:
**
**                       int(i) = idint( (arn(i)-amn) *
**                       	 (2**(64/npk)-1) / (amx-amn) + .5 )
**
**                       the numbers amn and (2**(64/npk)-1) / (amx-amn)
**			may then be stored as apn(1) and apn(2), followed
**			by the integers (int(i),i=1,nrn), packed npk per
**			word in apn(3) through apn(2+(nrn+npk-1)/npk)).
**			The original (arn(i),i=1,nrn) may subsequently
**			be reconstructed from the contents of the packed
**			array apn by means of the following formula:
**
**                       arn(i) = amn + int(i) * (amx-amn) /
**                       	 (2**(64/npk)-1)
**
**			Note that the relative error of the reconstructed
**			values is a function of the packing density.
**			The error being 1. / (2**(64/npk)-1), i.e.
**			2.3 e-9, 4.8 e-7, 1.5 e-5 for npk 2, 3, 4
**			respectively.
**
** portability           not portable.
**
** timing                as of September, 1992 the following timings
**                       obtained on a Cray-YMP8/864.
**
**                       each time given is the time required
**                       to pack or unpack 100,000 numbers, in milliseconds.
**
**                       routine     npk=2      npk=3      npk=4
**                       -------     -----      -----      -----
**
**			packaf       4.97       5.03       5.11
**			packbf       3.60       3.67       3.76
**			packcf       2.06       2.12       2.21
**			unpkbf       1.79       1.46       1.30
**
**
*/
