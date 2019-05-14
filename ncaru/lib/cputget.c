#define CRAY_PRIVATE
/* 
** Copyright 1994 University Corporation for Atmospheric Research (UCAR).
**	All rights reserved
** 
** Permission to use, copy, and modify this software and its documentation 
** for any non-commercial purpose is hereby granted without fee, provided 
** that the above copyright notice appear in all copies and that both that 
** copyright notice and this permission notice appear in supporting 
** documentation. UCAR makes no representations about the suitability of 
** this software for any purpose.   It is provided "as is" without express
** or implied warranty.
*/

#include <stdio.h>

#ifdef DEBUG
#include <stdio.h>
#endif
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include "cray.h"
#include <unistd.h>
#include <memory.h>
#include <malloc.h>

/* 
** Fortran & C  Cray "putwa" and "getwa" routines.
**
** HISTORY:
**	14Jun94  - PWM  Initial release, per Andy Mai's request.
*/



/*--------------------------------------------------------------------------*/
int craygetwa( cf, buf, loc, size, conv) 
CrayFile 	**cf;
char 		*buf; 
int		*loc;
int 		*size;
int		*conv;
{
	int 		errcode;
	int 		cwords;
	int 		zpad;

	/*
	** The file must be an unblocked file....
	*/
	if (!(*cf)->unblkFlag) return(CRAY_UNBLKERR);

	/* 
	** We only allocate the buffer once, however we do need to check
	** reallocating the buffer....  This buffer is released in
	** crayclose.
	*/
	if (*size*CRAY_WORD_SIZE  > (*cf)->cbufSize) {
	    if ((*cf)->cbuf) {
		(void)free((*cf)->cbuf);
		(*cf)->cbufSize = 0;
	    }

	    (*cf)->cbuf = malloc(*size*CRAY_WORD_SIZE);
	    if ((*cf)->cbuf  == (char *) NULL) return(0);
	    (*cf)->cbufSize = *size*CRAY_WORD_SIZE;
	}

	/*
	** Set the  memory to zeros...
	** We *have* to do this to prevent unused bits from 
	** changing word values.
	*/
	(*cf)->cbuf = memset((*cf)->cbuf, 0L, (*cf)->cbufSize);

	/*
	** Move the the word location pointed to by loc.
	** Then read in the data...
	*/
	if (lseek((*cf)->crayfd, (*loc-1)*CRAY_WORD_SIZE, SEEK_SET) == -1) 
	    return(-99);

	errcode = read((*cf)->crayfd, (*cf)->cbuf, *size*CRAY_WORD_SIZE);
	if (errcode != *size*CRAY_WORD_SIZE) return(CRAY_SHORTRD);


	cwords = errcode/CRAY_WORD_SIZE;

	switch(*conv) {
	    case 0: /* no conversion */
		 (void)memcpy(buf, (*cf)->cbuf, *size*CRAY_WORD_SIZE);
		 break;

   	    case 1: /* convert to single precision floats (32bit) */
      		ctospf((*cf)->cbuf,buf,&cwords);
      		break;

   	    case 2: /* convert to double precision floats (64bit) */
      		ctodpf((*cf)->cbuf,buf,&cwords);
      		break;

   	    case 3: /* convert to int (32bit) */
      		zpad=0;
      		ctospi((*cf)->cbuf,buf,&cwords,&zpad);
      		break;

   	    case 4: /* convert to "zero padded" (double length) ints */
      		zpad=1;
      		ctospi((*cf)->cbuf,buf,&cwords,&zpad);
      		break;
	}

	return(cwords);
}

/*--------------------------------------------------------------------------*/
int crayputwa( cf, buf, loc, size, conv)
CrayFile 	**cf;
unsigned char 	*buf; 
int		*loc;
int		*size;
int		*conv; 
{

	int 		rval;
	int 		zpad;

	/*
	** The file must be an unblocked file....
	*/
	if (!(*cf)->unblkFlag) return(CRAY_UNBLKERR);

	/* 
	** We only allocate the buffer once, however we do need to check
	** reallocating the buffer....  This buffer is released in
	** crayclose.
	*/
	if (*size*CRAY_WORD_SIZE  > (*cf)->cbufSize) {
	    if ((*cf)->cbuf) {
		(void)free((*cf)->cbuf);
		(*cf)->cbufSize = 0;
	    }

	    (*cf)->cbuf = malloc(*size*CRAY_WORD_SIZE);
	    if ((*cf)->cbuf  == (char *) NULL) return(0);
	    (*cf)->cbufSize = *size*CRAY_WORD_SIZE;
	}

	/*
	** Set the  memory to zeros...
	*/
	(*cf)->cbuf = memset((*cf)->cbuf, 0L, (long) (*cf)->cbufSize);

	/*
	** Move the the word location pointed to by loc.
	** Then read in the data...
	*/
	if (lseek((*cf)->crayfd, (*loc-1)*CRAY_WORD_SIZE, SEEK_SET) == -1) 
	    return(-99);

	/*
	** Convert as necesary...
	*/

   	switch (*conv) {
	    case 0: /* no convert... */
		(void)memcpy((*cf)->cbuf, buf, *size*CRAY_WORD_SIZE);
		break;

      	    case 1: /* convert IEEE float (32bit) to CRAY float */
	 	sptocf(buf,(*cf)->cbuf,size);
	 	break;

       	    case 2:  /* convert IEEE float (64bit) to CRAY float */
	 	dptocf(buf,(*cf)->cbuf,size);
	 	break;

      	    case 3:  /* 32 bit int to CRAY int */
	 	zpad=0;
	 	sptoci(buf,(*cf)->cbuf,size,&zpad);
	 	break;
		
      	    case 4:   /* 32bit double padded int to CRAY int */
	 	zpad=1;
	 	sptoci(buf,(*cf)->cbuf,size,&zpad);
	 	break;

      	    default:
	 	return(0);
   	}

	/*
	** Write the data....
	*/
	rval = write((*cf)->crayfd, (*cf)->cbuf, *size*CRAY_WORD_SIZE);
	if (rval != *size*CRAY_WORD_SIZE) return(CRAY_DISKERR);

	return(*size);

}

