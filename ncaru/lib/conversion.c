
/*
**      $Id: conversion.c,v 1.7 2000/02/25 22:17:43 morreale Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  1999			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		conversion.c
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Wed Jul 21 10:27:06 MDT 1999
**
**	Description:	Contains the numeric conversion routines.
*/


/* Copyright 1994 University Corporation for Atmospheric Research (UCAR).
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

/* 
**
**  Written April 1987 by Chuck D'Ambra.
**  Last modified 15 August 1988 by CD.
**  Modified by Dan Anderson for rs6k 22 Feb 1992
**  Modified to unroll loops by PWM 14Nov93
*/

#define CBIAS		040000	/*  Cray f.p. exponent bias  */
#define DPMBIAS		1022	/*  IEEE f.p. exponent bias  */
#define MBIAS		126	/*  IEEE f.p. exponent bias  */
#define CSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */
#define LCDIF		sizeof(double) - sizeof(char)
#define LCSDIF		sizeof(long) - sizeof(char)


static 	unsigned char 		maxman = 0xff;
static	unsigned char		minman = 0;


/*----------------------------------------------------------------------*/
/*                                                                      */
/*       P R I V A T E   R O U T I N E S                                */
/*                                                                      */
/*----------------------------------------------------------------------*/

static
void checkrng(
	unsigned char 	sign, 
	unsigned char	*cp,
	unsigned char	*p[4]
) {
	
	/*
	** int >= 0; 1st 33 bits must be 0. 
	*/
	if (sign == 0)	{
	    if ((*(cp+4) & CSIGNMASK) != 0 || *cp != 0 || *(cp+1) != 0
	                                     || *(cp+2) != 0 || *(cp+3) != 0)
	        p[0] = p[1] = p[2] = p[3] = &maxman;
	}
	else {		
	    /*  
	    ** int < 0; 1st 33 bits must be 1. 
	    */
	    if ((*(cp+4) & CSIGNMASK) < 0200 || *cp != 0377 || *(cp+1) != 0377
	                               || *(cp+2) != 0377  ||  *(cp+3) != 0377)
	      p[0] = p[1] = p[2] = p[3] = &minman;
	}
}



/*----------------------------------------------------------------------*/
/*                                                                      */
/*       P U B L I C   R O U T I N E S                                  */
/*                                                                      */
/*----------------------------------------------------------------------*/



/*
** Function:	 	ctodpf()
**
** Description:		Converts from Cray to  64-bit IEEE floats.
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/

void ctodpf(
	void	*in_array,
	void	*out_array,
	int 	*n
) {
  	int 			i;
	int			joff;
  	unsigned char 		*p, sign, *manp[6];
	unsigned char		*in = (unsigned char *) in_array;
	unsigned char		*out = (unsigned char *) out_array;
  	long 			exp;


	/*  Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/

  	for (i = 0; i < *n; i++) {
    	    p = in+i*8;
    	    joff = i * 8;

    	    sign = *(p) & CSIGNMASK;	/*  sign bit  */

    	    /*
    	    ** Point to mantissa...
    	    */
    	    manp[0] = p+2;
    	    manp[1] = p+3;
    	    manp[2] = p+4;
    	    manp[3] = p+5;
    	    manp[4] = p+6;
    	    manp[5] = p+7;


    	    exp = (*(p+1)) | ((*p & 0x7f) << 8);
    	    if (exp != 0) exp = exp - CBIAS + DPMBIAS;

	    /*  
	    ** If input is outside range representable on IEEE, set to
    	    **  closest representable number.  
	    */
    	    if (exp > 2046) {
      		exp = 2046;
      	    	/*  Set all bits in mantissa  */
		manp[0] = &maxman;
		manp[1] = &maxman;
		manp[2] = &maxman;
		manp[3] = &maxman;
		manp[4] = &maxman;
		manp[5] = &maxman;
    	    }
    	    else if (exp <= 0) {
      	  	exp = 0;
      		/*  Turn off all bits in mantissa  */
		manp[0] = &minman;
		manp[1] = &minman;
		manp[2] = &minman;
		manp[3] = &minman;
		manp[4] = &minman;
		manp[5] = &minman;
    	    }

	    /*  Pack it into 64 bit IEEE float representation:
	    **	bits 0-51:	mantissa
	    **	bits 52-62:	exponent
	    **	bit 63:		sign
 	    */

#if defined(ArchLinux) || defined(ArchOSF1)
     	    /* 
	    ** sign+upper 7 bits of exponent 
	    */
     	    out[joff+7] = sign | (exp >> 4); 

    	    /* next joff+1 get lower 4 bits of exponent plus first 5 bits 
     	    ** of cray mantissa....but throw away left normalized bit because 
     	    ** its not in IEEE representation..so it leaves 3bits of mp[0] 
     	    ** for next byte
     	    **/

    	    out[joff+6]=  ((exp      & 0xf) <<4) | ((*manp[0] >>3) & 0xf);
    	    out[joff+5] = ((*manp[0] & 0x7) <<5) | ((*manp[1] & 0xf8) >>3);
    	    out[joff+4] = ((*manp[1] & 0x7) <<5) | ((*manp[2] & 0xf8) >>3);
    	    out[joff+3] = ((*manp[2] & 0x7) <<5) | ((*manp[3] & 0xf8) >>3);
    	    out[joff+2] = ((*manp[3] & 0x7) <<5) | ((*manp[4] & 0xf8) >>3);
    	    out[joff+1] = ((*manp[4] & 0x7) <<5) | ((*manp[5] & 0xf8) >>3);
    	    out[joff] = ((*manp[5] & 0x7) <<5);
#else

     	    /* 
	    ** sign+upper 7 bits of exponent 
	    */
     	    out[joff] = sign | (exp >> 4); 

    	    /* next joff+1 get lower 4 bits of exponent plus first 5 bits 
     	    ** of cray mantissa....but throw away left normalized bit because 
     	    ** its not in IEEE representation..so it leaves 3bits of mp[0] 
     	    ** for next byte
     	    **/

    	    out[joff+1]=  ((exp      & 0xf) <<4) | ((*manp[0] >>3) & 0xf);
    	    out[joff+2] = ((*manp[0] & 0x7) <<5) | ((*manp[1] & 0xf8) >>3);
    	    out[joff+3] = ((*manp[1] & 0x7) <<5) | ((*manp[2] & 0xf8) >>3);
    	    out[joff+4] = ((*manp[2] & 0x7) <<5) | ((*manp[3] & 0xf8) >>3);
    	    out[joff+5] = ((*manp[3] & 0x7) <<5) | ((*manp[4] & 0xf8) >>3);
    	    out[joff+6] = ((*manp[4] & 0x7) <<5) | ((*manp[5] & 0xf8) >>3);
    	    out[joff+7] = ((*manp[5] & 0x7) <<5);
#endif

	}


	return;
}






/*
** Function:		ctospf()
**
** Description:		Cray to 32bit IEEE floats..
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void ctospf(
	void	*in_array,
	void	*out_array,
	int 	*n
) {
  	int 		i;
	int		joff;
  	unsigned char 	*p, sign, *expp, *manp[3];
  	long 		exp;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;

	/*  
	** Cray 64 bit float representation:
 	**	bits 0-47:	mantissa
 	**	bits 48-62:	exponent
 	**	bit 63:		sign
 	*/

  	for (i = 0; i < *n; i++) {
    	    p = in+i*8;
    	    joff = i * 4;

    	    sign = *(p) & CSIGNMASK;	/*  sign bit  */

    	    /*
	    ** Point at mantissa
	    */
	    manp[0] = p+2;
	    manp[1] = p+3;
	    manp[2] = p+4;
	
	    /* 
	    ** Get the exponet
	    */
	    exp = (*(p+1)) | ((*p & 0x7f) << 8);
	    if (exp != 0) exp = exp - CBIAS + MBIAS;
	
	    /*  
	    ** If input is outside range representable on IEEE, set to
	    ** closest representable number.  
	    */
	    if (exp > 254) { /*  Too large  */
	        exp = 254;
	        /*  Set all bits in mantissa  */
		manp[0] = &maxman;
		manp[1] = &maxman;
		manp[2] = &maxman;
	    }
	    else if (exp < 0) {	/*  Too small  */
	        exp = 1;
	        /*  Turn off all bits in mantissa  */
		manp[0] = &minman;
		manp[1] = &minman;
		manp[2] = &minman;
	    }
	
	    /*  
	    ** Pack it into 32 bit IEEE float representation:
	    **	bits 0-22:	mantissa
	    **	bits 23-30:	exponent
	    **	bit 31:		sign
	    */

#if defined(ArchLinux) || defined(ArchOSF1)
	    expp = ((unsigned char *)(&exp));
	    out[joff+3] = sign | (*expp >> 1);
	    out[joff+2] = (*manp[0] & (~CSIGNMASK)) | ((*expp & 1) << 7);
	    out[joff+1] = *manp[1];
	    out[joff] = *manp[2];
#else
	    expp = ((unsigned char *)(&exp)) + LCSDIF;
	    out[joff] = sign | (*expp >> 1);
	    out[joff+1] = (*manp[0] & (~CSIGNMASK)) | ((*expp & 1) << 7);
	    out[joff+2] = *manp[1];
	    out[joff+3] = *manp[2];
#endif
	}

	return;
}


/*
** Function:		ctospi()
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
void ctospi(
	void	*in_array,
	void	*out_array,
	int 	*n,
	int 	*zpad
) {
	int 		i, joff;
	unsigned char 	*pin, sign;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;
	unsigned char 	*p[4];
	
	for (i = 0; i < *n; i++) {
	    pin = in+i*8;
	    joff = i * (*zpad+1)*4;
	
	    sign = *(pin) & CSIGNMASK;	/*  sign bit  */
	
	    p[0] = pin+4;	
	    p[1] = pin+5;	
	    p[2] = pin+6;	
	    p[3] = pin+7;	
	
	    checkrng(sign,pin,p);	/*  check if int w/i IEEE limits  */
	
	    /*  
	    ** Pack it into 4 byte IEEE integer representation  
	    */
	
#if defined(ArchLinux) || defined(ArchOSF1)
	    out[joff+3] = sign | (*p[0] & (~CSIGNMASK));
	    out[joff+2] = *p[1];
	    out[joff+1] = *p[2];
	    out[joff] = *p[3];
#else
	    out[joff] = sign | (*p[0] & (~CSIGNMASK));
	    out[joff+1] = *p[1];
	    out[joff+2] = *p[2];
	    out[joff+3] = *p[3];
#endif

	    /* 
	    ** 4 bytes of zeros if requested 
	    */
	    if (*zpad) {
	        out[joff+4]=out[joff+5]=out[joff+6]=out[joff+7]=0;
	    }
	}

	return;
}



/*
** Function:		dptocf()
**
** Description:		Convert 64 bit IEEE to Cray Floats.
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void dptocf(
	void	*in_array,
	void	*out_array,
	int 	*n
) {
	int 		i, joff;
	unsigned char 	*p, sign, *manp[7];
	long 		exp;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;
	unsigned char	and;
	
	/*  
	** Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/
	
	for (i = 0; i < *n; i++) {
	    p = in+i*8;
	    joff = i * 8;
	
#if defined(ArchLinux) || defined(ArchOSF1)
	    sign = *(p+7) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+6;	
	    manp[1] = p+5;	
	    manp[2] = p+4;	
	    manp[3] = p+3;	
	    manp[4] = p+2;	
	    manp[5] = p+1;	
	    manp[6] = p;	
	    exp = ((*(p+6))>>4) | ((*(p+7) & 0x7f) << 4);

#else
	    sign = *(p) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+1;	
	    manp[1] = p+2;	
	    manp[2] = p+3;	
	    manp[3] = p+4;	
	    manp[4] = p+5;	
	    manp[5] = p+6;	
	    manp[6] = p+7;	
	    exp = ((*(p+1))>>4) | ((*p & 0x7f) << 4);
#endif
	

	    /*
	    ** Get the exponet
	    */
	    if (exp != 0) exp = exp - DPMBIAS + CBIAS;
	
	    /*  
	    ** If input is outside range representable on IEEE, set to
	    ** closest representable number. 
	    */
	    and=0xff;
	    if (exp <= 0) {  /* too small */
	        exp = 0;
	        /*  
	        ** Turn off all bits in mantissa 
	        */
		manp[0] = &minman;
		manp[1] = &minman;
		manp[2] = &minman;
		manp[3] = &minman;
		manp[4] = &minman;
		manp[5] = &minman;
		manp[6] = &minman;
	        and=0x00;
	    }
	
	    /*  
	    ** Pack it into 64 bit CRAY float representation:
	    **	bits 0-47:	mantissa
	    **	bits 48-62:	exponent
	    **	bit 63:		sign
	    */
	
	    out[joff] = sign | (exp >> 8);  /* sign+upper 7 bits of exponent */

	    out[joff+1] = ((exp & 0xff));
	    out[joff+2] = (0x80 & and)|((*manp[0] & 0xf)<<3)|((*manp[1])>>5);
	    out[joff+3] = ((*manp[1] & 0x1f) <<3) | ((*manp[2] ) >>5);
	    out[joff+4] = ((*manp[2] & 0x1f) <<3) | ((*manp[3] ) >>5);
	    out[joff+5] = ((*manp[3] & 0x1f) <<3) | ((*manp[4] ) >>5);
	    out[joff+6] = ((*manp[4] & 0x1f) <<3) | ((*manp[5] ) >>5);
	    out[joff+7] = ((*manp[5] & 0x1f) <<3) | ((*manp[6] ) >>5);
	}

	return;
}
	
	
	


/*
** Function:		dptoci()
**
** Description:		64 bit IEEE int truncated to 46bits to Cray 46 bit int.
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void dptoci(
	void	*in_array,
	void	*out_array,
	int	 *n
) {
  	int 		i, joff;
  	unsigned char 	*pin, uBs;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;
	unsigned char	sign;

  	for (i = 0; i < *n; i++) {

    	    pin = in+i*8;
    	    joff = i * 8;

#if defined(ArchLinux) || defined(ArchOSF1)
	    out[joff] = *(pin+7) & CSIGNMASK;	/*  sign bit  */
#else
	    out[joff] = *(pin) & CSIGNMASK;	/*  sign bit  */
#endif
	    if (out[joff] != 0x00) { 
		out[joff] = 0xff; 
		out[joff+1]  = 0xff;
		sign = 0xc0;

	    } else {
		out[joff+1]  = 0x00;
		sign = 0;
	    }

#if defined(ArchLinux) || defined(ArchOSF1) 
    	    out[joff+2] = sign | *(pin+5) & 0x3f;
    	    out[joff+3] = *(pin+4);
    	    out[joff+4] = *(pin+3);
    	    out[joff+5] = *(pin+2);
    	    out[joff+6] = *(pin+1);
    	    out[joff+7] = *pin;
#else
    	    out[joff+2] = sign | *(pin+2) & 0x3f;
    	    out[joff+3] = *(pin+3);
    	    out[joff+4] = *(pin+4);
    	    out[joff+5] = *(pin+5);
    	    out[joff+6] = *(pin+6);
    	    out[joff+7] = *(pin+7);
#endif
  	}

	return;
}

/*
** Function:		dptoci64()
**
** Description:		64 bit int to 64 bit int.
**			Used both ways...
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void dptoci64(
	void	*in_array,
	void	*out_array,
	int	 *n
) {
  	int 		i, joff;
  	unsigned char 	*pin, uBs;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;

  	for (i = 0; i < *n; i++) {

    	    pin = in+i*8;
    	    joff = i * 8;

#if defined(ArchLinux) || defined(ArchOSF1) 
    	    out[joff+0] = *(pin+7);
    	    out[joff+1] = *(pin+6);
    	    out[joff+2] = *(pin+5);
    	    out[joff+3] = *(pin+4);
    	    out[joff+4] = *(pin+3);
    	    out[joff+5] = *(pin+2);
    	    out[joff+6] = *(pin+1);
    	    out[joff+7] = *pin;

#else
    	    out[joff+0] = *pin;
    	    out[joff+1] = *(pin+1);
    	    out[joff+2] = *(pin+2);
    	    out[joff+3] = *(pin+3);
    	    out[joff+4] = *(pin+4);
    	    out[joff+5] = *(pin+5);
    	    out[joff+6] = *(pin+6);
    	    out[joff+7] = *(pin+7);
#endif
  	}

	return;
}


	

/*
** Function:		sptocf()
**
** Description:		32bit IEEE to Cray float..
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void sptocf(
	void	*in_array,
	void	*out_array,
	int 		*n
) {
	int 		i, joff;
	unsigned char	*p, sign, Cb, *manp[3];
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;
	long 		exp;
	union {
		float	f;
		unsigned char c[4];
	} u;
	
	/*   32 bit IEEE float representation:
	**	bits 0-22:	mantissa
	**	bits 23-30:	exponent
	**	bit 31:		sign
	*/

	/*  Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/
	
	for (i = 0; i < *n; i++) {
	    p = in+i*4;
	    joff = i * 8;
	
#if defined(ArchLinux) || defined(ArchOSF1)
	    sign = *(p+3) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+2;	
	    manp[1] = p+1;
	    manp[2] = p+0;
	    exp = ((*(p+2) & 0x80)>>7) | ((*(p+3) & 0x7f) << 1);
#else
	    sign = *(p) & CSIGNMASK;	/*  sign bit  */
	
	    manp[0] = p+1;	
	    manp[1] = p+2;
	    manp[2] = p+3;
	
	    exp = ((*(p+1) & 0x80)>>7) | ((*p & 0x7f) << 1);
#endif

	    Cb=0x80;
	    if (exp != 0) exp = exp - MBIAS + CBIAS;
	
	    if (exp <= 0) {   /* too small */
	      	exp = 0;
	      	Cb=0x00;
	      	manp[0] = &minman;
	      	manp[1] = &minman;
	      	manp[2] = &minman;
	
	    }
	

	    out[joff]   = sign | (exp >> 8);
	    out[joff+1] = (exp & 0xff) ;
	    out[joff+2] = Cb |((*manp[0]) & 0x7f) ;
	    out[joff+3] = *manp[1];
	    out[joff+4] = *manp[2];
	    out[joff+5] = 0x00;
	    out[joff+6] = 0x00;
	    out[joff+7] = 0x00;
	}
}
	
	


/*
** Function:		sptoci()
**
** Description: 	32bit IEEE int to Cray int (either 46 or 64 bits).
**
** In Args:
**
** Out Args:
**
** Return Values:
**
** Side Effects:
*/
void sptoci(
	void	*in_array,
	void	*out_array,
	int 	*n,
	int	*zpad
) {
	int 		i, j;
	unsigned char 	*pin;
	unsigned char	*in = (unsigned char *) in_array;
	unsigned char	*out = (unsigned char *) out_array;
	
	for (i = 0; i < *n; i++) {
	
	    pin = in+(*zpad+1)*i*4;
	    j = i * 8;
	
	
#if defined(ArchLinux) || defined(ArchOSF1)
	    out[j] = *(pin+3) & CSIGNMASK;	/*  sign bit  */
#else
	    out[j] = *(pin) & CSIGNMASK;	/*  sign bit  */
#endif
	    if (out[j] != 0x00) { 
		out[j] = 0xff; 
		out[j+1]  = 0xff;
		out[j+2]  = 0xff;
		out[j+3]  = 0xff;

	    } else {
		out[j+1]  = 0x00;
		out[j+2]  = 0x00;
		out[j+3]  = 0x00;
	    }

#if defined(ArchLinux) || defined(ArchOSF1)
	    out[j+4] = *(pin+3);
	    out[j+5] = *(pin+2);
	    out[j+6] = *(pin+1);
	    out[j+7] = *pin;
#else
	    out[j+4] = *pin;
	    out[j+5] = *(pin+1);
	    out[j+6] = *(pin+2);
	    out[j+7] = *(pin+3);
#endif
	}
}



#if defined(ArchSun) || defined(ArchSGI) || defined(ArchLinux) || defined(ArchOSF1)

void sptocf_(
	void   *in,
	void   *out,
	int	*n
) {
        sptocf(in, out, n);
        return;
}

void sptoci_(
	void   *in,
	void   *out,
	int	*n,
	int	*zpad
) {
        sptoci(in, out, n, zpad);
        return;
}

void ctodpf_(
	void   *in,
	void   *out,
	int	*n
) {
        ctodpf(in, out, n);
        return;
}
void ctospf_(
	void   *in,
	void   *out,
	int	*n
) {
        ctospf(in, out, n);
        return;
}

void ctospi_(
	void   *in,
	void   *out,
	int	*n,
	int	*zpad
) {
        ctospi(in, out, n, zpad);
        return;
}

void dptocf_(
	void   *in,
	void   *out,
	int	*n
) {
	dptocf(in, out, n);
	return;
}

void dptoci_(
	void   *in,
	void   *out,
	int	*n
) {
        dptoci(in, out, n);
        return;
}

#endif

