
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

#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>

#define CRAY_PRIVATE

#include <cray.h>

/*
**  cosfile  - Emulates the cosfile command available on the 
**		Cray systems written by Tom Parker.
**
**  AUTHOR:	Peter W. Morreale,  SCD Consulting
**   DATE:	02Nov93
*/

#define True	1
#define False	0
typedef char Boolean;

static int	min  	= 999999999;
static int	dmin  	= 999999999;
static int	max  	= 0;
static int	dmax  	= 0;
static int	nrec 	= 0;
static int	lastnrec= 0;
static int	nfile 	= 1;
static int	prsize 	= 0;
static Boolean 	aflag 	= True;
static Boolean	verbose = False;
static int	binary 	= 0;
static int	ascii 	= 0;
static int	bfi 	= 0;
static int	fsize	= 0;
static int	dsize	= 0;

extern void	ParseFile();
extern void	PrintEOR();
extern void	PrintEOF();
extern void	PrintEOD();

#define RECORD_SIZE  3*1024000

static unsigned char	record[RECORD_SIZE]; 

#define VERSION "2.1"

extern void Usage();
extern void exit();

/*---------------------------------------------------------------------------*/
main(argc, argv)
int	argc;
char	*argv[];
{
	int		i;
	extern int	opterr;
	extern int	optind;

	opterr = 0;	/* be quiet! */

	while((i = getopt(argc, argv, "av")) != -1) {
	    switch (i) {
		case 'a':
		    aflag = False;
		    break;

		case 'v':
		    verbose = True;
		    break;

		default:
		    Usage();	/* no return */
	    }
	}

	if (optind >= argc) Usage(); 	/* no return */


	for(i = optind; i < argc; i++) {
	    ParseFile(argv[i]);
	}

	exit(0);
	return(0);	/* for lint */
}
/*----------------------------------------------------------------------*/
void Usage()
{
        (void)fputs(" Usage: cosfile   -a | -v  files\n\n",
                    stderr);
        (void)fprintf(stderr, " Version %s\n", VERSION);
        exit(0);

}

/*---------------------------------------------------------------------*/
void ParseFile(flnm)
char	*flnm;
{
	CrayFile	*cf;
	int		bytes;
	int		lastBytes;
	unsigned char	*p;

	

	if ((cf = _CrayOpen(flnm, O_RDONLY, 0600, 0)) == (CrayFile *) NULL) {
	   return;
	}

	(void)printf("\nProcessing dataset: %s\n", flnm);

	nrec = 0;

	while ((bytes = _CrayRead(cf, record, RECORD_SIZE)) != CRAY_EOD) {


	    if (bytes >= 0) {
	        fsize += bytes;
		min = (bytes < min) ? bytes : min;
		max = (bytes > max) ? bytes : max;
	        nrec++;
		if (verbose) PrintEOR(bytes);
		lastBytes = bytes;
	    	if (aflag) {
	 	    p = record;
		    while (p < &record[bytes]) {
		    	if (*p == 27) 
		            bfi++;
		    	else if (*p > 32 && *p < 127)
		    	    ascii++;
		    	else
			    binary++;

		    	p++;
	    	    }
	        }
	    } 
	    else if (bytes == CRAY_EOF) {
	    	if (verbose && lastnrec != nrec) 
			(void)printf("%9d%9d\n", nrec, lastBytes);
		PrintEOF();
		/*
		** Update 
		*/
		prsize = 0;
		nrec = 0;
		dsize += fsize;
		fsize = 0;
		nfile++;
		ascii = 0;
		binary = 0;
		bfi = 0;
		dmin = (min < dmin) ? min : dmin;
		dmax = (max > dmax) ? max : dmax;
		min = 99999999;
		max = 0;
	    }
	    else {
  		(void)fprintf(stderr, 
		    " Error on record %d.  Dataset probably not COS-blocked.\n",
		     nrec);
		_CrayClose(cf);
		return;
	    }

		

	}

	PrintEOD();
	return;
}

/*-------------------------------------------------------------------------*/
void PrintEOR(len)
int	len;
{

	/*
	** If this is the first record, write the header line
	*/
	if (nrec == 1) (void)printf("\n     Rec#    Bytes\n");

	/*
	** Only print out the record if the lengths are not the same
	*/
	if (prsize != len) {
	    	(void)printf("%9d%9d\n", nrec, len);
	    	prsize = len;
		lastnrec = nrec;
	}


	return;
}

/*-------------------------------------------------------------------------*/
void PrintEOF()
{


	(void)printf("  EOF %d: Recs=%d Min=%d Max=%d Avg=%d Bytes=%d\n",
		      nfile,
		      nrec,
		      min,
		      max,
		      fsize/nrec,
		      fsize);

	if (aflag) {
	    (void)printf("         Type=");
	    if (binary && !ascii) 
	        (void)printf("Binary\n");
	    else if (ascii && !binary) {
		if (bfi)
		  (void)printf("ASCII (with BFI) -- byte counts inaccurate!\n");
		else
		  (void)printf("ASCII (with no BFI)\n");
	    }
	    else if (ascii && binary) {
		/*
		** Determine the percentages of the bytes in the file
		*/
	        int 	bin, asc;
	        bin = (binary + bfi) * 100 / fsize;
	        asc = 100 - bin;
	        if (ascii && asc == 0) {
		   asc = 1;
		   bin = 99;
		}
		if (binary && bin == 0) {
		   bin = 1;
		   asc = 99;
		}
	        (void)printf( "Binary or mixed -- Binary=%3d%% ASCII=%3d%%\n",
			      bin, asc);
	    }
	}
	return;

}

/*-------------------------------------------------------------------------*/
void PrintEOD()
{
	(void) printf("  EOD. Min=%d Max=%d Bytes=%d\n", dmin, dmax, dsize);
}

