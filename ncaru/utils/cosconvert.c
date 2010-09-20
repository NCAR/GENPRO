
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
#include <signal.h>
#include <string.h>

#define CRAY_PRIVATE

#include <cray.h>

/*
**  cosfile  - Emulates the cosfile command available on the 
**		Cray systems written by Tom Parker.
**
**  AUTHOR:	Peter W. Morreale,  SCD Consulting
**   DATE:	02Nov93
*/

#define VERSION "2.1"

#define True	1
#define False	0
typedef char Boolean;

static FILE	*fp = (FILE *) NULL;
static char	oflnm[1024] = {'\0'};
static int	ocf;

static char	blanks[256] = {" "};

Boolean	bfi = False;
Boolean	ch = False;
Boolean	bi = False;
Boolean dobfi = False;

static int nrec = 0;

#define RECORD_SIZE	3*1024000

#define BFI_CHAR	27

static unsigned char	record[RECORD_SIZE];

extern void	exit();
extern void	Toast();
extern void	Usage();
extern void	ConvertFile();
extern void	RemoveBFI();

/*---------------------------------------------------------------------------*/
main(argc, argv)
int	argc;
char	*argv[];
{
	int		i;
	extern int	opterr;
	extern int	optind;

	opterr = 0;	/* be quiet! */


	(void)signal(SIGINT, Toast);

	while((i = getopt(argc, argv, "bcdx")) != -1) {
	    switch (i) {
		case 'b':
		    bi = True;
		    ch = bfi = dobfi = False;
		    break;

		case 'c':
		    bfi = True;
		    bi = ch = dobfi = False;
		    break;

		case 'd':
		    ch = True;
		    bi = ch = dobfi = False;
		    break;

		case 'x':
		    dobfi = True;
		    bi = ch = bfi = False;
		    break;

		default:
		    Usage();
	    }
	}

	if (!(bi || ch || bfi))  Usage(); /* no return*/

	if (optind >= argc)  Usage();	/* no return*/

	for(i = optind; i < argc; i++) {
	    if (dobfi) 
		RemoveBFI(argv[i]);
	    else
	        ConvertFile(argv[i]);
	}

	exit(0);
	return(0); /* for lint */
}

/*----------------------------------------------------------------------*/
void Usage()
{
	(void)fputs(" Usage: cosconvert   -b | -c | -d | -x   files\n\n",
		    stderr);
	(void)fprintf(stderr, " Version %s\n", VERSION);
	exit(0);

}
/*---------------------------------------------------------------------*/
void ConvertFile(flnm)
char	*flnm;
{
	int		cf;
	unsigned char	*rp;
	int		bytes, len;
	char		*p;


	cf = CrayOpen(flnm, O_RDONLY, 0600);
	if (cf < 0) {
	   (void)fprintf(stderr, "Unable to open file: %s\n", flnm);
	   return;
	}

	/*
	** Open a output file in the current directory.
	**
	** We need to do it here since we will rename the file after 
	** conversion
	*/
	(void)sprintf(oflnm, "%s-conv.%d", flnm, (int) getpid());
	if ((fp = fopen(oflnm, "w")) == (FILE *) NULL) {
	    (void)fprintf(stderr, 
			  "Unable to open temp file, %s unchanged\n", 
			  flnm);
	    CrayClose(cf);
	    return;
	}


	nrec = 1;

	while((bytes = CrayRead(cf, record, RECORD_SIZE)) >= 0) {

	    if (bfi) {
		rp = record;
		while(rp < &record[bytes]) {
		    if (*rp == BFI_CHAR) { /* bfi char */
		        len = *(++rp) - 30;
			(void)fwrite(blanks, 1, len, fp);
		    }
		    else
			(void)fwrite(rp, 1, 1, fp);
		    rp++;
		}
	    }
	    else {
		(void)fwrite(record, 1, bytes, fp);
	    }

	    if (ch || bfi) (void)fwrite("\n", 1, 1, fp);

	    nrec++;

	}

	(void)fclose(fp);
	fp = (FILE *) NULL;

	if (Terminate(bytes, cf, flnm)) {
	    CrayClose(cf);
	    (void)unlink(oflnm);
	    return;
	}

	CrayClose(cf);


	if (rename(oflnm, flnm)) {
	    (void)fprintf(stderr, 
			  " Unable to rename tmp file, %s unchanged\n",
			  flnm);
	}

	(void)unlink(oflnm);

	return;

}

/*----------------------------------------------------------------------*/
void RemoveBFI(flnm)
char	*flnm;
{
	int		cf;
	unsigned char	*p;
	int		bytes,  ier;
	char		*ptr;

	/*
	** Set a buffer for both the input and output cray files 
	*/

	/*
	** Open the input file 
	*/
	cf = CrayOpen(flnm, O_RDONLY, 0600, 0);
	if (cf < 0) {
	   (void)fprintf(stderr, "Unable to open file: %s\n", flnm);
	   return;
	}

	/*
	** Create a temp file name and open the temp file
	*/
	(void)sprintf(oflnm, "%s-conv.%d", flnm, (int)getpid());
	ocf = CrayOpen(oflnm, O_WRONLY | O_TRUNC | O_CREAT, 0600, 0);
	if (ocf < 0) {
	   (void)fprintf(stderr, "Unable to open temp file, %s unchanged\n", 
			 flnm);
	   CrayClose(cf);
	   return;
	}

	nrec = 1;

	while ((bytes = CrayRead(cf, record, RECORD_SIZE)) >= 0) {
	    p = record;
	    while (p < &record[bytes]) {

		/*
		** If this is a BFI character, then 
		**  write the number of blanks 
		*/
		if (*p == BFI_CHAR) 
	 	    ier = CrayWrite(ocf, (unsigned char *) blanks, 
								(*(++p) - 30));
		else 
		    ier = CrayWrite(ocf, p, 1);

		/*
		** Check the write
		*/
		if (ier < 0) {
			CrayClose(ocf);
			CrayClose(cf);
			(void)unlink(oflnm);
			(void)fprintf(stderr, 
				      "Unable to write record, %s unchanged\n",
				      flnm);
			return;
		}

		/*
		** Advance the record pointer 
		*/
		p++;

		/* 
		** Increment the record counter 
		*/
		nrec++;

	    }

	}

	CrayClose(ocf);

	if (Terminate(bytes, cf, flnm)) {
	    CrayClose(cf);
	    (void)unlink(oflnm);
	    return;
	}

	CrayClose(cf);


	if (rename(oflnm, flnm)) {
	    (void)fprintf(stderr, 
			  " Unable to rename tmp file, %s unchanged\n",
			  flnm);
	}

	(void)unlink(oflnm);

	return;


}
/*----------------------------------------------------------------------*/
void Toast()
{
	if (dobfi) {
	    if (ocf) (void)CrayClose(ocf);
	}
	else {
	    if (fp) (void)fclose(fp);
	}
	
	(void)unlink(oflnm);
	exit(0);
}

/*----------------------------------------------------------------------*/
int Terminate(flag, cf, flnm)
int		flag;
CrayFile	*cf;
char		*flnm;
{

	if (flag == CRAY_EOF) {
	    if ((flag = CrayRead(cf, record, RECORD_SIZE)) != CRAY_EOD) {
		(void) fprintf(stderr, 
			       " Dataset has > 1 file, %s unchanged\n",
		               flnm);
		return(1);
	    }
	    else {
		return(0);
	    }
	}

	switch (flag) {

	    case CRAY_OPENERR:
		(void)fprintf(stderr, 
			      " Error opening cray file, %s unchanged\n",
			      flnm);
		return(1);

	    case CRAY_BADCRAY:
	        (void)fprintf(stderr, 
		    " Error on record %d.  Dataset probably not COS-blocked.\n",
		    nrec+1);
		return(1);

	    case CRAY_EOD:
	        (void)fprintf(stderr,
			   " Unexpected EOD found at record %d, %s unchanged\n",
			   nrec+1,
			   flnm);
		return(1);

	    case CRAY_DISKERR:
		(void)fprintf(stderr, 
			      " Read error on record %d.  %s unchanged\n", 
			      nrec+1, flnm);
		return(1);
	
	}

	return(0);
}
	
