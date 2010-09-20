
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
**  cossplit  - Emulates the cossplit command available on the 
**		Cray systems written by Tom Parker.
**
**  AUTHOR:	Peter W. Morreale,  SCD Consulting
**   DATE:	06Nov93
*/

#define VERSION "2.1"


static char	oflnm[128] = {'\0'};
static CrayFile	*ocf = (CrayFile *) NULL;

static char	prefix[256] = {'\0'};

static int nrec = 0;

static int maxFiles = 0;
static int fileNo   = 1;

#define RECORD_SIZE	3*1024000

static unsigned char	record[RECORD_SIZE];

extern void	Usage();
extern void	exit();
extern void	Toast();
extern void	SplitDataset();
extern CrayFile *OpenOutput();

/*---------------------------------------------------------------------------*/
main(argc, argv)
int	argc;
char	*argv[];
{
	int		i;
	extern int	opterr;
	extern int	optind;
	extern char	*optarg;

	opterr = 0;	/* be quiet! */


	(void)signal(SIGINT, Toast);

	while((i = getopt(argc, argv, "m:p:")) != -1) {
	    switch (i) {
		case 'm':
		    maxFiles = atoi(optarg);
		    if (maxFiles == 0) Usage();	/* no return */
		    break;

		case 'p':
		    (void)strcpy(prefix, optarg);
		    break;

		default:
		    Usage();
	    }
	}

	if ((optind+1) - argc)  Usage();	/* no return*/

        SplitDataset(argv[optind]);

	exit(0);
	return(0);	/* for lint */
}

/*----------------------------------------------------------------------*/
void Usage()
{
	(void)fputs(" Usage: cossplit  [-m maxFiles] [-p prefix] file",
		    stderr);
	(void)fprintf(stderr, " Version %s\n", VERSION);
	exit(0);

}
/*---------------------------------------------------------------------*/
void SplitDataset(flnm)
char	*flnm;
{
	CrayFile	*cf;
	int		bytes;
	int		ier;
	int		num_files = 0;


	cf = _CrayOpen(flnm, O_RDONLY, 0600, 0);
	if (cf == (CrayFile *) NULL) {
	   (void)fprintf(stderr, "Unable to open file: %s\n", flnm);
	   return;
	}

	ocf = OpenOutput();
	if (!ocf) {
	    _CrayClose(cf);
	    return;
	}


	nrec = 1;

	while((bytes = _CrayRead(cf, record, RECORD_SIZE)) != CRAY_EOD) {


	    if (bytes > 0) {
	       ier = _CrayWrite(ocf, record, bytes);
	       if (ier < 0) {
		   (void)fprintf(stderr, 
				 "Unable to write record %d of file %d to %s\n",
				 nrec, 
				 fileNo, 
				 oflnm);
		    (void)_CrayClose(cf);
		    return;
		}
	    }
	    else if (bytes == CRAY_EOF) {
		if (_CrayClose(ocf)) {
		    (void)fprintf(stderr, 
				  "Unable to close file: %s, removing...\n", 
				  oflnm);
		    (void)_CrayClose(cf);
		    (void)unlink(oflnm);
		    return;
		}
		num_files++;

		if (maxFiles && num_files >= maxFiles) return;

		ocf = OpenOutput();
		if (!ocf) {
		    (void)unlink(oflnm);
		    _CrayClose(cf);
		    return;
		}
	    }
	    else {	/* something's wrong! */
		(void)fprintf(stderr, 
			      "ERROR reading record %d of file %s\n", 
			      nrec, 
			      flnm);
		(void)_CrayClose(cf);
		(void)_CrayClose(ocf);
		ocf = (CrayFile *) NULL;
		return;
	    }

	    nrec++;


	}

	/*
	** We have a extra output file created, just unlink it...
	*/
	(void)unlink(oflnm);

	_CrayClose(cf);

	return;

}

/*----------------------------------------------------------------------*/
CrayFile *OpenOutput()
{
	CrayFile	*cf;

	if (*prefix) {
		(void)sprintf(oflnm, "%s.f%3.3d", prefix, fileNo);
	}
	else {
		(void)sprintf(oflnm, "f%3.3d", fileNo);
	}

	cf = _CrayOpen(oflnm, O_WRONLY | O_TRUNC | O_CREAT, 0660, 0);
	if (cf == (CrayFile *) NULL) {
	   (void)fprintf(stderr, "Unable to open temp file: %s\n", oflnm);
	   return(cf);
	}

	fileNo++;

	return(cf);
}
/*----------------------------------------------------------------------*/
void Toast()
{
	if (ocf) (void)_CrayClose(ocf);
	(void)unlink(oflnm);
	exit(0);
}
