
/*
**      $Id$
*/
/************************************************************************
*									*
*			     Copyright (C)  1999			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		write_test.c
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Wed Apr 14 09:14:18 MDT 1999
**
**	Description:	test cos writing.
*/


/* Copyright 1999 University Corporation for Atmospheric Research (UCAR).
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
#include <stdlib.h>
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <string.h>
#include <errno.h>

#define CRAY_PRIVATE

#include <ncaru/cray.h>

/*
**  cossplit  - Emulates the cossplit command available on the 
**		Cray systems written by Tom Parker.
**
**  AUTHOR:	Peter W. Morreale,  SCD Consulting
**   DATE:	06Nov93
*/


static char	*record = NULL;

static int		record_size = 0;
static int		num_records = 0;
static int		block_size = 0;

/*----------------------------------------------------------------------*/
static
void Usage()
{
	(void)fputs(
		"Usage: write_test -b blk_size -n num_recs -r rec_size file\n",
		    stderr);
	(void)fputs("Where:\n", stderr);
	(void)fputs("\t-b\t	block size\n", stderr);
	(void)fputs("\t-n\t	number of records\n", stderr);
	(void)fputs("\t-r\t	record size\n", stderr);

	return;


}

/*---------------------------------------------------------------------*/
static
int WriteData(
	char	*flnm
) {
	CrayFile	*cf;
	int		i;
	int		ier;
	int		size = 0;


	if (!block_size) {
		fputs("Invalid or missing -b arg\n", stderr);
		Usage();
		return(-1);
	}
	else {
		(void)fprintf(stdout,"Block size set to %d blocks\n",
				block_size);
		(void)crayblocks(&block_size);
	}

	/*
	** check and preset the record to  '='
	*/
	if (!record_size) {
		fputs("Invalid or missing -r arg\n", stderr);
		Usage();
		return(-1);
	}
	else {
		size = record_size * CRAY_WORD_SIZE;
		record = malloc(size);
		(void)memset(record, '=',  size);
		(void)fprintf(stdout, "Record size is %d words\n",
				record_size);
	}



	/*
	** Open the file 
	*/
	cf = _CrayOpen(flnm, O_WRONLY | O_TRUNC | O_CREAT, 0660, 0);
	if (cf == (CrayFile *) NULL) {
	   	(void)fprintf(stderr, "Unable to open file: %s\n", flnm);
	   	return(-1);
	}


	(void) fprintf(stdout, "Writing %d records\n", num_records);

	for(i = 0; i < num_records; i++) {
		ier = _CrayWrite(cf, (unsigned char *) record, size);
		if (ier < 0) {
		   	(void)fprintf(stderr, "Unable to write record %d\n", i);
			return(-1);
		}
	}

	ier = _CrayClose(cf);
	if (ier < 0) {
		(void) fprintf(stderr, "Unable to close file: errno=%d\n",
				errno);
		return(-1);
	}

	return(0);


}

/*---------------------------------------------------------------------------*/
main(
	int	argc,
	char	**argv
) {
	int		i;
	extern int	opterr;
	extern int	optind;
	extern char	*optarg;

	opterr = 0;	/* be quiet! */


	while((i = getopt(argc, argv, "b:n:r:")) != -1) {
		switch (i) {

			case 'b':
		    		block_size = atoi(optarg);
		    		break;

			case 'n':
		    		num_records = atoi(optarg);
		    		break;

			case 'r':
				record_size = atoi(optarg);
		    		break;

			default:
		    		Usage();
				exit(1);
	    	}
	}

	if ((optind+1) - argc)  {
		Usage();
		exit(1);
	}

        i = WriteData(argv[optind]);

	if (i < 0) i = 1;

	return(i);	
}
