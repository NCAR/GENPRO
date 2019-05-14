/*
**      File: 		main.c
**      Purpose:	routines for parsing a cos-blocked file.
**      Date:		14Nov94
**      Author:         Peter W. Morreale
**      Email:          morreale@ncar.ucar.edu
**      Organization:   NCAR/SCD, Boulder CO
**      Version:        1.0
**
**      (c) Copyright 1994 University Corporation for Atmospheric Research
**                         All Rights Reserved
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <memory.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#define CRAY_PRIVATE 1

#include <ncaru/cray.h>


#define BCW_F "BCW  fwi = %3d bn = %d\n"
#define EOR_F " EOR fwi = %3d ubc = %2d pri = %5d rn = %d words=%d\n" 
#define EOF_F " EOF fwi = %3d ubc = %2d pri = %5d rn = %d\n"
#define EOD_F "EOD  fwi = %3d ubc = %2d pri = %5d rn = %d\n"

struct bcw	zbcw;
struct rcw	zrcw;
int		wordcount = 0;

/*---------------------------------------------------------------------------*/
void ParseBlock(dataBlock)
char *dataBlock;
{
	union pcw	*cw;
	struct rcw	rcw;
	struct bcw	bcw;
	static int	rn = 0;
	static int	lastbn = -1;


	cw = (union pcw *) &dataBlock[0];


	while(cw <= (union pcw *) &dataBlock[CRAY_BLOCK_SIZE-1]) {

	    rcw = zrcw;
	    bcw = zbcw;
	    
	    switch (cw->u.m) {
		case M_BCW:
		    UNPACK_BCW(&cw->block, &bcw);
		    printf(BCW_F, bcw.fwi, bcw.bn);
		    if (bcw.bn != lastbn +1) {
			printf("   ****** invalid block number *****\n");

		    }
		    lastbn = bcw.bn;
		    wordcount += bcw.fwi;
		    cw += bcw.fwi + 1;
		    break;
		    
		case M_EOR:
		    UNPACK_RCW(&cw->record, &rcw);
		    printf(EOR_F, rcw.fwi, rcw.ubc, rcw.pri, rn, wordcount);
		    cw += rcw.fwi + 1;
		    wordcount = rcw.fwi;
		    rn++;
		    break;
		    
		case M_EOF:
		    UNPACK_RCW(&cw->record, &rcw);
		    printf(EOF_F, rcw.fwi, rcw.ubc, rcw.pri, rn);
		    cw += rcw.fwi + 1;
		    wordcount = rcw.fwi; 
		    rn=0;
		    break;
		case M_EOD:
		    UNPACK_RCW(&cw->record, &rcw);
		    printf(EOD_F, rcw.fwi, rcw.ubc, rcw.pri, rn);
		    rn++;
		    return;

		default: 
		    printf("Unknown control word, exiting\n");
		    exit(1);
	    }
	    
	}

	return;

}

/*---------------------------------------------------------------------------*/
main(argc, argv)
int argc; 
char *argv[];
{
	/*
	** Parse the control words in a cos blocked file.
	*/

	char		dataBuf[CRAY_BLOCK_SIZE]; 
	int		fd;
	int		bytes;
	int		rc = 0;

	if (argc != 2) {
	    printf("Usage: parse flnm\n");
	    exit(1);
	}

	fd = open(argv[1], O_RDONLY, 0644);
	if (fd == -1) {
	    printf("Unable to open \"%s\"\n", argv[1]);
	    exit(1);
	}

	while(1) {
	    (void)memset(dataBuf, '\0', CRAY_BLOCK_SIZE);
	    bytes = read(fd, dataBuf, CRAY_BLOCK_SIZE);
	    switch (bytes) {
		case CRAY_BLOCK_SIZE:
		    ParseBlock(dataBuf);
		    break;

		case -1: 
		    perror("Error Reading");
		    rc = 1;
		    break;
		
		case 0:
		    printf("Normal termination\n");
		    return(0);
		
		default:
		    printf("short read of %d bytes, exiting\n", bytes);
		    ParseBlock(dataBuf);
		    rc = 1;
		    break;
	    }

	    if (rc) break;

	}


	return(0);


}
