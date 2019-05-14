
/*
**      $Id: cray.c,v 1.7 2000/02/25 22:17:44 morreale Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  2000			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
**	File:		cray.c
**
**	Author:		Peter Morreale
**			National Center for Atmospheric Research
**			PO 3000, Boulder, Colorado
**
**	Date:		Fri Feb 18 11:03:04 MST 2000
**
**	Description:	Implementation of the Cray COS I/O routines.
*/

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
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <memory.h>
#include <string.h>

#define CRAY_PRIVATE

#include "cray.h"
#include "conversion.h"

/* 
** Fortran & C  Cray blocking routines
**
** HISTORY:
**      Original Cray routines written by Craig Ruff.
**	Initial port to the RS6000 by Greg Woods. (added Fortran wrappers)
**
**      22Oct93  - PWM  Cleaned up the source, and added backspace 
** 		   and unblocked I/O capabilities.
**
**	14Nov96	 - PWM	Added arrays for holding the File Pointer.
*/

static union pcw	zcw;
extern int 		errno;

static int		BUF_BLK_SZ = 256;


typedef struct {
	CrayFile	*cf;
} CrayArray_T;

#define MAX_ARRAY_SIZE	256

static CrayArray_T	ca[MAX_ARRAY_SIZE] = {NULL};



/*#############################################################################

  CRAY dataset manipulation routines

  Originally written by Craig Ruff for MASnet. 
  Enhanced by Peter W. Morreale  

##############################################################################*/


/*------------------------------------------------------------------------*/
static 
int	convert_write_data(
	int		conversion_flag,
	void		*in,
	void		*out,
	int		num_words
) {
	int		zpad;


	switch(conversion_flag) {

   		case 1: /* convert to single precision floats (32bit) */
      			sptocf(in, out, &num_words);
      			break;

		case 2: /* convert to double precision floats (64bit) */
			dptocf(in, out, &num_words);
      			break;

		case 3: /* convert to int (32bit) */
			zpad=0;
			sptoci(in, out, &num_words, &zpad);
			break;

		case 4: /* convert to "zero padded" (double length) ints */
			zpad=1;
			sptoci(in, out, &num_words, &zpad);
			break;

		case 5: /* 64bit ints to 64bit ints */
			dptoci(in, out, &num_words);
			break;

		default:
			return(0);
	}

	return(num_words);

}

/*------------------------------------------------------------------------*/
static 
int	convert_read_data(
	int		conversion_flag,
	void		*in,
	void		*out,
	int		num_words
) {
	int		zpad;


	switch(conversion_flag) {

   		case 1: /* convert to single precision floats (32bit) */
      			ctospf(in, out, &num_words);
      			break;

		case 2: /* convert to double precision floats (64bit) */
			ctodpf(in, out, &num_words);
      			break;

		case 3: /* convert to int (32bit) */
			zpad=0;
			ctospi(in, out, &num_words, &zpad);
			break;

		case 4: /* convert to "zero padded" (double length) ints */
			zpad=1;
			ctospi(in, out, &num_words, &zpad);
			break;

		case 5: /* 64bit ints to 64bit ints */
			dptoci(in, out, &num_words);
			break;

		default:
			return(0);
	}

	return(num_words);

}
/*------------------------------------------------------------------------*/
static
int check_buf(
	CrayFile 	*cf,
	const int	size
) {
	/* 
	** We only allocate the buffer once, however we do need to check
	** reallocating the buffer....  This buffer is released in
	** crayclose.
	*/
	if (size > cf->cbufSize) {
		cf->cbuf = realloc(cf->cbuf, size);
		if (!cf->cbuf) return(-1);

		cf->cbufSize = size;
	}


	return(0);
}


/*------------------------------------------------------------------------*/
static 
CrayFile	*cf_locate(
	const int	index
) {

	/*
	** Return the cf struct pointer based on the index.
	*/
	if (index < 0 || index > (MAX_ARRAY_SIZE-1)) return(NULL);

	return(ca[index].cf);
}

/*------------------------------------------------------------------------*/
static 
void	cf_remove(
	CrayFile	*cf
) {
	int	i;

	/*
	** Free up this pointer array space...
	*/

	for(i = 0; i < MAX_ARRAY_SIZE; i++) {
		if (ca[i].cf && ca[i].cf == cf) {
			ca[i].cf = NULL;
			break;
		}
	}

	return;
}

/*--------------------------------------------------------------------------*/
static 
int readblock(
	CrayFile	*cf
) {
	struct bcw	bcw;
	int		n;


	/*
	** Read in a new buffer.
	*/
	n = read(cf->crayfd, cf->buffer, cf->numBlocks*CRAY_BLOCK_SIZE);
	if (n < 0) return(CRAY_DISKERR);

	/*
	** set the pointers accordingly.
	*/
	cf->cw = cf->start_buf;
	cf->end_block = cf->cw + CRAY_BLOCK_WORDS;

	/*
	** Unpack the BCW...
	*/
	if (cf->cw->u.m != M_BCW)  return(CRAY_BADCRAY);
	UNPACK_BCW(&cf->cw->block, &bcw);

	cf->blockn 	= bcw.bn;
	cf->fwi       	= bcw.fwi;

	return(0);
}


/*------------------------------------------------------------------------*/
static
int position(
	CrayFile 	*cf
) {
	/*
	** Position a file before the last EOF (blocked file) or 
	** at the end (unblocked file) for appending
	*/
	long		offset;
	int		rc;
	struct rcw	rcw;
	union pcw	*end_block;

	/*
	** Set the offset and seek to either the last block in the file,
	** or the end of the file (unblocked files).
	*/
	if (cf->unblkFlag == 1) 
	    offset = 0L;
	else
	    offset = -4096L;
	
	rc = lseek(cf->crayfd, offset, SEEK_END);
	if (rc < 0 || cf->unblkFlag == 1) return(CRAY_SEEK);

	/*
	** Read in the last block of the file.
	*/
	if ((rc = readblock(cf)) < 0) return(rc);

	end_block = cf->cw + 512;

	cf->cw++;

	/*
	** Traverse the block until we find the EOD
	*/
	while((cf->cw + cf->fwi) < end_block) {
	    cf->cw += cf->fwi;
	    UNPACK_RCW(&cf->cw->record, &rcw);

	    /*
	    ** If this is a EOD, break out of the loop
	    */
	    if (rcw.m == M_EOD)  break;

	    if (rcw.m != M_EOR && rcw.m != M_EOF) return(-1);
	    cf->fwi = rcw.fwi;
	    cf->cw++;
	}

	/*
	** Position before the EOF mark.
	*/
	cf->cw -= 2;

	return(0);
}
/*--------------------------------------------------------------------------*/
static
void write_bcw(
	CrayFile	*cf
) {

	struct bcw	bcw;

	cf->blockn++;

	*cf->cw = zcw;

	bcw.bdf = 0;
	bcw.fwi = 0;
	bcw.m = M_BCW;
	bcw.bn = cf->blockn;

	PACK_BCW(&bcw, &cf->cw->block);
	cf->pfi++;
	cf->pri++;

	return;
	
}

/*--------------------------------------------------------------------------*/
static
int	truncfile(
	CrayFile	*cf
) {
	off_t		len;

	/*
	** Truncate the file at it's current offset.
	** assumes that the library buffer has been flushed.
	*/

	len = lseek(cf->crayfd, 0, SEEK_CUR);
	if (len < 0) return(CRAY_SEEK);

	if (ftruncate(cf->crayfd, len)) return(CRAY_DISKERR);

	return(0);
}

/*--------------------------------------------------------------------------*/
static 
int flushblock(
	CrayFile	*cf
) {
	int	len;


	/*
	** Now write only those blocks which are "dirty"
	*/
	len = (cf->end_block - cf->start_buf) * CRAY_WORD_SIZE;

	if (write(cf->crayfd, cf->buffer, len) != len) {
		return(CRAY_DISKERR);
	}

	cf->cw 		= cf->start_buf;
	*cf->cw 	= zcw;

	return(0);
}

/*--------------------------------------------------------------------------*/
static
void cray_init(
	CrayFile	*cf
) {

	cf->end_buf	= (union pcw *)&cf->buffer[cf->numBlocks*CRAY_BLOCK_SIZE];
	cf->start_buf	= (union pcw *) cf->buffer;
	cf->end_block	= (union pcw *) &cf->buffer[CRAY_BLOCK_SIZE];
	cf->blockn 	= 0;
	cf->reod 	= 0;
	cf->weod 	= 0;
	cf->weof 	= 0;
	cf->pri 	= 0;
	cf->pfi 	= 0;

	if (cf->oflags == O_RDONLY) {
		cf->cw = cf->end_buf;

	} 
	else {
		cf->cw 		= cf->start_buf;
		*cf->cw 	= zcw;
		cf->cw->u.m 	= M_BCW;
		SET_PCW_FWI(cf->cw, 0);
	}

	return;
}

/*--------------------------------------------------------------------------*/
static
CrayFile *cray_open(
	const char	*name,
	const int	flag,
	const int	mode,
	const int	unblkFlag
) {
	CrayFile	*cf = malloc(sizeof(CrayFile));
	int		buf_size;
	char		*p;


	if (!cf) {
#ifdef DEBUG
                printf("Cannot open %s, errno %d\n",name,errno);
		perror(name);
#endif 
		return(NULL);
	}
	(void)memset((char *) cf, '\0', sizeof(CrayFile));

	if ((cf->crayfd = open(name, flag, mode)) < 0) {
#ifdef DEBUG
                printf("Cannot open %s, errno %d\n",name,errno);
		perror(name);
#endif 
		return(NULL);
	}

	cf->oflags 	= flag;
	cf->unblkFlag 	= unblkFlag;
	cf->last_op 	= C_INIT;

	/*
	** Set the block size for this blocked buffer.
	*/
	if (p = getenv("CRAYBLOCKS")) {
  	    cf->numBlocks = atoi(p);
	}
	else {
	    cf->numBlocks = BUF_BLK_SZ;
	}

	buf_size = cf->numBlocks * CRAY_BLOCK_SIZE;
	    

	/*
	** Only allocate a buffer if the open is for blocked files...
	*/
	if (unblkFlag == 0) {
	    cf->buffer = malloc(buf_size);
	    if (cf->buffer == NULL) {
	        (void)free(cf);
	        return(NULL);
	    }
	}


	cray_init(cf);


	return(cf);
}

/*--------------------------------------------------------------------------*/
static
int cray_read(
	CrayFile	*cf,
	unsigned char	*buf,
	const int	bytes
) {
	int		len;
	int		size = bytes;
	int		n;
	unsigned char	*p =  buf;
	struct rcw	rcw;
	struct bcw	bcw;
	int		bytes_read = 0;



	/*
	** If this is an unblocked read, then we can ignore 
	** all the blocked I/O settings and merely read to fill 
	** in the given buffer.
	*/
	if (cf->unblkFlag == 1) {
	   /*
	   ** Read must a multiple of 4096 (512 word blocks...)
	   */
	   if ((size/CRAY_WORD_SIZE)%512) return(CRAY_UNBLKERR);

	   if ((len = read(cf->crayfd, buf, size))!=size) return(CRAY_UNBLKERR);
	   cf->last_op = C_READ;
	   return(len);
	}



	/*
	** If we are here, then this is a blocked read....
	** Check to make sure we have not reached the EOD
	*/
	if (cf->reod)  return(CRAY_EOD);

	
	/*
	**  Read in a library buffer if necessary.
	*/
	if (cf->last_op == C_INIT || cf->last_op == C_REWND) {
	    if ((n = readblock(cf)) != 0) return(n);
	    cf->cw++;
	}

	/*
	** Now read until the end of the next record.
	*/

	for(;;) {

	    /*
	    ** Copy the data requested...
	    */
	    len = cf->fwi*CRAY_WORD_SIZE;
	    len = len > size ? size : len;
	    if (size > 0 && len > 0) {
		    memcpy(p, (char *) cf->cw, len);
		    bytes_read += len;
		    p += len;
		    size -= len;
	    }


	    /*
	    ** Move forward in the library buffer.  If necessary
	    ** read in a new block.
	    */
	    cf->cw += cf->fwi;
	    if (cf->cw >= cf->end_buf) {
	            if ((n = readblock(cf)) != 0) return(n);
	    }


	    /*
	    ** Parse the control word.
	    */
	    switch (cf->cw->u.m) {
		case M_BCW:
		    UNPACK_BCW(&cf->cw->block, &bcw);
		    cf->blockn = bcw.bn;
		    cf->fwi = bcw.fwi;
		    cf->cw++;
		    cf->end_block += CRAY_BLOCK_WORDS;
		    break;

		case M_EOR:
		    UNPACK_RCW(&cf->cw->record, &rcw);
		    cf->fwi = rcw.fwi;
		    cf->pri = rcw.pri;
		    goto DONE;

		case M_EOF:
		    UNPACK_RCW(&cf->cw->record, &rcw);
		    cf->fwi = rcw.fwi;
		    cf->cw++;
		    return(CRAY_EOF);

		case M_EOD:
		    cf->reod = 1;
		    return(CRAY_EOD);

		default:
		    return(CRAY_BADCRAY);
	    }
	}

	DONE:

	p -= rcw.ubc / 8;
	cf->last_op = C_READ;  /* set the last operation flag */
	cf->cw++;
	return(bytes_read);
}


/*--------------------------------------------------------------------------*/
static
int cray_write(
	CrayFile	*cf,
	unsigned char	*buffer,
	const int	bytes_to_write
) {
	int		bytes;
	int		size = bytes_to_write;
	int		left;
	int		len;
	int		ubc;
	unsigned char	*buf =  buffer;
	int		n;
	struct rcw	rcw;


	/*
	** If this is an unblocked write, then we can ignore 
	** all the blocked I/O settings and merely write the data
	*/
	if (cf->unblkFlag == 1) {
	   /*
	   ** Writes must a multiple of 4096 (512 word blocks...)
	   */
	   if ((size%CRAY_BLOCK_SIZE)) return(CRAY_UNBLKERR);

	   if ((len = write(cf->crayfd,buf,size))!=size) return(CRAY_DISKERR);

	   cf->last_op = C_WRITE;
	   return(0);
	}

	/*
	** If we are here, then this is a blocked write....
	*/
	len 		= (size + CRAY_WORD_SIZE - 1) & ~(CRAY_WORD_SIZE - 1);
	ubc 		= (len - size) * CRAY_WORD_SIZE;
	len 		/= CRAY_WORD_SIZE;
	cf->weof 	= 0;
	cf->pri 	= 0;

	/*
	** reset some pointers based on the last operation 
	*/
	if (cf->last_op == C_READ || cf->last_op == C_BCKSP) 
	    cf->cw--; 			/* move back to the RCW */
	else if (cf->last_op == C_REWND)
	    cray_init(cf);	/* re-init for writing */

	
	do {
		/*
		** Determine the length of the data we can write to 
		** the buffer, leave room for a BCW...
		*/
		cf->fwi = len;
		left = cf->end_block - cf->cw - 1;
		if (len > left)
			cf->fwi = left;

		SET_PCW_FWI(cf->cw, cf->fwi);
		cf->cw++;


		/*
		** Copy data to next BCW...
		*/
		bytes = cf->fwi * CRAY_WORD_SIZE;
		if (bytes > 0) {
			memcpy((char *) cf->cw, buf, bytes);
			cf->cw += cf->fwi;
			len -= cf->fwi;
			buf += bytes;
			size -= bytes;
		}


		/* 
		** If out of room in the buffer, flush it.  
		**
		*/
		if (cf->cw >= cf->end_buf) {
			if ((n = flushblock(cf)) < 0) return(n);
			write_bcw(cf);
			cf->end_block = cf->cw + CRAY_BLOCK_WORDS;
		}


		/*
		** Do we need a bcw?
		*/
		if (cf->cw >= cf->end_block) {
			write_bcw(cf);
			cf->end_block += CRAY_BLOCK_WORDS;

		    	/*
                    	** If we have nothing more to write in the buffer,
                    	** we need to increment the current word pointer so
                    	** that the EOR doesn't overwrite this BCW.
                    	** Added: 16Nov94 PWM
                    	*/
                    	if (size <= 0) cf->cw++;

		}


	} while (size > 0);

	if (cf->cw == cf->start_buf) {
		SET_PCW_FWI(cf->cw, 0);
		cf->cw++;

	} else {
		/*
		 * Zero the unused bits in the end of a record.
		 * Bug: if we just called flushblock, the place
		 *	we should have zeroed was just written
		 *	to disk!  Thus, we shouldn't do it again
		 *	now or we'll over write something before
		 *	the buffer in memory.  That's why we're
		 *	in the else branch.
		 */
		buf = (unsigned char *) cf->cw;
		len = ubc / 8;
		while (--len >= 0)
			*--buf = '\0';
	}

	*cf->cw 	= zcw;
	rcw.tran 	= 0;
	rcw.bdf 	= 0;
	rcw.srs 	= 0;
	rcw.fwi 	= 0;
	rcw.m 		= M_EOR;
	rcw.ubc 	= (unsigned char) ubc;
	rcw.pri 	= cf->pri;
	rcw.pfi 	= cf->pfi;
	PACK_RCW(&rcw, &cf->cw->record);

	cf->pri 	= 0;
	cf->last_op 	= C_WRITE;

	cf->weof 	= 0;
	cf->weod 	= 0; 

	return(0);
}

/*--------------------------------------------------------------------------*/ 
static
int cray_write_eof(
	CrayFile	*cf
) {
	struct rcw	rcw;
	int		n;

	/*
	** If this is an unblocked file, we can't do this.
	*/
	if (cf->unblkFlag == 1) return(CRAY_UNBLKERR);

	/*
	** If we already wrote this, don't do it again.
	*/
	if (cf->weof) return(0);

	SET_PCW_FWI(cf->cw, 0);

	cf->cw++;

	if (cf->cw >= cf->end_buf) {
		if ((n = flushblock(cf)) < 0) return(n);
		cf->end_block = cf->cw + CRAY_BLOCK_WORDS;

		write_bcw(cf);

		SET_PCW_FWI(cf->cw, 0);
		cf->cw++;
	}

	*cf->cw = zcw;

	rcw.tran 	= 0;
	rcw.bdf 	= 0;
	rcw.srs 	= 0;
	rcw.fwi 	= 0;
	rcw.m 		= M_EOF;
	rcw.ubc 	= 0;
	rcw.pri 	= 0;
	rcw.pfi 	= cf->pfi;
	PACK_RCW(&rcw, &cf->cw->record);

	cf->pfi = 0;
	cf->weof = 1;
	cf->last_op = C_WRITE;

	return(0);
}

/*--------------------------------------------------------------------------*/
static
int cray_write_eod(
	CrayFile	*cf
) {
	struct rcw	rcw;
	int		n;

	/*
	** If this is an unblocked file, we can't do this.
	*/
	if (cf->unblkFlag == 1) return(CRAY_UNBLKERR);


	/*
	** If we already wrote this, don't do it again...
	*/
	if (cf->weod) return(0);


	/*
	** Add a EOF if we do not already have one.
	*/
	if (!cf->weof) {
		if ((n = cray_write_eof(cf)) < 0) return(n);
	}

	SET_PCW_FWI(cf->cw, 0);

	cf->cw++;

	if (cf->cw >= cf->end_buf) {
		if ((n = flushblock(cf)) < 0) return(n);

		write_bcw(cf);
		cf->end_block = cf->cw + CRAY_BLOCK_WORDS;
		SET_PCW_FWI(cf->cw, 0);
		cf->cw++;
	}

	*cf->cw = zcw;
	rcw.ubc = 0;
	rcw.tran = 0;
	rcw.bdf = 0;
	rcw.srs = 0;
	rcw.fwi = 0;
	rcw.pri = 0;
	rcw.pfi = 0;
	rcw.m = M_EOD;
	PACK_RCW(&rcw, &cf->cw->record);

	cf->weod = 1;
	cf->last_op = C_WRITE;

	cf->cw++;
	if (cf->cw >= cf->end_block) cf->end_block += CRAY_BLOCK_WORDS;

	return(0);
}

/*--------------------------------------------------------------------------*/
static
int cray_close(
	CrayFile	*cf
) {
	int	n = 0;

	/*
	** We only perform the control word and block flushing if 
	** this is a blocked file....
	*/
	if (cf->unblkFlag == 0)  {
	    switch (cf->last_op) {

		case C_INIT:    /* no-op */
		case C_REWND:
		case C_READ:
		    	break;

		case C_WRITE:
	            	if (!cf->weod) {
		    		if ((n = cray_write_eod(cf))) return(n);
			}

	            	if (cf->cw > cf->start_buf) {
		        	if (flushblock(cf))  return(CRAY_DISKERR);
				write_bcw(cf);
			}
		    	break;
		    
		case C_BCKSP:
		    	/*
		    	** Don't write an EOF and EOD here, 
			** _CrayBack already did.
		    	*/
	            	if (cf->cw > cf->start_buf) {
		        	if (flushblock(cf))  return(CRAY_DISKERR);
			}
		    	break;
	    }
    	}

	(void)close(cf->crayfd);

	/*
	** Free the I/O buffer and the conversion buffer
	*/
	if (cf->buffer) (void)free(cf->buffer);
	if (cf->cbuf) (void)free(cf->cbuf);

	cf_remove(cf);

	(void)free(cf);

	return(n);
}


/*--------------------------------------------------------------------------*/
static
int cray_back(
	CrayFile	*cf
) {
	long		offset;
	union  pcw	*endrec;
	struct rcw	rcw;
	struct bcw	bcw;
	int		bufblockn;
	int		blockn;
	int		pri;
	int		rc;
	int		flushed;


        UNPACK_BCW(&cf->start_buf->block, &bcw);

	endrec 		= cf->cw;
	bufblockn 	= bcw.bn;
	blockn 		= cf->blockn;
	flushed 	= 0;
	pri		= 0;


	switch(cf->last_op) {
	    case C_INIT:	/* just return... no-op yet */
	    case C_REWND:
		return(0);
	
	    case C_READ:
		if (cf->oflags & O_RDONLY) return(CRAY_RDONLY);
		pri = cf->pri;
		endrec -= 2;
		break;

	    case C_BCKSP:
		endrec++;
		UNPACK_RCW(&endrec->record, &rcw);
		pri = rcw.pri;

		/*
		**  If pri == 0, then  the end of this record is in this
		**  block, set endrec to point to the end of the last word
		**  in the record  here.  If we have a pri, then we set the
		**  endrec below.
		*/
		if (pri == 0) endrec--;  
		break;
	
	    case C_WRITE:
		/*
		** If the file was opened O_WRONLY, then this is 
		** an illegal operation.  The reason is that we will 
		** probably have to use readblock() and that will fail.
		*/
		if (cf->oflags & O_WRONLY) return(CRAY_WRONLY);

		/*
		** Unpack the RCW to get the pri.
		*/
		if (cf->cw->u.m != M_EOR)  return(CRAY_BADCRAY);
		UNPACK_RCW(&cf->cw->record, &rcw);
		pri = rcw.pri;

		endrec--;  

		/*
		** If there isn't enough room in the buffer for the 
		** following EOF and EOF, then we'll flush the buffer.
		** Set flushed here so that we will re-read the block.
		*/
		if (cf->end_buf - cf->cw < 2) flushed = 1;

                /*
                ** Write a eof and eod
                */

                if ((rc = cray_write_eod(cf)) < 0)  return(rc);

                /*
                ** flush if necessary
                */
                if (flushed) {
			rc = flushblock(cf);
			rc = truncfile(cf);
                }

		break;
	
	}

	/*
	** Determine whether 
	** we need to read in a new lib buffer.
	*/
	if (blockn - pri < bufblockn || flushed) {  

	    /* 
	    **  Yup, the starting block for this record is not 
	    **  in the current buffer, or, we previously did a 
	    **  write and we had the flush the buffer as per above.
	    **  Re-read a new buffer 
	    */

	    offset = (blockn - pri) * CRAY_BLOCK_SIZE;
	    if (lseek(cf->crayfd, offset, SEEK_SET) < 0) return(CRAY_SEEK);
	    if ((rc = readblock(cf)) != 0) return(rc);
	    cf->cw++;

	    /*
	    ** Reset endrec only if this buffer was not flushed 
	    */

	    if (!flushed) endrec = cf->end_block;
	}
	else { 	

	    /* 
	    ** nope, the starting block is in this buffer.
	    ** Just set the pointers accordingly
	    */

	    cf->cw = cf->start_buf + ((blockn-pri)-bufblockn)*CRAY_BLOCK_WORDS;
	    cf->end_block = cf->cw + CRAY_BLOCK_WORDS;

	    /*
	    ** If we have a pri, then set endrec to the end of the block.
	    */
	    if (pri) endrec = cf->end_block;

	    /*
	    ** This must be a BCW now.
	    */
	    if (cf->cw->u.m != M_BCW)  return(CRAY_BADCRAY);

            UNPACK_BCW(&cf->cw->block, &bcw);
	    cf->blockn = bcw.bn;
	    cf->fwi = bcw.fwi;
	    cf->cw++;
	}

	/*
	** Traverse the block to find the last control word.
	** This is the start of the previous record
	*/
	while((cf->cw + cf->fwi) < endrec) {
	    cf->cw += cf->fwi;
	    UNPACK_RCW(&cf->cw->record, &rcw);
	    if (rcw.m != M_EOR && rcw.m != M_EOF) return(-1);
	    cf->fwi = rcw.fwi;
	    cf->pri = rcw.pri;
	    cf->pfi = rcw.pfi;
	    cf->cw++;
	}

	cf->reod = 0;
	cf->last_op = C_BCKSP;

	return(0);
	    

}
/*--------------------------------------------------------------------------*/
static
int cray_rewind(
	CrayFile 	*cf
) {
	int	rc;


	switch(cf->last_op) {
	    case C_INIT:	/* just return... */
	    case C_REWND:
		return(0);
	
	    case C_READ:
	    case C_BCKSP:
		break;
	
	    case C_WRITE:
		/*
		** Write a eof and eod
		*/
		if (cf->weod == 0) {
		    if ((rc = cray_write_eod(cf)) < 0)  return(rc);
		}

		/*
		** flush if necessary
		*/
		if ((rc = flushblock(cf)) < 0) return(rc);
		break;
	
	}

	/*
	** seek to the beginning.
	*/
	if (lseek(cf->crayfd, 0L, SEEK_SET)) return(CRAY_SEEK);

	/*
	** Re-initialize the cf struct.
	*/
	cray_init(cf);
	cf->last_op = C_REWND;

	return(0);
}

/*--------------------------------------------------------------------------*/
static
long cray_seek(
	CrayFile	*cf,
	const int	word_off,
	const int	pos
) {
	int	whence;
	off_t	offset = (off_t) (word_off*CRAY_WORD_SIZE);
	
	/*
	** Seek to a point in an unblocked file only.
	*/
	if (!cf->unblkFlag) return(CRAY_UNBLKERR);

	switch(pos) {
		
		case 0:
			whence = SEEK_SET;
			break;
		case 1:
			whence = SEEK_CUR;
			break;
		case 2:
			whence = SEEK_END;
			break;
		
		default:
			return(CRAY_SEEK);
	}

	return((long) lseek(cf->crayfd, offset, whence));


}

#ifdef DEBUG
/*-----------------------------------------------------------------------*/
void _CrayCfDump(
	CrayFile	*cf
) {
	/*
	** For debugging purposes only 
	*/

	(void)printf("Dump of cf struct:\n\n");
	(void)printf("\tunblkFlag = %d\n", cf->unblkFlag);
	(void)printf("\tlast_op   = ");
	switch(cf->last_op) {
	    case C_INIT:
		(void)printf("C_INIT\n");
		break;
	    case C_READ:
		(void)printf("C_READ\n");
		break;
	    case C_WRITE:
		(void)printf("C_WRITE\n");
		break;
	    case C_BCKSP:
		(void)printf("C_BCKSP\n");
		break;
	    case C_REWND:
		(void)printf("C_REWND\n");
		break;

	    default:
		(void)printf("*** UNKNOWN ***\n");
		break;
	}
	(void)printf("\tblockn    = %d\n", cf->blockn);
	(void)printf("\tcrayfd    = %d\n", cf->crayfd);
	(void)printf("\treod      = %d\n", cf->reod);
	(void)printf("\tweod      = %d\n", cf->weod);
	(void)printf("\tweof      = %d\n", cf->weof);
	(void)printf("\tfwi       = %d\n", cf->fwi);
	(void)printf("\tpfi       = %d\n", cf->pfi);
	(void)printf("\tpri       = %d\n", cf->pri);
	(void)printf("\tcw        = %d words from block\n", 
		     (int) (cf->cw - cf->start_buf));
	(void)printf("\tend_buf    = %d words from block\n",
		     (int) (cf->end_buf - cf->start_buf));
	(void)printf("\tnumBlocks = %d\n", cf->numBlocks);
	(void)printf("\tbuffer    = %s\n", cf->buffer  ? "(defined)":"(NULL)");
	(void)printf("\tcbuf      = %s\n", cf->cbuf  ? "(defined)" : "(NULL)");
	(void)printf("\tcbufSize  = %d\n", cf->cbufSize);
}
#endif

/*
** These routines are originally written for Fortran, therefore most all
** arguments are pointers.  C folks need to pass addresses!
**
** WARNING!!!!!
**
** These routines will not work correctly if the user has specified 
** the "intsize=8 option on the IBM xlf compiler.  In that case, the 
** address if pointing to a true 64 bit integer.
** They will work for  autodbl=dblpad since in that case, the ints are
** only *padded* with an extra 4 bytes.
*/

/****************************************************************************/
/*                                                                          */
/*          P U B L I C   R O U T I N E S                                   */
/*                                                                          */
/****************************************************************************/


/*-------------------------------------------------------------------------*/
int CrayBlocks(
	int	size
) {
	/*
	** Set the number of blocks for a crayopen()
	*/
	if (size <= 0)   return(-1);

	BUF_BLK_SZ = size;
	return(0);

}
/*-------------------------------------------------------------------------*/
int CrayOpen(
	const char 	*flnm,
	int  		rwflags,
	int		mode
) {
	int 		flags;
	int		unblkFlag;
	int		index;


	/*
	** First, loop through the array to find a empty spot.
	*/
	for(index = 1; index < MAX_ARRAY_SIZE; index++) {
		if (!ca[index].cf)  break;
	}

	if (index > (MAX_ARRAY_SIZE - 1)) return(CRAY_OPENERR);



	switch(rwflags) {

	    /*
	    ** Blocked file flags
	    */

   	    case 0: 	/* read only */
      	   	flags = O_RDONLY;
		unblkFlag = 0;	/* False */
      		break;

   	    case 1: 	/* write only, create, truncate */
      		flags = O_WRONLY | O_CREAT | O_TRUNC;
		unblkFlag = 0;	/* False */
      		break;

   	    case 2: 	/* read/write, create, no truncate */
      		flags = O_RDWR | O_CREAT;
		unblkFlag = 0;	/* False */
      		break;

   	    case 3:
      		flags = O_APPEND | O_RDWR;
		unblkFlag = 0;	/* False */
      		break;

	    /*
	    ** Unblocked file flags
	    */

   	    case 10: 	/* read only */
      	   	flags = O_RDONLY;
		unblkFlag = 1;	/* True */
      		break;

   	    case 11: 	/* write only, create, truncate */
      		flags = O_WRONLY | O_CREAT | O_TRUNC;
		unblkFlag = 1;	/* True */
      		break;

   	    case 12: 	/* read/write, create, no truncate */
      		flags = O_RDWR | O_CREAT;
		unblkFlag = 1;	/* True */
      		break;

   	    case 13:
      		flags = O_APPEND | O_RDWR;
		unblkFlag = 1;	/* True */
      		break;

   	    default:
      		return(CRAY_OPENERR);
	}
      
	/*
	** Open the file
	*/
	ca[index].cf = cray_open(flnm, flags, mode, unblkFlag); 

	if (ca[index].cf ==  NULL) return(CRAY_OPENERR);

	/* 
	** for append modes we must verify EOD && EOF and back up over them
	*/
	if (rwflags == 3 || rwflags == 13) {
	    if (position(ca[index].cf) != 0)  {
		(void) cray_close(ca[index].cf);
		return(CRAY_OPENERR);
	    }
	}

	return(index);
}

/*--------------------------------------------------------------------------*/
int CrayRew(
int	index
) {

	return(cray_rewind(cf_locate(index)));
}

/*--------------------------------------------------------------------------*/
int CrayRead(
	int		index,
	void	 	*buf,
	int 		size,
	int		conv
) {
	int 		rc;
	int		num_bytes = size*CRAY_WORD_SIZE;
	int 		cwords;
	CrayFile	*cf;

	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	/*
	** Handle the special case of no conversion here.  
	** Read directly into the user's buffer.
	*/
	if (conv == 0) { 
	    rc = cray_read(cf, buf, num_bytes);
	    return((rc < 0) ? rc : rc/CRAY_WORD_SIZE);
	}

	/*
	** For all other conversion cases, allocate a buffer for the 
	** read data so that the converted data can be placed in the
	** user's buffer...
	**
	** If we fail to reallocate the buffer, return zero words read...
	*/
	if (check_buf(cf, num_bytes)) return(CRAY_MEM);


	/*
	** Read data into the conversion buffer.
	** Yup, in this case, we are (in essence) double buffering the 
	** file.   If positive, then errcode is the number of bytes read
	*/
 	rc = cray_read(cf, cf->cbuf, num_bytes);
	if (rc < 0)  return(rc);

	cwords = rc/CRAY_WORD_SIZE;

	if (convert_read_data(conv, cf->cbuf, buf, cwords) != cwords) {
		return(0);
	}

	return(cwords);

} 
/*--------------------------------------------------------------------------*/
int CrayWrite(
	int		index,
	void	 	*buf, 
	int		words,
	int		conv
) {

	int 		rc;
	int		num_bytes = words*CRAY_WORD_SIZE;
	CrayFile	*cf;


	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	/*
	** For no conversion, place in the user's buffer...
	*/
	if (conv == 0) {
	    rc =  cray_write(cf, buf, num_bytes);
	    return((rc == 0) ? words : rc);
	}

	/*
	** For all other conversion cases, allocate a buffer for the 
	** read data so that the converted data can be placed in the
	** user's buffer...
	**
	** If we fail to allocate the buffer, return zero words read...
	*/
	if (check_buf(cf, num_bytes)) return(CRAY_MEM);

	/*
	** Convert the data.
	*/
	if (convert_write_data(conv, buf, cf->cbuf, words) != words) 
								return(0);

	/*
	** Now write it...
	*/
	rc = cray_write(cf, cf->cbuf, num_bytes);


	return((rc == 0) ? words : rc);
}

/*--------------------------------------------------------------------------*/
int CrayWeof(
	int	index
) {
	CrayFile	*cf;
	/*
	** Write a Cray EOF to the file 
	*/
	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);
	return(cray_write_eof(cf));

}

/*--------------------------------------------------------------------------*/
int CraySeek(
	int	index,
	int	word_off,
	int	whence

) {
	CrayFile	*cf;
	/*
	** Write a Cray EOF to the file 
	*/
	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);
	return(cray_seek(cf, word_off, whence));

}

/*--------------------------------------------------------------------------*/
int CrayWeod(
	int	index
) {
	CrayFile	*cf;

	/*
	** Write a Cray EOD to the file, users should *never* 
	** call this.  It's only here for historical purposes
	*/
	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	return(cray_write_eod(cf));
}

/*--------------------------------------------------------------------------*/
int CrayClose(index)
int	index;
{
	CrayFile	*cf;
	int		rc;
	/*
	** Close the file
	*/
	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	rc = cray_close(cf);

	if (rc == 0) {
		ca[index].cf = NULL;
	}

	return(rc);
}

/*--------------------------------------------------------------------------*/
int CrayBack(
	int	index
) {
	CrayFile	*cf;
	/*
	** Backspace
	*/
	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);
	return(cray_back(cf));
}

/*--------------------------------------------------------------------------*/
int CrayGetWa( 
int	 	index,
char 		*buf,
int		loc,
int 		size,
int		conv
) {
	CrayFile	*cf;
	int 		errcode;
	int		num_bytes = size*CRAY_WORD_SIZE;
	int 		cwords;


	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	/*
	** The file must be an unblocked file....
	*/
	if (!cf->unblkFlag) return(CRAY_UNBLKERR);

	/*
	** For all other conversion cases, allocate a buffer for the 
	** read data so that the converted data can be placed in the
	** user's buffer...
	**
	** If we fail to allocate the buffer, return zero words read...
	*/
	if (check_buf(cf, num_bytes)) return(CRAY_MEM);


	/*
	** Move the the word location pointed to by loc.
	** Then read in the data...
	*/
	if (lseek(cf->crayfd, (loc-1)*CRAY_WORD_SIZE, SEEK_SET) == -1) 
	    return(-99);

	errcode = read(cf->crayfd, cf->cbuf, num_bytes);
	if (errcode != num_bytes) return(CRAY_SHORTRD);

	cwords = errcode/CRAY_WORD_SIZE;

	/*
	** If there is no conversion, copy the data and return here.
	*/
	if (conv == 0) {
		 (void)memcpy(buf, cf->cbuf, num_bytes);
		 return(cwords);
	}

	/*
	** Convert...
	*/
	if (convert_read_data(conv, cf->cbuf, buf, cwords) != cwords) return(0);

	return(cwords);
}

/*--------------------------------------------------------------------------*/
int CrayPutWa( 
int	 	index,
unsigned char 	*buf, 
int		loc,
int		size,
int		conv 
) {

	CrayFile	*cf;
	int 		rc;
	int		num_bytes = size*CRAY_WORD_SIZE;

	if ((cf = cf_locate(index)) == NULL) return(CRAY_BADINDEX);

	/*
	** The file must be an unblocked file....
	*/
	if (!cf->unblkFlag) return(CRAY_UNBLKERR);

	/*
	** For all other conversion cases, allocate a buffer for the 
	** read data so that the converted data can be placed in the
	** user's buffer...
	**
	** If we fail to allocate the buffer, return zero words read...
	*/
	if (check_buf(cf, num_bytes)) return(CRAY_MEM);

	/*
	** Move the the word location pointed to by loc.
	** Then read in the data...
	*/
	if (lseek(cf->crayfd, (loc-1)*CRAY_WORD_SIZE, SEEK_SET) == -1) 
	    return(-99);

	/*
	** Convert as necesary...
	*/

	if (conv == 0) {
		(void)memcpy(cf->cbuf, buf, num_bytes);
	}
	else {
		if (convert_write_data(conv, buf, cf->cbuf, size) != size) 
			return(0);
	}

	/*
	** Write the data....
	*/
	rc = write(cf->crayfd, cf->cbuf, num_bytes);
	if (rc != num_bytes) return(CRAY_DISKERR);

	return(size);

}

/*
** Because the Fortran compilers add an underscore to Fortran entry
** points, we need to define the following wrappers to the user 
** entry points.  
** 
** There probably is a cleaner way to manage this, however we are now 
** stuck with this documented (and widely used) interface.
**
** By doing this, the interfaces are the same for SUNs, SGIs,  and RS6000s
*/

#if defined(ArchSun) || defined(ArchSGI) || defined(ArchOFS1) || \
	defined(__uxp__) || defined(ArchHP) || defined(ArchIBM) || \
	defined(ArchLinux) || defined(ArchOSF1)

int crayblocks_(
	int *size
) {
        return(CrayBlocks(*size));
}

int crayopen_(
	char    *flnm,
	int     *rwflags,
	int     *mode,
	int     plen
) {

	char		path[1025];
	char		*p;

	/*
	** Check for a trailing blank in the filename, 
	** set to '\0';
	*/
	if (plen > sizeof(path)-1) return(CRAY_OPENERR);
	(void)strncpy(path, flnm, plen);
	path[plen] = '\0';

	p = &path[plen-1];
	while(p > path && *p == ' ') {
		*p = '\0';
		p--;
	}
        return(CrayOpen(path, *rwflags, *mode));
}

int crayrew_(
	int	*index
) {

        return(CrayRew(*index));
}

int crayread_(
	int		*index,
	void 		*buf,
	int             *size,
	int             *conv
) {
        return(CrayRead(*index, buf, *size, *conv));
}

int craywrite_(
	int		*index,
	void		*buf,
	int             *size,
	int             *conv
) {
        return(CrayWrite(*index, buf, *size, *conv));
}

int crayseek_(
	int		*index,
	int             *word_off,
	int             *whence
) {
        return(CraySeek(*index, *word_off, *whence));
}
int crayweof_(
	int	*index
) {
        return(CrayWeof(*index));
}

int crayweod_(
	int	*index
) {
        return(CrayWeod(*index));
}

int crayback_(
	int	*index
) {
        return(CrayBack(*index));
}

int crayclose_(
	int 	*index
) {
        return(CrayClose(*index));
}

#endif


/*
**  The following are the "old" C bindings...
*/

int crayblocks(
	int *size
) {
        return(CrayBlocks(*size));
}

int crayopen(
	char    *flnm,
	int     *rwflags,
	int     *mode,
	int     plen
) {

	char		path[1025];
	char		*p;

	/*
	** Check for a trailing blank in the filename, 
	** set to '\0';
	*/
	if (plen > sizeof(path)-1) return(CRAY_OPENERR);
	(void)strncpy(path, flnm, plen);
	path[plen] = '\0';

	p = &path[plen-1];
	while(p > path && *p == ' ') {
		*p = '\0';
		p--;
	}
        return(CrayOpen(path, *rwflags, *mode));
}

int crayrew(
	int	*index
) {

        return(CrayRew(*index));
}

int crayread(
	int		*index,
	void 		*buf,
	int             *size,
	int             *conv
) {
        return(CrayRead(*index, buf, *size, *conv));
}

int craywrite(
	int		*index,
	void		*buf,
	int             *size,
	int             *conv
) {
        return(CrayWrite(*index, buf, *size, *conv));
}

int crayseek(
	int		*index,
	int             *word_off,
	int             *whence
) {
        return(CraySeek(*index, *word_off, *whence));
}
int crayweof(
	int	*index
) {
        return(CrayWeof(*index));
}

int crayweod(
	int	*index
) {
        return(CrayWeod(*index));
}

int crayback(
	int	*index
) {
        return(CrayBack(*index));
}

int crayclose(
	int 	*index
) {
        return(CrayClose(*index));
}

