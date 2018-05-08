/* int GpOpen (char *file)
 *  Open GENPRO file
 *   This code is modified from the RDP file `gprotocdf.c'
 *   Modified March, April 1998 by RLR to add abililty to read COS-blocked
 *    files
 */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1998 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int GpOpen (char *file)

{
/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */

/*  Set up COS-blocked file settings */

  int iflag = 0, mode = 0, nWords = 4096, iconv = 0;
  int rc;

/*  Check for COS_BLOCKED first
 *    (Code adapted from NIMBUS:adsIO.c, Chris Webster) */
  if ((infd = crayopen(file, &iflag, &mode, strlen(file))) < 0)
  {
    DiskData = STRIPPED;
  }
  else
  {
    DiskData = COS_BLOCKED;
/*  Read a record to see if file really is COS-blocked */
    if ((rc = crayread(&infd, h_buf, &nWords, &iconv)) < 0)
    {
      crayclose(&infd);
      DiskData = STRIPPED;
    }
  }
  if (DiskData == COS_BLOCKED)
  {
/*  Return to beginning of COS-blocked file */
    crayrew(&infd);
  }
  if (DiskData == STRIPPED)
  {
    if ((Gp_fp = fopen (file,"rb")) == NULL)
    {
      perror (file);
      return FALSE;
    }
  }
  return TRUE;
}
