/* int get_phrec(void)
 * Read a physical GENPRO header record and return its size in chars */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1998 */

/*  This version written by RLR to handle COS-blocked files */

# include <stdio.h>

# include "g2n.h"

int get_phrec(void)

{

  int nWords = 4096, iconv = 0;
  int wds;            /*  number of Cray words (bytes * 8) */

  if ((wds = crayread(&infd, h_buf, &nWords, &iconv)) > 0)
/* #############  Debug print
  printf ("%s:  %i words in header record\n",progname,wds); */
/*  wds*=8; */
  return (wds*8);
  {
/*  Reached end of header file.  Data file is next, so don't close it */
/*    crayclose(&infd); */
/* #############  Debug print
    printf ("%s:get_phrec:  reached end of header record. \n",progname,wds); */
    return NO;
  }
}
