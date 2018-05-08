/* void cleanup(void)
 *  Clean up and exit after a ^C was pressed or a fatal error occurs */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1998 */

/*  This version written by RLR to handle COS-blocked files */

# include <stdlib.h>
# include <stdio.h>

# include "g2n.h"

void cleanup(void)

{
  fprintf(stderr,"\n%s:  Quitting.\n",progname);
  if (ncid != -1) CloseNetCDF();
  else
  {
    if (DiskData == STRIPPED)
      close (Gp_fp);
    if (DiskData == COS_BLOCKED)
      crayclose(&infd);
  }
  exit(1);
}
