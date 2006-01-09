/* int name_idx (char *name)
 *  Get index of a GENPRO variable name */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int name_idx (char *name)

{

  int i;    /* counter */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  for (i=0;i<nvars;i++)
  {
    if (strcmp(name,Gpars[i].name) == 0)
    {
      return i;
    }
  }
/* If I get here, the name never appeared in the APPVAR list. */
  return -1;
}
