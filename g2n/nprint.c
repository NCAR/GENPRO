/* int nprint(char *line)
 *  Check a GENPRO header line for non-printing characters except the
 *   newline character */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int nprint(char *line)

{

  int i;

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  for (i=0;i<HDR_LEN;i++)
    if ((isprint(*(line+i))==NO) && (*(line+i)!='\n')) return YES;
  return NO;
}
