/* void get_datsiz(char *line)
 *  Get DATSIZ value (number of bits in a physical record) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

/*  How to do it:
 *   Skip past keyword, equals sign and possible space
 *   Read number
 *   Convert from ascii to integer
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_datsiz(char *line)

{

  char *p;  /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"DATSIZ =");
  if (p != NULL)
  {
    datsiz = atoi(p);
  }
}
