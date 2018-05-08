/* void get_datlog(char *line)
 *  Get DATLOG value (number of logical records in a physical record) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

/*  How to do it:
 *   Skip past keyword, equals sign and possible space
 *   Read number
 *   Convert from ascii to integer
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_datlog(char *line)

{

  char *p;  /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"DATLOG =");
  if (p != NULL)
  {
    datlog = atoi(p);
  }
}
