/* void get_logbit(char *line)
 *  Get LOGBIT value (number of bits in a logical record) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

/*  How to do it:
 *   Skip past keyword, equals sign and possible space(s)
 *   Read number
 *   Convert from ascii to integer and save
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_logbit(char *line)

{

  char *p;  /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"LOGBIT =");
  if (p != NULL)
  {
    logbit = atoi(p);
  }
}
