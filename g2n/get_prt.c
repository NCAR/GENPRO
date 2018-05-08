/* void get_prt(char *line)
 *  Decode PRTIME (Flight start time) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 2010 */

/* Written by Janine Aquino Nov 29, 2010 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_prt(char *line)

{

  char *p;  /* pointer into current line */
  int j;    /* counter */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"/PRTIME = (\"");
  if (p != NULL)
  {
    prtime[0] = atoi(p);
  }
  p = strtok(NULL,"H\",\" ");
  if (p != NULL)
  {
    prtime[1] = atoi(p);
  }
  p = strtok(NULL,"M\",\" ");
  if (p != NULL)
  {
    prtime[2] = atoi(p);
  }
  printf ("get_prt:  Begin Time = %d:%d:%d\n",prtime[0],prtime[1],prtime[2]);
}
