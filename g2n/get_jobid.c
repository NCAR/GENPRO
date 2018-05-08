/* void get_jobid(char *line)
 *  Get the JOBID (job serial number) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

/*  How to do it:
 *   Skip to the first "
 *   Read until a <space> character is detected
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_jobid(char *line)

{

  char *p;    /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line, "\"");
  if (p != NULL)
  {
    p = strtok(NULL," ");
    if (p != NULL)
    {
      while (*p == ' ') ++p;
      strncpy(jobid, p, 6);
      jobid[6] = '\0';
    }
  }
}
