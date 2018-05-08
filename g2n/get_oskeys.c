/* int get_oskeys(char *line)
 *  Get sub-keywords from GENPRO ORDVAR header line */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int get_oskeys(char *line)

{

  int i;     /* counter */
  int nord;  /* Index for current sub-keyword */
  char *p;   /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Skip the keyword itself and first delimiter */
  nord = 0;
  p = strtok(line,"=");
/*     The \n character should only appear in my test file */
  while ((p=strtok(NULL,", \n")) != NULL)
  {
    for (i=0; Oskeyword[i] != NULL; i++)
    {
/*      if (strcmp(p,Oskeyword[i]) == '\0')  */
      if (strcmp(p,Oskeyword[i]) == 0)
      {
        Osklist[nord++] = i;
        break;    /* out of "for" loop */
      }
    }
  }
  return nord;
}
