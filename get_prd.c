/* void get_prd(char *line)
 *  Decode PRDATE (Flight date) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_prd(char *line)

{

  char *p;  /* pointer into current line */
  int j;    /* counter */
  char *mon[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG",
                   "SEP", "OCT", "NOV", "DEC" };

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"/PRDATE =(\"");
  if (p != NULL)
  {
    prdate[0] = atoi(p);
  }
  p = strtok(NULL,",\" ");
  if (p != NULL)
  {
/* If p is a month string, I need to translate it to a month number. */
    if (isdigit(*p))
    {
      prdate[1] = atoi(p);
    }
    else
    {
      for (j=0;j<11;j++)
      {
        if (strcmp(p,mon[j]) == 0)
        {
          prdate[1]=j+1;
          break;
        }
      }
    }
  }
  p = strtok(NULL,",\" ");
  if (p != NULL)
  {
    prdate[2] = atoi(p);
  }
}
