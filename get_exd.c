/* void get_exd(char *line)
 *  Decode EXDATE (Date data were processed by GENPRO) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_exd(char *line)

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
  p = strtok(line,"/EXDATE =(\"");
  if (p != NULL)
  {
    exdate[0] = atoi(p);
  }
  p = strtok(NULL,",\" ");
/*  If p is a month string, I need to translate it to a month number. */
  if (p != NULL)
  {
    if (isdigit(*p))
    {
      exdate[1] = atoi(p);
    }
    else
    {
      str_toupper(p);
      for (j=0;j<11;j++)
      {
        if (strcmp(p,mon[j]) == 0)
        {
          exdate[1]=j+1;
          break;
        }
      }
    }
  }
  p = strtok(NULL,",\" ");
  if (p != NULL)
  {
    exdate[2] = atoi(p);
  }
}
