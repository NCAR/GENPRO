/* int get_names(char *line)
 *  Get GENPRO variable name(s) on an APPVAR line and put it(them) into the
 *   parameter array by decoding each name delmited by a comma and optional
 *   white space */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

/*  How to do it:
 *   decode an entire APPVAR line
 *   put all the names found into the variable array Gparm
 *   increment "nvar" (the total number of variables) appropriately.
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int get_names(char *line)

{

  char *p;  /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"=");
/*      The \n character should only appear in my test file */
  while ((p=strtok(NULL,", \n")) != NULL)
  {
/*  Test to see whether I still have room for another variable */
    if (nvars >= NVAR)
    {
/*   Too many variables */
      return -1;
    }
    strcpy(Gpars[nvars].name,p);
/*  Preset variable's critical parameters to default values or, in the
   case of critical parameters, to bogus values. */
    par_preset(nvars);
    nvars++;
  }
  return 0;
}
