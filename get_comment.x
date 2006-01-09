/* void get_comment(char *line)
 *  Decode COMMENT to get data format (NCAR, U Wyoming, NOAA Twin Otter) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_comment(char *line)

{
/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Assume that format is NCAR's unless a specific format string
 *  is found on the first COMMENT line.
 *  (Note:  This method is not foolproof.  If an NCAR data set has either
 *           UW, N2UW or 48RF in the first comment line, it will fail. */
  if ((strstr(line," UW ") != NULL) || (strstr(line,"N2UW ") != NULL))
  {
    G_Format = Format_UW;
/*  ###########  Diagnostic print  */
    printf ("get_comment:  Format is %s\n","Format_UW");
/*  Check for Twin Otter variant */
    if ((strstr(line," 48RF ") != NULL) || (strstr(line," N48RF ") != NULL))
    {
      G_Format = Format_N48RF;
/*  ###########  Diagnostic print */
      printf ("get_comment:  Format changed to %s\n","Format_N48RF");
    }
  }
/* Following is the default that is preset in the file "g2n.h".  If a new
 *  format is needed, then this code may need to be activated.
  else
  {
    G_Format = Format_NCAR;
 *  ###########  Diagnostic print
    printf ("get_comment:  Format is %s\n", "Format_NCAR");
  }
 */
}
