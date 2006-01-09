/* void get_ext(char *line)
 *  Decode EXTIME (Time data were processed by GENPRO) */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_ext(char *line)

{

  char *p;  /* pointer into current line */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  p = strtok(line,"/EXTIME =(\"H");
  extime[0] = atoi(p);
  p = strtok(NULL,"M,\" ");
  extime[1] = atoi(p);
  p = strtok(NULL,"S,\" ");
  extime[2] = atoi(p);
}
