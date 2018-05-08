/* int get_lkey(char *line, char *key)
 *  Get leading keyword from GENPRO header line */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int get_lkey(char *line, char *key)

{

  char *Kw;
  int j;
  int match = FALSE;
  char *l;

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Skip legal leading characters in a logical record  */
  while (*line == ' ' || *line == '/') line++;

/* March through a header's logical record
 *   a character at a time looking for keywords  */
  do
  {
/*  Now march through the keywords  */
    for (j=0; Keyword[j] != NULL; j++)
    {
/*   Reset pointer to search for new keyword at the same place  */
      Kw = Keyword[j];
      l = line;
      match = TRUE;
      while (*Kw != '\0')
      {
        if (*l != *Kw)
        {
          match = FALSE;
          break;
        }
        Kw++,l++;
      }
      if (match == TRUE) break;
    }
/*    if (!match) break;   *  Go to next line  */
    if (match == FALSE) break;  /*  Go to next line  */
  } while (*++line != '\0' && match==FALSE);
  if (match == TRUE) strcpy(key,Keyword[j]);
  return match;
}
