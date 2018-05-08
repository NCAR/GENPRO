/***********************  TrimTrailingBlanks()  **************************/
# include <string.h>

/* char * TrimTrailingBlanks(char *s)  */
void TrimTrailingBlanks(char *s)
/* remove trailing blanks from character string */
/*  adapted from "libraf/util.c:TrimTrailingBlanks" by RLR  October 1998
 *   for use with g2n  */

{
  int i;
  int len;

  len=(int)strlen(s);

  for (i=len-1; i>=0; i--)
    if (!(isspace(s[i])) )
  {

/* With gcc compilation (but not SPARCWorks...), accessing the len'th
   position of s results in SEGV.  Don't do it.  Assume it's already set to
   the null character if char just before it is first non-space character
   found.  */

    if ((i+1)<len) s[i+1]='\0';
    return;
  }
/*  return NULL;  */
  return;
}
