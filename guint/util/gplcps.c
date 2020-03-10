/* caps.c -- convert all characters from upper to lower case
*/

#include <ctype.h>
#include <string.h>
caps_(s)
char *s;
{
int i;
 i=0;
 while (i <= strlen(s)) {
 if (isupper(s[i])) 
  s[i]=tolower(s[i]);
 i++;
 }
}
