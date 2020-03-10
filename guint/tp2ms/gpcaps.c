/* caps.c -- convert all characters from lower to upper case
*/

#include <ctype.h>
#include <string.h>
caps_(s)
char *s;
{
int i;
 i=0;
 while (i <= strlen(s)) {
 if (islower(s[i])) 
  s[i]=toupper(s[i]);
 i++;
 }
}
