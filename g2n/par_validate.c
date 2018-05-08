/* int par_validate(void)
 *  Verify that values of all variables' critical parameters have been read
 *   from the GENPRO header */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>
# include <string.h>
# include "g2n.h"

int par_validate(void)

{

  char *T;          /* Title/units pointer */
  int n;            /* Counter */
  int t;            /* Title flag */
  int flag = TRUE;  /* Good flag */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  for (n=0;n<nvars;n++)
  {

/* Critical parameters:
 * ------------------- */
/*  SAMPLE */
/*    SAMPLE is checked in `read_hdr.' */

/*  RATE   */
/*    RATE is checked in `read_hdr.' */

/*  BITS */
    if (Gpars[n].bits == PRE_BITS)
    {
/*       Because, in GENPRO-II, data are always saved as unsigned 32-bit
 *        integers, instead of quitting as fatal if BITS is missing, I will,
 *        instead, preset BITS to 32 and warn the user. */
      fprintf (stderr,"%s:par_validate:  Warning:  %s has missing "
        "parameter:  BITS.\n",progname,Gpars[n].name);
      fprintf (stderr,"  Setting BITS to 32.\n");
/*      flag = FALSE; */
      Gpars[n].bits = 32;
    }
/*  FSTBIT */
    if (Gpars[n].fstbit == PRE_FSTBIT)
    {
      fprintf (stderr,"%s:par_validate:  %s has missing parameter:  "
        "FSTBIT.\n",progname,Gpars[n].name);
      fprintf (stderr,"  This is fatal.\n");
      flag = FALSE;
    }
/*  FACTOR */
    if (Gpars[n].factor == PRE_FACTOR)
    {
/*       Factor should NEVER be a negative number. */
      fprintf (stderr,"%s:par_validate:  %s has missing parameter:  "
        "FACTOR.\n",progname,Gpars[n].name);
      fprintf (stderr,"  This is fatal.\n");
      flag = FALSE;
    }
    if (Gpars[n].factor == 0.0)
    {
/*       Factor should NEVER be zero. */
/*        Note:  One may want to set FACTOR = 1.0 instead of quitting. */
      fprintf (stderr,"%s:par_validate:  %s has zero value for parameter:"
        "  FACTOR.\n",progname,Gpars[n].name);
      fprintf (stderr,"  This is fatal.\n");
      flag = FALSE;
    }
/*  TERM */
/*    if (Gpars[n].term == PRE_TERM);
 *       There is no easy way to check TERM for a valid number.  Just about
 *        any offset is legal.  Therefore, I'll do nothing and only rely
 *        on FACTOR, because, normally, they are both read from the same
 *        logical header record. */
/*  TITLE */
    T = Gpars[n].title;
    while (*T == ' ') T++;
    if (*T == '\0')
    {
/*       If the variable's title has only <space> characters, set it to
 *        PRE_TITLE. */
      strcpy(Gpars[n].title,PRE_TITLE);
      t = FALSE;
    }
    else
    {
      t = TRUE;
    }
/*  UNITS */
    T = Gpars[n].units;
    while (*T == ' ') T++;
    if (*T == '\0')
    {
/*       If the variable's units has only <space> characters, set it
 *        to PRE_UNITS if the variable's title was also only <space>
 *        characters.  Otherwise, set units to "none." */
      if (t == TRUE)
      {
        strcpy(Gpars[n].units,"none");
      }
      else
      {
        strcpy(Gpars[n].units,PRE_UNITS);
      }
    }
/* Parameter that doesn't need to be checked, since it is derived from
 *  other critical parameters.
    if (Gpars[n].fstpt == PRE_FSTPT);
 */
/* CONKEY will be used to decode the alternate U of Wyoming format.
    if (Gpars[n].conkey == PRE_CONKEY);
 */
/* Non-critical parameters.  As of now, I don't intend to use these.
    if (Gpars[n].skip == PRE_SKIP);
    if (Gpars[n].sclkey == PRE_SCLKEY);
 */
  }
  return flag;
}
