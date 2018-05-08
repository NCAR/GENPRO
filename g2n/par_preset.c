/* void par_preset(int n)
 *  Preset variable's parameters to known values */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997 */

/* The purpose of this function is to make sure that non-critical parameters
 *  have a default value and critical parameters are set to a bogus value
 *  that can be checked later. */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void par_preset(int n)

{
/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  strcpy(Gpars[n].title,PRE_TITLE);
  strcpy(Gpars[n].units,PRE_UNITS);
  Gpars[n].sample = PRE_SAMPLE;
  Gpars[n].rate = PRE_RATE;
  Gpars[n].bits = PRE_BITS;
  Gpars[n].fstbit = PRE_FSTBIT;
  Gpars[n].fstpt = PRE_FSTPT;
  Gpars[n].skip = PRE_SKIP;
  Gpars[n].conkey = PRE_CONKEY;
  Gpars[n].sclkey = PRE_SCLKEY;
  Gpars[n].term = PRE_TERM;
  Gpars[n].factor = PRE_FACTOR;
  Gpars[n].output = YES;
}
