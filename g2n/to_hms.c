/* void to_hms(NR_TYPE *sam, NR_TYPE *hr, NR_TYPE *mi, NR_TYPE *se)
 *  Calculate hours, minutes and seconds from seconds after midnight
 *   with provision to handle a midnight transition */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1994 */

# include <stdio.h>

# include "g2n.h"

void to_hms(NR_TYPE *sam, NR_TYPE *hr, NR_TYPE *mi, NR_TYPE *se)

{

  long isam;
  long ihr, imi, ise;

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  isam = ((long)*sam)%86400;
  ihr = (isam/3600);
  imi = (isam%3600/60);
  ise = (isam%60);
  *hr = (NR_TYPE)ihr;
  *mi = (NR_TYPE)imi;
  *se = (NR_TYPE)ise;
}
