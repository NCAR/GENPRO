/* int get_hrec(char *s, size_t n)
 * Read a logical GENPRO header record (80 character line) */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1998 */

/*  This version adapted by RLR to handle stripped or COS-blocked files */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int get_hrec(char *s, size_t n)

{

  static int first = TRUE;    /* initialization flag */
  static int wds;             /* # 32-bit words in physical header record */
  static int rec_no;          /* logical record number (0 = first) */
  static int lr;              /* # logical header records per physical record */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  if (DiskData == STRIPPED)
  {
    return (fread (s, 1, n, Gp_fp) == n);
  }
  if (DiskData == COS_BLOCKED)
  {
    if (first == TRUE)
    {
      first = FALSE;
      if ((wds = get_phrec()) <= 0) return NO;
      lr = wds/n;
      rec_no = 0;
    }
    if (rec_no >= lr | rec_no < 0)
    {
      if ((wds = get_phrec()) <= 0) return NO;
      rec_no = 0;
    }
/* Set logical record index and address. */
/*  ############  need to copy string of "n=80" chars from h_buf --> s */
/*    h_buf &h_buf */
/*    s = &h_buf[rec_no*n]; */
    memcpy(s,&h_buf[rec_no*n],n);
    rec_no++;
    return YES;
  }
  return NO;
}
