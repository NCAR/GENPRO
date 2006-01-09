/* int get_ldrec(void)
 *  Read a logical GENPRO data record */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1998 */

/*  This version created to enable reading COS-blocked data files  */

# include <stdio.h>

# include "g2n.h"

int get_ldrec(void)

{

  static int rec_no = -1;    /* logical record number */
  static int first = TRUE;
  static int wds;
  int i, lr;
  int flag;          /* Good record, if YES */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  if (first == TRUE)
  {
    first = FALSE;
/* Note:  I'm assuming that GENPRO data record values have 32 bits. */
    wds = logbit/32;
  }
  if (rec_no >= datlog | rec_no < 0)
  {
    if (get_pdrec() == NO) return NO;
    rec_no = 0;
  }
/* Set logical record index and address. */
  lr = rec_no++ * wds;
  if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
  {
    ig = &d_buf[lr];
  }
  else /* if (G_Format == Format_UW_IEEE) */
  {
    igf = &f_buf[lr];
  }
/* Check to see if logical record is empty (EOF).
 * Assume that first logical record is OK (else why write it?) */
  if (rec_no != 0)
  {
    if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
    {
      for (i=lr;i<lr+wds;i++)
      {
        if (d_buf[i] != 0)
        {
          flag = YES;
          break;
        }
        else
        {
          flag = NO;
        }
      }
    }
    else /* if (G_Format == Format_UW_IEEE) */
    {
      for (i=lr;i<lr+wds;i++)
      {
        if (f_buf[i] != 0.0)
        {
          flag = YES;
          break;
        }
        else
        {
          flag = NO;
        }
      }
    }
  }
  if (flag == TRUE) l_d_rec++;
  return flag;
}
