/* int get_pdrec(void)
 *  Read a physical GENPRO data record */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1998 */

/*  This version written by RLR to handle COS-blocked files */

# include <stdio.h>

# include "g2n.h"

int get_pdrec(void)

{

  int i;
  size_t j;
  static int iconv = 0;
  static int nWords;     /*  Maximum record size (8-byte Cray words)  */
  int wds;               /*  actual record size (8-byte Cray words)  */
  int first = TRUE;

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Note:  I'm assuming that GENPRO data record values have 32 bits. */

  if (DiskData == STRIPPED)
  {
    j = (size_t)(datsiz/8);
    if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
    {
      i = (int)fread(d_buf, j, 1, Gp_fp);
    }
    else /* if (G_Format == Format_UW_IEEE) */
      i = (int)fread(f_buf, j, 1, Gp_fp);
    if (i != 1)
    {
      if (feof(Gp_fp) == NO)
      {
        perror(infile);
      }
      return NO;
    }
    p_d_rec++;
    return YES;
  }

/*  COS-blocked record format  */
  if (DiskData == COS_BLOCKED)
  {
    if (first)
    {
      first = FALSE;
      nWords = datsiz/64 + 1;
/*  Skip the EOF between header and data */
      if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
        wds = crayread(&infd, d_buf, &nWords, &iconv);
      else /* if (G_Format == Format_UW_IEEE) */
        wds = crayread(&infd, f_buf, &nWords, &iconv);
      if (wds > 0)
        return (wds*8);
      if (wds != CRAY_EOF)
      {
        fprintf(stderr,"%s:  COS-block file read error %i\n",progname,wds);
        return NO;
      }
    }
    if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
      wds = crayread(&infd, d_buf, &nWords, &iconv);
    else /* if (G_Format == Format_UW_IEEE) */
      wds = crayread(&infd, f_buf, &nWords, &iconv);
    if (wds > 0)
    {
  /* #############  Debug print
      printf ("%s:get_pdrec:  %i words in data record\n",progname,wds); */
      return (wds*8);
    }
  /* #############  Debug print
    printf ("%s:get_pdrec:  reached end of data file.\n",progname,wds); */
    return NO;
  }
  return NO;
}
