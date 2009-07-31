/* void d_P_convert(int n)
 *  Convert a PMS-1D variable's data from its raw, packed-integer value to
 *   its intended, real value (includes proper decoding of data indices to
 *   get the data and properly stuff them into the output array
 *  Adapted from d_convert.c by RLR  971230
 *  Enhanced for Intel-based machines (Linux) by RLR 20040311
*/
/*  COPYRIGHT:  University Corporation for Atmospheric Research, 1997, 1998, 2004
 */
/*  See notes in d_convert.c for information relating to how conversion
 *   keys and other variable parameters are used.
 *
 *                            Ronald L. Ruth
 *                            NCAR/ATD/RAF
 */

# include <stdio.h>
# include "g2n.h"
/* # include "portable.h"  <-- cannot find file 20080123 RLR */

void d_P_convert(int n)

{

  int i, j, k;   /* index and loop indices */
  int *ip;       /* input address pointer */
  int *op;       /* output address pointer */
  NR_TYPE v;     /* temporary value storage */

/*  ################     Begin test variables */
/*  static int cnt = 0; */
/*  ################     End  test variables */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */

/*  ################     Begin test code (do limited iterations)  */
/*  if (cnt++ > 12 ) cleanup (); */
  printf ("%s:d_P_convert:  working on Probe %i, (%s)\n",progname,n,Pgpars[n].name);
  printf ("%s:d_P_convert: p_buf start address = %i\n",progname,&p_buf[Pgpars[n].fstpt]);
  printf ("%s:d_P_convert: Probe output starting index = %i\n",progname,Pgpars[n].fstpt);
/*  ################      End  test code */

  k = 0;
/* Test for GENPRO data type */
  if (G_Format == Format_NCAR)
/*  NCAR or standard Wyoming */
  {
/* Test for PMS-1D data type */
    if (pms1d[n].type == 1)
/*  Standard PMS-1D data */
    {
/*  ################  Test print */
      printf ("%s:d_P_convert:  probe type 1, rate = %i\n",progname, Pgpars[n].rate);
      for (i=0;i<Pgpars[n].rate;i++)
      {
/*  ################  Test print */
        printf ("%s:d_P_convert:  probe has %i cells\n",progname,pms1d[n].num_found_cells);
        for (j=0;j<pms1d[n].num_found_cells;j++)
        {
/* Do I set a value for the first cell position and offset the others? */
          if (j==0 && pms1d[n].cell_offset == YES)
/*  Yes */
          {
/*  ################  Test print */
            printf ("%s:d_P_convert:  invoking cell offset insert\n",progname);
            printf ("%s:d_P_convert:  Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
/*   Chris said to use "zero" instead of "missing_val" here */
            p_buf[Pgpars[n].fstpt+k++] = 0.;
          }
/* Do I have a missing cell (See setup_pms1d.c around line 52.) ? */
          if (pms1d[n].cell_map[j] == -1)
/*  Yes--Fill it with the missing value */
          {
            v = missing_val;
          }
          else
/*  No--All is OK */
          {
            v = ntohl(*(ig+ pms1d[n].cell_map[j] + i));
/* For this implementation:  if SCLKEY != 1, I will use the scaling algorithm
    with TERM and FACTOR.  (To be rigorous, SCLKEY = 1 to use the unaltered,
    integer value, and SCLKEY = 2 to use the scaling algorithm.) */
            if (Pgpars[n].sclkey != 1) /* i.e., SCLKEY = 2 */
            {
              v /= Pgpars[n].factor;
              v -= Pgpars[n].term;
            }
            if (v == 99999.) v = missing_val;
          }
/*  ################  Test print */
          printf ("%s:d_P_convert:  Cell #%i = %f\n",progname,j+1,v);
/*
          printf ("%s:d_P_convert:   Its base cell map address = %i\n",progname,pms1d[n].cell_map[j]);
          printf ("%s:d_P_convert:   Its cell map offset = %i\n",progname,i);
          printf ("%s:d_P_convert:   Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
 */
          p_buf[Pgpars[n].fstpt+k++] = v;
        }
/*  ################  Test print */
        printf ("%s:d_P_convert:  num_found_cells = %i, cell_offset = %i, num_defined_cells = %i\n",progname,pms1d[n].num_found_cells,pms1d[n].cell_offset,pms1d[n].num_defined_cells);
        for (j=(pms1d[n].num_found_cells+pms1d[n].cell_offset);j<pms1d[n].num_defined_cells;j++)

        {
/*  ################  Test print */
          printf ("%s:d_P_convert:  Fill Cell #%i\n",progname,j);
          printf ("%s:d_P_convert:  Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
/*   Chris said to use "zero" instead of "missing_val" here */
          p_buf[Pgpars[n].fstpt+k++] = 0.;
        }
      }
      printf ("%s:d_P_convert:  Highest p_buf address = %i\n\n",progname,&p_buf[Pgpars[n].fstpt+k-1]);
    }
    if (pms1d[n].type == 2)
/*  High-rate PMS-1D data */
    {
/*  ################  Test print */
      printf ("%s:d_P_convert:  probe type 2, rate = %i\n",progname, Pgpars[n].rate);
      for (i=0;i<Pgpars[n].rate;i++)
      {
/*  ################  Test print */
        printf ("%s:d_P_convert:  probe has %i cells\n",progname,pms1d[n].num_found_cells);
        for (j=0;j<pms1d[n].num_found_cells;j++)
        {
/* Do I set a value for the first cell position and offset the others? */
          if (j==0 && pms1d[n].cell_offset == YES)
/*  Yes */
          {
/*  ################  Test print */
            printf ("%s:d_P_convert:  invoking cell offset insert\n",progname);
            printf ("%s:d_P_convert:  Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
/*   Chris said to use "zero" instead of "missing_val" here */
            p_buf[Pgpars[n].fstpt+k++] = 0.;
          }
          v = ntohl(*(ig+ pms1d[n].cell_map[0]+ i*pms1d[n].num_found_cells+ j));
/* For this implementation:  if SCLKEY != 1, I will use the scaling algorithm
    with TERM and FACTOR.  (To be rigorous, SCLKEY = 1 to use the unaltered,
    integer value, and SCLKEY = 2 to use the scaling algorithm.) */
          if (Pgpars[n].sclkey != 1) /* i.e., SCLKEY = 2 */
          {
            v /= Pgpars[n].factor;
            v -= Pgpars[n].term;
          }
          if (v == 99999.) v = missing_val;
          printf ("%s:d_P_convert:  Cell #%i = %f\n",progname,j+1,v);
          printf ("%s:d_P_convert:   Its base cell map address = %i\n",progname,pms1d[n].cell_map[0]+ i*pms1d[n].num_found_cells+ j);
          printf ("%s:d_P_convert:   Its cell map offset = %i\n",progname,i);
          printf ("%s:d_P_convert:  Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
          p_buf[Pgpars[n].fstpt+k++] = v;
        }
        for (j=(pms1d[n].num_found_cells+pms1d[n].cell_offset);j<pms1d[n].num_defined_cells;j++)

        {
/*  ################  Test print */
          printf ("%s:d_P_convert:  Fill Cell #%i\n",progname,j);
          printf ("%s:d_P_convert:  Cell output index == %i\n",progname,Pgpars[n].fstpt+k);
/*   Chris said to use "zero" instead of "missing_val" here */
          p_buf[Pgpars[n].fstpt+k++] = 0.;
        }
      }
      printf ("%s:d_P_convert:  Highest p_buf address = %i\n\n",progname,&p_buf[Pgpars[n].fstpt+k-1]);
    }
  }
/*  ################
 *    I won't worry yet about Wyoming data that have this PMS-1D format.
 *     They have a lower-case set of variable names.  In general the code
 *     should be the same with the substitution of "igf" for "ig when
 *     decoding the values.
 *  ################
 */
  else /* if ((G_Format == Format_UW)  || (G_Format == Format_UW_IEEE))*/
  {
    fprintf(stderr,"%s:  g2n cannot yet handle Wyoming data with PMS-1D data\n",progname);
    cleanup();
  }
/*else if ( G_Format == Format_N48RF )
  {
    fprintf(stderr,"%s:  g2n cannot yet handle NOAA Twin Otter data with PMS-1D data\n",progname);
    cleanup();
  }
  else (future format support)
  {
  }
 */
  return;
}
