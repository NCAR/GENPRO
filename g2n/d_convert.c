/* void d_convert(int n)
 *  Convert a variable's data from its raw, packed-integer value to
 *   its intended, real value
 *  Enhanced for Intel-based machines (Linux) by RLR 20040311
*/
/*  COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997
 */
/*  Note 1:  To be rigorous about the conversion from a GENPRO perspective,
 *            CONKEY, SCLKEY, TERM and FACTOR all need to be used.
 *
 *           CONKEY determines the format of the retrieved data:
 *             0 = internal binary floating format of system on which the data
 *                  were written (IEEE floating-point for some University of
 *                  Wyoming data sets);
 *             1 = unsigned (positive) binary integer (default);
 *             2 = 2's complement binary integer;
 *             3 = 16-bit 2's complement binary integer with swapped bytes;
 *             4 = bit string with time in hours, minutes and seconds
 *                  assuming that the hours information is multiplied
 *                  by 10000 and minutes is multiplied by 100;
 *             5 = sets a dependent variable to the value of the
 *                  INPUT Independent Variable;
 *             6 = complement the bit string (i.e., bit string = !(bit string);
 *             7 = apply CONKEY = 6 followed by CONKEY = 2;
 *             8 = Wyoming KingAir Archive tape format I. V. decode;
 *             9 = Hewlett Packard floating point;
 *            10 = Wyoming KingAir flag word decode;
 *            11 = 32-bit IEEE floating-point format.
 *
 *           SCLKEY determines whether or not TERM and FACTOR should be
 *            used in the conversion.  A value of 2 means yes, 1 means no.
 *            (The GENPRO default is SCLKEY = 2.)
 *
 *           FACTOR is the divisor to be used with the converted number.
 *
 *           TERM is the offset to be subtracted from the partially-converted
 *            number after FACTOR is applied.
 */
/*  Note 2:  GENPRO definition:  SAMPLE refers to values being input;
 *            RATE refers to values being output.  For some reason, SAMPLE
 *            was used as the output rate for most NCAR GENPRO data sets.
 *            (Actually, RATE = SAMPLE in most GENPRO headers.)
 *           For some Wyoming data formats, RATE and SAMPLE differ, and RATE
 *            refers to the output rate.  For this version this function
 *            was modified to use only the RATE parameter.  (A routine in
 *            read_hdr tests whether SAMPLE should be used instead of RATE,
 *            by comparing LOGBIT to the total of all the SAMPLE and RATE
 *            values, and overwrites all the RATE values with SAMPLE values,
 *            if necessary.  (Rev 2.3, February 1997  RLR)
 */
/*  Note 3:  In this function, use of the long string of CONKEY values
 *            has not been fully implemented.  The first CONKEY value is
 *            checked in the function get_lparms.c., and, if zero, all the
 *            data values are assumed to be in IEEE floating-point format.
 */
/*  Note 4:  For version 2.4, the use of CONKEY is implemented in a limited
 *            way (only values of 0, 1 or 2), which should cover 99% of all
 *            GENPRO data.  There is a design problem, however, because the
 *            data are read into either an integer or floating-point buffer
 *            depending upon whether or not the Wyoming IEEE format has been
 *            detected by the function "get_lparms.c."  This decision is
 *            made using only the first occurrence of the CONKEY parameter
 *            in the GENPRO header.  Therefore, this function will only
 *            deal with CONKEY = 1 and CONKEY = 2 for the supported integer
 *            formats.  (Writing code to handle the other CONKEY values is
 *            more of a challenge.)
 */
/*  Note 5:  For this implementation:  if SCLKEY != 1, I will use the scaling
 *            algorithm with TERM and FACTOR.  (To be rigorous, SCLKEY = 2
 *            to use them, but only two legal values exist, 1 or 2, so in my
 *            opinion, this keeps things a bit more sane.)
 *
 *                            Ronald L. Ruth
 *                            NCAR/ATD/RAF
 */

# include <stdio.h>
# include "g2n.h"
/* # include "portable.h"   <-- cannot find file  */

void d_convert(int n)

{

  int i;
  NR_TYPE v;

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Test for data type */
  if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
  {
/*  NCAR, standard Wyoming or NOAA Twin Otter N48RF */
    for (i=0;i<Gpars[n].rate;i++)
    {
/* For this implementation:  if SCLKEY != 1, I will use the scaling algorithm
    with TERM and FACTOR.  (To be rigorous, SCLKEY = 2 to use them.) */
      if (Gpars[n].sclkey != 1) /* i.e., SCLKEY = 2 */
      {
        v = (NR_TYPE)ntohl(*(ig+Gpars[n].fstpt+i))/Gpars[n].factor - Gpars[n].term;
      }
      else /* if (Gpars[n].sclkey = 1) */
      {
        v = (NR_TYPE)ntohl(*(ig+Gpars[n].fstpt+i));
      }
/* The GENPRO missing value is 99999.  If it exists, convert it to
 *  the netCDF missing value. */
      if (v == 99999.)
      {
        g_buf[Gpars[n].fstpt+i] = missing_val;
      }
      else
      {
        g_buf[Gpars[n].fstpt+i] = v;
      }
    }
  }
  else /* if (G_Format == Format_UW_IEEE) */
  {
    for (i=0;i<Gpars[n].rate;i++)
    {
      v = ntohf(*(igf+Gpars[n].fstpt+i));
/* The GENPRO missing value is 99999.  If it exists, convert it to
 *  the netCDF missing value. */
      if (v == 99999.)
      {
        g_buf[Gpars[n].fstpt+i] = missing_val;
      }
      else
      {
        g_buf[Gpars[n].fstpt+i] = v;
      }
    }
  }
/*  else (future format support)
  {
  } */
  return;
}
