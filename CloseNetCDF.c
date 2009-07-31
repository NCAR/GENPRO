/* void CloseNetCDF(void)
 *  Finish up and close both GENPRO and netCDF files */

/* Original code written by Chris Webster for dap2cdf.c */
/* Adapted for GENPRO to netCDF program by Ron Ruth    November 1994 */

/* COPYRIGHT:      University Corporation for Atmospheric Research, 1998 */

/*  This version written by RLR to handle COS-blocked files */

# include <stdio.h>
# include <string.h>
# include <time.h>
# include <errno.h>

# include "netcdf.h"
# include "g2n.h"

void CloseNetCDF(void)

{

  int h, m, s;

/*
 *
 *       * * * * * * * * * * * * * * *
 *      *                             *
 *     *  Executable code starts here  *
 *      *                             *
 *       * * * * * * * * * * * * * * *
 */
/* Check for bogus end time */
  if ((h=(int)g_buf[Gpars[ihour].fstpt])   < 0) h=-1;
  if ((m=(int)g_buf[Gpars[iminute].fstpt]) < 0) m=-1;
  if ((s=(int)g_buf[Gpars[isecond].fstpt]) < 0) s=-1;
/* Update time interval in netCDF header */
  sprintf(buffer, "%02d:%02d:%02d-%02d:%02d:%02d",
    StartFlight.tm_hour,
    StartFlight.tm_min,
    StartFlight.tm_sec,
    (int)g_buf[Gpars[ihour].fstpt],
    (int)g_buf[Gpars[iminute].fstpt],
    (int)g_buf[Gpars[isecond].fstpt]);
/*  Following test should not be needed unless I am processing more
 *   than one time period, so I'll ignore it for now.
  if ((len = strlen(buffer) + 1) > DEFAULT_TI_LENGTH)
    ncredef(ncid);
 */
  (void)ncattput(ncid,NC_GLOBAL,"TimeInterval",NC_CHAR,strlen(buffer)+1,(void *)buffer);

/* Echo time period, if requested */
  if (batch_flag == FALSE) printf ("%s:  Time Interval:  %s\n",progname,buffer);

/* Close the netCDF file */
  (void)ncclose(ncid);

/* Close the GENPRO file */
  if (DiskData == STRIPPED)
    close (Gp_fp);
  if (DiskData == COS_BLOCKED)
    crayclose(&infd);

/* Done. */
  return;
}
