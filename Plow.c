/* void Plow(void)
 *  Plow through the data and write netCDF output */
/* Modified Nov 29, 2010 Janine Aquino 
    check for first record time before start time of flight and modify date
    accordingly (assume midnight rollover)*/

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997 */

/* How to do it:
 *  Decode and convert all required GENPRO input data and put values into
 *   the output buffer.
 *  First record:
 *   Decode the reference time variable(s); create and output the value for
 *    base_time.
 *  All records:
 *   Use reference time variables to update and output time_offset.
 *  Write values into netCDF file.
 *  At EOF:
 *   Close netCDF file and return
 */

# include <stdio.h>
# include <string.h>
# include <errno.h>
# include <time.h>

# include "netcdf.h"
# include "g2n.h"

# define ERR  (-1)

void Plow(void)

{

  int i;                        /* counter */
  static time_t BaseTime;       /* space for base_time structure */
  struct tm *gt;                /* pointer to UTC space */
  static int firstTime = TRUE;  /* Initialize, if true */
  static int err_cnt = 0;       /* netCDF error counter */
  static int RecordNumber = 0;  /* netCDF output record number */
  static int DayIncrement = 0;  /* Number of days to increment date */

/*

	      * * * * * * * * * * * * * * *
	     *                             *
	    *  Executable code starts here  *
	     *                             *
	      * * * * * * * * * * * * * * *
 */
/* Get a logical record. */
  while ((i = get_ldrec()) != NO)
  {
    /* Decode all the data in a logical record. */
    /*  (Note:  I'm doing each variable separately, so I can easily adapt the
     *           code later to do a subset of the variable list.
     *  decode standard GENPRO variables */
	for (i=0;i<nvars;i++)
	{
    /*  incorporate output flag (Version 2.4)  RLR 971223 */
	  if (Gpars[i].output == YES) d_convert(i);
	}
    /*  decode PMS-1D variables (Version 2.4)  RLR 971224 */
	for (i=0;i<n_pms1d_probes;i++)
	{
	  if (Pgpars[i].output == YES) d_P_convert(i);
	}

    /* Set up time variables the first time through. */
    if (firstTime == TRUE)
    {
    /*  Check if time of first record is before start time of flight */
    /*  If it is, then assume midnight rollover and increment date one day */
      int prtime_Seconds = prtime[0]*60*60 + prtime[1]*60 + prtime[2];
      int g_buf_Seconds = (int)g_buf[Gpars[ihour].fstpt]*60*60 + (int)g_buf[Gpars[iminute].fstpt]*60 + (int)g_buf[Gpars[isecond].fstpt];
      if (g_buf_Seconds <  prtime_Seconds && strcmp(Gpars[ihour].title, "UNALTERED TAPE TIME") == 0)
      {
         prdate[0] = prdate[0] + 1;
      }
      else if (g_buf_Seconds + prtime_Seconds >= 24*60*60 && strcmp(Gpars[ihour].title, "TIME FROM TPTIME") == 0)
      {
         prdate[0] = prdate[0] + 1;
      }
      else if (strcmp(Gpars[ihour].title, "TIME FROM TPTIME") != 0 && strcmp(Gpars[ihour].title, "UNALTERED TAPE TIME") != 0)
      {
          printf("Unkown time type %s! Date incrementation will occour!\n");
      } 
      firstTime = FALSE;
      StartFlight.tm_mday = prdate[0];
      StartFlight.tm_mon = prdate[1] - 1;
      StartFlight.tm_year = prdate[2];
      StartFlight.tm_isdst = -1;
      StartFlight.tm_hour = 0;
      StartFlight.tm_min  = 0;
      StartFlight.tm_sec  = 0;

      BaseTime = mktime(&StartFlight);
      gt = gmtime(&BaseTime);
      StartFlight.tm_hour = StartFlight.tm_hour - gt->tm_hour;
      BaseTime = mktime(&StartFlight);
/*  Write date to netCDF file */
      (void)ncvarput1(ncid, baseTimeID, NULL, (void *)&BaseTime);
/*  Extra_Times is set in .h file */
      if (Extra_Times == TRUE)
      {
/*  Save initial time_offset. */
        g_buf[Gpars[tof].fstpt] = g_buf[Gpars[time_index].fstpt];
/*  Calculate HOUR, MINUTE and SECOND. */
        to_hms(&g_buf[Gpars[time_index].fstpt],&g_buf[Gpars[ihour].fstpt],&g_buf[Gpars[iminute].fstpt],&g_buf[Gpars[isecond].fstpt]);
      }
      else
      {
/*  Calculate and save initial time_offset. */
        g_buf[Gpars[tof].fstpt] = 3600*g_buf[Gpars[ihour].fstpt] + 60*g_buf[Gpars[iminute].fstpt] + g_buf[Gpars[isecond].fstpt];
      }
      StartFlight.tm_hour = (int)g_buf[Gpars[ihour].fstpt];
      StartFlight.tm_min  = (int)g_buf[Gpars[iminute].fstpt];
      StartFlight.tm_sec  = (int)g_buf[Gpars[isecond].fstpt];
    }
    else
/* Update the time variables. */
    {
/*  Time offset. */
      g_buf[Gpars[tof].fstpt] += 1.0;
/*  HOUR, MINUTE and SECOND, if necessary. */
      if (Extra_Times == TRUE)
      {
        to_hms(&g_buf[Gpars[tof].fstpt],&g_buf[Gpars[ihour].fstpt],&g_buf[Gpars[iminute].fstpt],&g_buf[Gpars[isecond].fstpt]);
      }
    }
/* Write out the data record. */
    if (ncrecput(ncid, RecordNumber, data_p) == ERR)
    {
      if (errno == ENOSPC)
      {
        fprintf(stderr,"Disk full, closing file and exiting...\n");
        return;
      }
      fprintf(stderr,
        "WriteNetCDF: ncrecput failure, RecordNumber = %d, errno = %d\n",
        RecordNumber, errno);
      if (++err_cnt > 8)
      {
        fprintf(stderr,
          "Too many write errors, closing file and exiting...\n");
        return;
      }
    }
/* Echo time to user, if requested. */
    if (echo_time == YES)
      printf("%s:  Time:  %02d:%02d:%02d\r",progname,
       (int)g_buf[Gpars[ihour].fstpt],
       (int)g_buf[Gpars[iminute].fstpt],
       (int)g_buf[Gpars[isecond].fstpt]);
    ++RecordNumber;
  }
  return;
}
