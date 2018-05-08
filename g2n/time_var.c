/* int time_var(void)
 *  Add or change time variables to comply with the netCDF format */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1997 */
/*  Revision 1 by RLR  (fix some more U Wyoming differences)     4 Aug 1997  */
/*  Revision 2 by RLR  (use time_units defined in "g2n.h")       5 Dec 1997  */
/*  Revision 2 by RLR  (improve some of the code via "for"loops) 5 Dec 1997  */

/* How to do it:
 *  Define base_time and time_offset for netCDF output file.
 *  Create space in the GENPRO output buffer for time_offset.
 *  NCAR:  If any of the variables HR, MIN or SEC is missing, search for the
 *   time variables PTIME or TPTIME.  If they are absent, too, return FALSE,
 *   because I cannot go on.  (netCDF requires a time variable.)  If time is
 *   present, use (in order of preference):  HR, MIN, SEC or PTIME or TPTIME.
 *   If the variables HR, MIN and SEC are found, rename them to HOUR, MINUTE
 *   and SECOND.  If they cannot be found, create them, add them to the Gpar
 *   array and preset their needed parameters.  Set a flag so I know to look
 *   for them.  Set indices for time.
 *  UW:  If "totalsec" and "time" are missing, return FALSE, because I
 *   cannot go on.  (netCDF requires a time variable.)  Otherwise, use
 *   "totalsec" (first) or "time" as the reference time.  Create the
 *   variables HOUR, MINUTE and SECOND, add them to the Gpar array and
 *   preset their needed parameters.  Set a flag so I know where to look
 *   for them.  Set the indices for time.
 */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

int time_var(void)

{

  int ipt, itp;   /* indices for PTIME and TPTIME */
  int i;          /* counter index */
  int ovars = 0;  /* Number of output variables */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Check for available space */
  if (nvars+2 >= NVAR)
  {
    fprintf(stderr,"%s:time_var:  Fatal:  ",progname);
    fprintf(stderr,"time_var:  No room for time variables.\n");
    fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
    return FALSE;
  }
/* time_offset (placed immediately after the GENPRO variables) */
  ovars = tof = nvars;
  strcpy(Gpars[tof].name,"time_offset");
  strcpy(Gpars[tof].title,"Seconds since base_time.");
  Gpars[tof].rate = 1;
  Gpars[tof].fstpt = Gpars[tof-1].fstpt + Gpars[tof-1].rate;
/* base_time (placed immediately after time_offset) */
  tbt = ++ovars;
  strcpy(Gpars[tbt].name,"base_time");
  strcpy(Gpars[tbt].title,"Seconds since Jan 1, 1970.");
  Gpars[tbt].rate = 1;
  Gpars[tbt].fstpt = Gpars[tbt-1].fstpt + Gpars[tbt-1].rate;
/* Essential time variables (HOUR, MINUTE, SECOND) */
  ovars++;
/* Search for extant time variables */
  ihour = name_idx("HOUR" );
  iminute = name_idx("MINUTE");
  isecond = name_idx("SECOND");
  if (G_Format == Format_NCAR)
/*  NCAR data format */
  {
/*   If I don't have time variables, create them beyond the GENPRO list
 *     and preset their essential parameters */
    if (ihour < 0 || iminute < 0 || isecond < 0)
    {
      if ((ihour=name_idx("HR" )) < 0 ||
         (iminute=name_idx("MIN")) < 0 ||
         (isecond=name_idx("SEC")) < 0 )
      {
        if (ovars+2 < NVAR)
        {
           if ( (ipt=name_idx("PTIME")) >= 0 ||
            (itp=name_idx("TPTIME")) >= 0 )
          {
            Extra_Times = TRUE;
            for (i=0;i<3;i++)
            {
              strcpy(Gpars[ovars+i].name,time_vars[i]);
              strcpy(Gpars[ovars+i].units,time_units[i]);
              Gpars[ovars+i].rate = 1;
              Gpars[ovars+i].fstpt = Gpars[ovars+i-1].fstpt +
                Gpars[ovars+i-1].rate;
            }
            if (ipt >= 0)
            {
              time_index = ipt;
              for (i=0;i<3;i++)
              {
                strcpy(Gpars[ovars+i].title,"TIME FROM PTIME");
              }
            }
            else
            {
              time_index = itp;
              for (i=0;i<3;i++)
              {
                strcpy(Gpars[ovars+i].title,"TIME FROM TPTIME");
              }
            }
            ihour = ovars++;
            iminute = ovars++;
            isecond = ovars;
          }
          else
/*      I have no time variables to work with! */
          {
            fprintf (stderr,"%s:time_var:  Fatal:  ",progname);
            fprintf(stderr,"No time variables found.\n");
            fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
            return FALSE;
          }
        }
        else
/*     I have no room for extra time variables! */
        {
          fprintf (stderr,"%s:time_var:  Fatal:  ",progname);
          fprintf(stderr,"time_var:  No room for time variables.\n");
          fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
          return FALSE;
        }
      }
      else
      {
/*   Change name of extant time variables */
        strcpy(Gpars[ihour].name,time_vars[0]);
        strcpy(Gpars[iminute].name,time_vars[1]);
        strcpy(Gpars[isecond].name,time_vars[2]);
      }
    }
  }
  else if ((G_Format == Format_UW) || (G_Format == Format_UW_IEEE) || (G_Format == Format_N48RF))
/*  Wyoming data format */
  {
/*   If I don't have necessary time variables, create them and add them
 *     beyond the extant GENPRO list and preset their essential parameters */
    if (ihour < 0 || iminute < 0 || isecond < 0)
    {
      if (ovars+2 < NVAR)
      {
         if ( (ipt=name_idx("totalsec")) >= 0 )
        {
          Extra_Times = TRUE;
          time_index = ipt;
          for (i=0;i<3;i++)
          {
            strcpy(Gpars[ovars+i].name,time_vars[i]);
            strcpy(Gpars[ovars+i].units,time_units[i]);
            strcpy(Gpars[ovars+i].title,"TIME FROM totalsec");
            Gpars[ovars+i].rate = 1;
            Gpars[ovars+i].fstpt = Gpars[ovars+i-1].fstpt +
              Gpars[ovars+i-1].rate;
          }
        }
         else if ( (ipt=name_idx("time")) >= 0 )
        {
          Extra_Times = TRUE;
          time_index = ipt;
          for (i=0;i<3;i++)
          {
            strcpy(Gpars[ovars+i].name,time_vars[i]);
            strcpy(Gpars[ovars+i].units,time_units[i]);
            strcpy(Gpars[ovars+i].title,"TIME FROM time");
            Gpars[ovars+i].rate = 1;
            Gpars[ovars+i].fstpt = Gpars[ovars+i-1].fstpt +
              Gpars[ovars+i-1].rate;
          }
        }
        else
/*   I have no time variables to work with! */
        {
          fprintf (stderr,"%s:time_var:  Fatal:  ",progname);
          fprintf(stderr,"No time variables found.\n");
          fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
          return FALSE;
        }
        ihour = ovars++;
        iminute = ovars++;
        isecond = ovars;
      }
      else
/*   I have no room for extra time variables! */
      {
        fprintf (stderr,"%s:time_var:  Fatal:  ",progname);
        fprintf(stderr,"time_var:  No room for time variables.\n");
        fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
        return FALSE;
      }
    }
  }
/*  else if (G_Format == Format_HK) */
/*  Hong Kong data format */
/*  {  */
/*   Variable names OK for Hong Kong format.  No changes needed. */
/*  }  */
  return TRUE;
}
