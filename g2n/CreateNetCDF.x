/* void CreateNetCDF(void)
 *  Create netCDF file and set up its header */

/* Original code written by Chris Webster for dap2cdf.c */
/* Adapted for GENPRO to netCDF program by Ron Ruth    November 1994 */

/*  COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997 */

# include <stdlib.h>
# include <string.h>
# include <stdio.h>
# include <time.h>

# include "netcdf.h"
# include "g2n.h"

void CreateNetCDF(void)

{

  time_t        t;
  struct tm     tm;
  register int  i, k;
  int           j = 0;
  long          lvalue;
  int           ivalue;

/*
 *
 *       * * * * * * * * * * * * * * *
 *      *                             *
 *     *  Executable code starts here  *
 *      *                             *
 *       * * * * * * * * * * * * * * *
 */
/* Create the netCDF file */
    if ((ncid = nccreate(outfile, NC_CLOBBER)) == (-1))
  {
    fprintf(stderr,"%s:CreateNetCDF:   Can't open %s.\n",progname,outfile);
    close (Gp_fp);
    exit(1);
  }
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  ncid = %i\n",ncid);

/* Define dimensions */
/*  1 -- Unlimited (time) */
  TimeDim  = ncdimdef(ncid, "Time", NC_UNLIMITED);

/*  2 -- Rates */
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  number of defined rates: %i\n",nRates);
  for (i=0;i<nRates;i++)
  {
    sprintf (buffer,"sps%d",Rates[i]);
/*  ###########  Diagnostic print */
    printf ("CreateNetCDF:  netCDF defined rate: %i\n",Rates[i]);
    printf ("CreateNetCDF:  netCDF defined rate: %s\n",buffer);
    DimRateID[i] = ncdimdef(ncid, buffer, Rates[i]);
  }

/*  3 -- Vectors */
  DimVectorID[0] = ncdimdef(ncid, "Vector16", 16);
  DimVectorID[1] = ncdimdef(ncid, "Vector32", 32);
  DimVectorID[2] = ncdimdef(ncid, "Vector64", 64);

/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  Created all dimdefs\n");

/* Global Attributes */
  strcpy(buffer, "NCAR Research Aviation Facility");
  (void)ncattput(ncid,NC_GLOBAL,"Source",NC_CHAR,strlen(buffer)+1,(void *)buffer);

  strcpy(buffer, "P.O. Box 3000, Boulder, CO 80307-3000");
  (void)ncattput(ncid,NC_GLOBAL,"Address",NC_CHAR,strlen(buffer)+1,(void *)buffer);

  strcpy(buffer, "(303) 497-1030");
  (void)ncattput(ncid,NC_GLOBAL,"Phone",NC_CHAR, strlen(buffer)+1,(void *)buffer);

  strcpy(buffer, "NCAR-RAF/nimbus");
  (void)ncattput(ncid,NC_GLOBAL,"Conventions",NC_CHAR,strlen(buffer)+1,(void *)buffer);

/*  I won't use this one, since GENPRO data are usually production output. */
/*  strcpy(buffer, "This file contains PRELIMINARY DATA that are NOT to be used for critical analysis.");
  (void)ncattput(ncid,NC_GLOBAL,"WARNING",NC_CHAR,strlen(buffer)+1,(void *)buffer);
 */
/* Date and time of original processing */
  tm.tm_year = exdate[2];
  tm.tm_mon  = exdate[1] - 1;
  tm.tm_mday = exdate[0];
  tm.tm_hour = extime[0];
  tm.tm_min  = extime[1];
  tm.tm_sec  = extime[2];
  tm.tm_wday  = 0;
  tm.tm_yday  = 0;
  tm.tm_isdst = 0;
  (void)strftime(buffer, 128, "%h %d %T UTC %Y", &tm);
  (void)ncattput(ncid,NC_GLOBAL,"DateProcessed",NC_CHAR,strlen(buffer)+1,(void *)buffer);
/* Job ID */
  sprintf(buffer,"%s",jobid);
  (void)ncattput(ncid,NC_GLOBAL,"JOBID",NC_CHAR,strlen(buffer)+1,(void *)buffer);
/* Date and time this conversion is being done */
  {
  t = time(0);
  tm = *gmtime(&t);
  (void)strftime(buffer, 128, "%h %d %R UTC %Y", &tm);
  (void)ncattput(ncid, NC_GLOBAL, "DateConvertedFromGENPRO", NC_CHAR,
                    strlen(buffer)+1, (void *)buffer);
  }
  strcpy(buffer, project);
  (void)ncattput(ncid,NC_GLOBAL,"ProjectName",NC_CHAR,strlen(buffer)+1,(void *)buffer);
/* Aircraft tail number */
  (void)ncattput(ncid, NC_GLOBAL,"Aircraft",NC_CHAR,strlen(aircraft)+1,(void *)aircraft);
/* Project number */
  sprintf(buffer, "%d", projno);
  (void)ncattput(ncid, NC_GLOBAL,"ProjectNumber",NC_CHAR,strlen(buffer)+1,(void *)buffer);
/* Flight type, number and segment */
  if (fltno < 10)
  {
    if (segment != ' ' && segment != '\0')
    {
      sprintf(buffer,"%cF0%d%c",ftype,fltno,segment);
    }
    else
    {
      sprintf(buffer,"%cF0%d",ftype,fltno);
    }
  }
  else
  {
    if (segment != ' ' && segment != '\0')
    {
      sprintf(buffer,"%cF%d%c",ftype,fltno,segment);
    }
    else
    {
      sprintf(buffer,"%cF%d",ftype,fltno);
    }
  }
  (void)ncattput(ncid,NC_GLOBAL,"FlightNumber",NC_CHAR,strlen(buffer)+1,(void *)buffer);

/* Flight date */
  sprintf(buffer,"%02d/%02d/19%d",prdate[1],prdate[0],prdate[2]);
  (void)ncattput(ncid,NC_GLOBAL,"FlightDate",NC_CHAR,strlen(buffer)+1,(void *)buffer);

/* Create one blank time segment */
  sprintf(buffer, "  :  :  -  :  :       ");
  (void)ncattput(ncid,NC_GLOBAL,"TimeInterval",NC_CHAR,strlen(buffer)+1,(void *)buffer);

/* For all output data:
 *  First netCDF dimension is time dimension.
 *  Second is Rate in sps.
 *  Third is VectorLength (if needed).
 */
  dims[0] = TimeDim;

/* Time Variables, here to keep Gary/WINDS happy  */
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  ncvardef baseTime\n");
  baseTimeID = ncvardef(ncid, Gpars[tbt].name, NC_LONG, 0, 0);
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  baseTimeID = %i, tbt = %i\n",baseTimeID, tbt);
  (void)ncattput(ncid,baseTimeID,"long_name",NC_CHAR,strlen(Gpars[tbt].title)+1,Gpars[tbt].title);
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  ncvardef timeOffset\n");
  timeOffsetID = ncvardef(ncid, Gpars[tof].name, NC_FLOAT, 1, dims);
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  timeOffsetID = %i, tof = %i\n",timeOffsetID, tof);
  (void)ncattput(ncid,timeOffsetID,"long_name",NC_CHAR,strlen(Gpars[tof].title)+1,Gpars[tof].title);
  data_p[j] = &g_buf[Gpars[tof].fstpt];
/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  data_p[%i] = %i for %s\n",j,&g_buf[Gpars[tof].fstpt],Gpars[tof].name);

/* For each GENPRO variable selected for output:
 *  - Set dimensions
 *  - define variable
 *  - Set attributes
 *  - Set data_pointer
 */
  for (i=0; i<nvars; ++i)
  {
    if (Gpars[i].output == YES)
    {
      Nvars++;
      for (k=0; k<nRates; k++)
      {
        if (Gpars[i].rate == Rates[k])
        {
          if (Gpars[i].rate == 1)
          {
            ndims = 1;
          }
          else
          {
            ndims = 2;
            dims[1] = DimRateID[k];
          }
          break;
        }
      }
/*  ###########  Diagnostic print */
      printf ("CreateNetCDF:  ndims(%i) = %i\n",i, ndims);
      printf ("CreateNetCDF:  ncvardef %s\n",Gpars[i].name);
      varid[j] = ncvardef(ncid, Gpars[i].name, NC_FLOAT, ndims, dims);
      (void)ncattput(ncid,varid[j],"units",NC_CHAR,strlen(Gpars[i].units)+1,Gpars[i].units);
      (void)ncattput(ncid,varid[j],"long_name",NC_CHAR,strlen(Gpars[i].title)+1,Gpars[i].title);
      lvalue = Gpars[i].rate;
      (void)ncattput(ncid,varid[j],"OutputRate",NC_LONG,1,&lvalue);
      (void)ncattput(ncid,varid[j],"missing_value",NC_FLOAT,1,&missing_val);
  /*  Note:  Following index is one more, because time_offset comes first. */
      data_p[++j] = &g_buf[Gpars[i].fstpt];
/*  ###########  Diagnostic print */
      printf ("CreateNetCDF:  data_p[%i] = %i for %s\n",j,&g_buf[Gpars[i].fstpt],Gpars[i].name);
    }
  }

/*  ###########  Diagnostic print */
  printf ("CreateNetCDF:  Defined all standard variables\n");

/* Handle reformatted PMS-1D probes, if any (Rev 2.4  RLR 971229) */
  ndims = 3;
  for (i=0; i<n_pms1d_probes; ++i)
  {
    Nvars++;
    for (k=0; k<nRates; k++)
    {
      if (Pgpars[i].rate == Rates[k])
      {
/*  ###########  Diagnostic print */
        printf ("CreateNetCDF:  Creating dims for probe %i\n",i);
        dims[1] = DimRateID[k];
        break;
      }
    }
    dims[2] = DimVectorID[pms1d[i].vectorID];
/*  ###########  Diagnostic print */
    printf ("CreateNetCDF:  ncvardef %s\n",Pgpars[i].name);
    varid[j] = ncvardef(ncid, Pgpars[i].name, NC_FLOAT, ndims, dims);
    (void)ncattput(ncid,varid[j],"units",NC_CHAR,strlen(Pgpars[i].units)+1,Pgpars[i].units);
    (void)ncattput(ncid,varid[j],"long_name",NC_CHAR,strlen(Pgpars[i].title)+1,Pgpars[i].title);
/*    (void)ncattput(ncid,varid[j],"Category",NC_CHAR, 9,"PMS Probe"); */
    (void)ncattput(ncid,varid[j],"missing_value",NC_FLOAT,1,&missing_val);
    lvalue = Pgpars[i].rate;
    (void)ncattput(ncid,varid[j],"OutputRate",NC_LONG,1,&lvalue);
    lvalue = pms1d[i].num_defined_cells;
    (void)ncattput(ncid,varid[j],"VectorLength",NC_LONG,+1,&lvalue );
    if (pms1d[i].cell_offset == YES)
    {
      ivalue = 1;
      (void)ncattput(ncid,varid[j],"FirstBin",NC_INT,1,&ivalue);
      (void)ncattput(ncid,varid[j],"LastBin",NC_INT,1,&pms1d[i].num_found_cells);
    }
    else
    {
      ivalue = 1;
      (void)ncattput(ncid,varid[j],"FirstBin",NC_INT,1,&ivalue);
      ivalue = pms1d[i].num_found_cells - 1;
      (void)ncattput(ncid,varid[j],"LastBin",NC_INT,1,&ivalue);
    }
/*      (void)ncattput(ncid,varid[j],"CellSizes",NC_FLOAT,i,cellSize); */
/*      (void)ncattput(ncid,varid[j],"CellSizeUnits",NC_CHAR,NC_CHAR,12,"micrometers"); */
/*  Note:  Following index is one more, because time_offset comes first. */
    data_p[++j] = &p_buf[Pgpars[i].fstpt];
/*  ###########  Diagnostic print */
    printf ("CreateNetCDF:  data_p[%i] = %i for %s\n",j,&p_buf[Pgpars[i].fstpt],Pgpars[i].name);
  }

/* Create extra time variables, if necessary */
/*  Note:  For this code, missing but essential time variables have been
 *   put at the end of the GENPRO converted data buffer. */
  if (Extra_Times == TRUE)
  {
    ndims = 1;
/*  (The following can be a "for" loop, because `ihour', `iminute' and
      `isecond' are all consecutive indices created in `time_var.c.') */
    for (i=ihour;i<=isecond;i++)
    {
/*  ###########  Diagnostic print */
      printf ("CreateNetCDF:  ncvardef %s\n",Gpars[i].name);
      varid[j] = ncvardef(ncid,Gpars[i].name,NC_FLOAT,ndims,dims);
      (void)ncattput(ncid,varid[j],"units",NC_CHAR,strlen(Gpars[i].units)+1,Gpars[i].units);
      (void)ncattput(ncid,varid[j],"long_name",NC_CHAR,strlen(Gpars[i].title)+1,Gpars[i].title);
      (void)ncattput(ncid,varid[j],"missing_value",NC_FLOAT,1,&missing_val);
      lvalue = Gpars[i].rate;
      (void)ncattput(ncid,varid[j],"OutputRate",NC_LONG,1,&lvalue);
/*  Note:  Following index is one more, because time_offset comes first. */
      data_p[++j] = &g_buf[Gpars[i].fstpt];
/*  ###########  Diagnostic print */
      printf ("CreateNetCDF:  data_p[%i] = %i for %s\n",j,&g_buf[Gpars[i].fstpt],Gpars[i].name);
    }
    Nvars += 3;
  }

  printf ("%s:CreateNetCDF: Number of GENPRO variables output to netCDF file = %i.\n",progname,Nvars);

  (void)ncendef(ncid);

}  /* END CREATENETCDF */
