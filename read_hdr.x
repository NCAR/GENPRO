/* int read_hdr(void)
 *  Read and decode needed parameters from a GENPRO header file */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997 */

# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "g2n.h"

/*---------------------------------------------------------------------------*/
int read_hdr(void)

{

  static char projline[81];  /* space for PROJECT line (for later decode) */
  char line[HDR_LEN+1];      /* current input line */
  char *lin;                 /* pointer to line */
  char key[KEYLEN] ;         /* present keyword */
  char *ky;                  /* pointer to key */
  int go_flag = TRUE;        /* All header information OK, if true */
  size_t num = HDR_LEN;      /* number of characters to get */
  int i, j, n;               /* counter, index */
  int new;                   /* new RATE found, if TRUE */
  int lrate = 0;             /* local RATE storage */
  int tsample = 0;           /* SAMPLE total */
  int trate = 0;             /* RATE total */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  ky = &key[0];
  lin = &line[0];

/* Read all the header record lines (until ENDHD). */
  while (get_hrec(line,num) != 0)
  {
/*  I need to tell string functions where the end-of-line is. */
    line[HDR_LEN]= '\0';
/*  Count number of logical header records */
    nhrec++;
/*  One can put here a check for GENPRO-II format.  If the first logical
 *   header record does not have the string "BEGINHD", it is not a
 *   GENPRO-II file (probably not a GENPRO file at all or one in GENPRO-I
 *   format.)  If it's GENPRO-I, one can then use another routine to decode
 *   its unique data.  For now, I'll just quit.
 */
    if ((nhrec == 1) && (strstr(line,Keyword[0]) == NULL))
    {
      fprintf(stderr,"%s:  Input file is not in GENPRO-II format.  Quitting.\n",progname);
      cleanup();
    }

/*  Search for non-printing characters in the line. */
    if (nprint(line))
    {
      fprintf (stderr,"%s:read_hdr:  Warning:  ",progname);
      fprintf (stderr,"Found non-printing character(s) in\n");
      fprintf (stderr,"  GENPRO logical header record number %d which follows between <>.\n",nhrec);
      fprintf (stderr,"  <%s>\n",line);
/*  ####################################################################
 *      Ask for approval to continue ?
 *  #################################################################### */
    }
/*  Search for leading keywords  */
    if (get_lkey(lin,ky) == TRUE)
    {
/*   Acknowledge BEGINHD */
      if (strcmp(ky,Keyword[0]) == 0)
      {
        Kwfound = Kwfound | BEGINHD;
      }
/*   Save the PROJECT line */
      else if (strcmp(ky,Keyword[1]) == 0)
      {
        strcpy(projline,line);
/*   Decode the PROJECT line (if the COMMENT line has been read) */
        if ((Kwfound & COMMENT) == COMMENT)
          get_proj(projline);
        Kwfound = Kwfound | PROJECT;
      }
/*   Decode PRDATE */
      else if (strcmp(ky,Keyword[2]) == 0)
      {
        get_prd(lin);
        Kwfound = Kwfound | PRDATE;
      }
/*   Decode COMMENT */
      else if (strcmp(ky,Keyword[3]) == 0)
      {
        get_comment(lin);
/*   Decode the PROJECT line (if it has been read) */
        if ((Kwfound & PROJECT) == PROJECT)
          get_proj(projline);
        Kwfound = Kwfound | COMMENT;
      }
/*   Decode EXDATE */
      else if (strcmp(ky,Keyword[4]) == 0)
      {
        get_exd(lin);
        Kwfound = Kwfound | EXDATE;
      }
/*   Decode EXTIME */
      else if (strcmp(ky,Keyword[5]) == 0)
      {
        get_ext(lin);
        Kwfound = Kwfound | EXTIME;
      }
/*   Decode JOBID */
      else if (strcmp(ky,Keyword[6]) == 0)
      {
        get_jobid(lin);
        Kwfound = Kwfound | JOBID;
      }
/*   Decode LOGBIT */
      else if (strcmp(ky,Keyword[7]) == 0)
      {
        get_logbit(lin);
        Kwfound = Kwfound | LOGBIT;
      }
/*   Decode DATLOG */
      else if (strcmp(ky,Keyword[8]) == 0)
      {
        get_datlog(lin);
        Kwfound = Kwfound | DATLOG;
      }
/*   Decode DATSIZ */
      else if (strcmp(ky,Keyword[9]) == 0)
      {
        get_datsiz(lin);
        Kwfound = Kwfound | DATSIZ;
      }
/*   Decode APPVAR -- GENPRO variable names */
      else if (strcmp(ky,Keyword[10]) == 0)
      {
        i = get_names(lin);
/*  If the return from get_names = -1, I've exceeded my array size. */
        if (i == -1)
        {
          fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
          fprintf(stderr,"Variable array size (NVARS) exceeded.\n");
          fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
          cleanup();
        }
        Kwfound = Kwfound | APPVAR;
      }
/*   Decode ORDVAR -- LETVAR parameter order */
      else if (strcmp(ky,Keyword[11]) == 0)
      {
        nOsk = get_oskeys(lin);
        Kwfound = Kwfound | ORDVAR;
      }
/*   Decode LETVAR -- variable parameters */
      else if (strcmp(ky,Keyword[12]) == 0)
      {
        i = get_lparms(line);
        if (!(i < 0))
        {
          Kwfound = Kwfound | LETVAR;
        }
      }
/*   Decode ENDHD */
      else if (strcmp(ky,Keyword[13]) == 0)
      {
        Kwfound = Kwfound | ENDHD;

/*   At this point, if this is an NCAR GENPRO-II file, I need to skip to
 *    the end of the physical header file.  NCAR GENPRO-II header records
 *    consist of 10 logical 80-character records (800 characters) per
 *    physical record.  U of Wyoming header records (logical and physical)
 *    have 80 characters each, so this is not necessary. */
        if (G_Format == Format_NCAR)
          while ((nhrec % 10) != 0)
          {
            (void)get_hrec(line,num);
            nhrec++;
          }
        break;
      }
      else
      {
        fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
        fprintf(stderr,"unknown keyword.\n");
        fprintf(stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
        return FALSE;
      }
    }
  }

/* --------------------------------------------------------------- */
/* Test here to make sure I have everything I need from the header */
/* --------------------------------------------------------------- */
  go_flag = TRUE;

/*  Critical global header parameters */
/* LOGBIT */
  if (logbit == PRE_LOGBIT)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
    fprintf(stderr,"LOGBIT not found in GENPRO header.\n");
    go_flag = FALSE;
  }
/* DATLOG */
  if (datlog == PRE_DATLOG)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
    fprintf(stderr,"DATLOG not found in GENPRO header.\n");
    go_flag = FALSE;
  }
/* DATSIZ */
  if (datsiz == PRE_DATSIZ)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
    fprintf(stderr,"DATSIZ not found in GENPRO header.\n");
    go_flag = FALSE;
  }
/* Compare physical data record size to logical data record size */
  if (logbit*datlog > datsiz)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
    fprintf(stderr,"Inconsistent GENPRO header data:\n   LOGBIT*DATLOG > DATSIZ.\n");
    go_flag = FALSE;
  }
/* Check to see if I have all needed keywords */
  if ((Kwfound & Kwcritical) != Kwcritical)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:\n",progname);
    fprintf(stderr,"   Kwcritical = %o\n",Kwcritical);
    fprintf(stderr,"      Kwfound = %o\n",Kwfound);
    fprintf(stderr,"   Key information missing from the GENPRO header.\n");
    go_flag = FALSE;
  }
/* Compare logical data record size to sum of all variable rate/sample data */
  for (i=0;i<nvars;i++)
  {
/* Totalize RATE and SAMPLE */
    trate += Gpars[i].rate*Gpars[i].bits;
    tsample += Gpars[i].sample*Gpars[i].bits;
/* Begin diagnostic print
    fprintf (stderr,"%s:read_hdr: tsample = %i, trate = %i, i = %i\n"
         ,progname,tsample,trate,i);
   End diagnostic print
 */
  }
/* Begin diagnostic print
  fprintf (stderr,"%s:read_hdr: tsample = %i, trate = %i, logbit = %i\n",
      progname,tsample,trate,logbit);
   End diagnostic print
 */
/* If SAMPLE total but not RATE total matches LOGBIT, move each SAMPLE value
     into its corresponding RATE position so I can decode the data correctly.
 */
  if (trate != logbit && tsample == logbit)
  {
    fprintf(stderr,"%s:read_hdr:  Warning:  ",progname);
    fprintf(stderr,"Sum of GENPRO header variable's SAMPLE parameters\n"
        "       matches LOGBIT--using SAMPLEs instead of RATEs.\n");
    for (i=0;i<nvars;i++)
    {
      Gpars[n].rate = Gpars[n].sample;
    }
  }
  if (tsample != logbit && trate != logbit)
  {
    fprintf(stderr,"%s:read_hdr:  Fatal:  ",progname);
    fprintf(stderr,"GENPRO header's logical record size (%d).\n", logbit);
    fprintf(stderr,"  matches neither the sum of the RATE (%d) nor SAMPLE"
          " (%d) parameters.\n",trate, tsample);
    go_flag = FALSE;
  }

/*  Load the array of unique output rates */
  for (i=0;i<nvars;i++)
  {
    lrate = Gpars[i].rate;
    if (nRates == 0)
    {
      Rates[nRates] = lrate;
      nRates++;
    }
    else
    {
      new = TRUE;
      for (j=0;j<nRates;j++)
      {
        if (Rates[j] == lrate)
        {
          new = FALSE;
          break;
        }
      }
      if (new == TRUE)
      {
        if (nRates < NRATES)
        {
          Rates[nRates] = lrate;
          nRates++;
        }
        else
        {
          fprintf (stderr,"%s:read_hdr:  Exceeded maximum number"
              " of unique output rates.",progname);
          fprintf (stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
          cleanup();
        }
      }
    }
  }

/*  A strongly-desired parameter is PROJECT, which, for NCAR files, has the
 *   project number, flight number and segment, and project name.  It is not
 *   necessary for converting the data, however, so it is not a critical
 *   parameter.  If any of the following are missing, it makes sense for the
 *   user to enter these at run-time (or they could be preset to something):
 *     COMMENT -- Format type (NCAR or Wyoming)
 *     PRDATE  -- Flight date
 *     PROJECT -- Project number
 *                Project title (e.g., WISPIT)
 *                Flight type, flight number and segment (e.g. RF03A)
 */
/*  Warn if any PROJECT (projno, fltno, etc.) information is missing. */
  if ((Kwfound & PROJECT) != PROJECT)
  {
    fprintf (stderr,"%s:read_hdr:  Warning:  ",progname);
    fprintf (stderr,"Project information missing from the GENPRO header.\n");
  }
/* ######################################################################### *
 *  Code goes here to check the above, warn user and preset them, if necessary.
 * ######################################################################### */

/*  Check that all the variables' critical parameters are present */
/*  if ( ! par_validate()) <--  Will this work? */
  if (par_validate() != TRUE)
  {
    go_flag = FALSE;
  }
/* Check to see if it's worth doing any more work */
  if (go_flag == FALSE) return go_flag;

/* Add/change time variables, if necessary */
  if (time_var() != TRUE)
  {
    go_flag = FALSE;
  }

/* Handle PMS-1D variables here */
  setup_pms1d();
/* #############  Test purposes: don't go on for now */
/*  go_flag = FALSE;
  printf ("%s:read_hdr:  Stopping here for test purposes.\n",progname);
 */

/* Sort the array of output rates */
/*  (later development--I haven't gotten this to work yet)
 *  if (nRates > 1)
 *    qsort((char *)Rates, nRates, sizeof(int), intcompare);
 */

/*  Information summary goes here */
/*  if (infoflag)
  { */
    printf ("%s:read_hdr:  Number of GENPRO variables (from header) = %d.\n",progname,nvars);
    printf ("%s:read_hdr:  Number of GENPRO logical header records read = %d.\n",progname,nhrec);
/*  }
 */
  return go_flag;
}
/*---------------------------------------------------------------------------*/
/* int intcompare(int *i, int *j)
 *  Compare two integers
 *   Example `intcompare' from man page */
/*
int intcompare(int *i, int *j)

{
  return(*i - *j);
}
 */
