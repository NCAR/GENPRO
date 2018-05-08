/*
 * GENPRO to netCDF conversion software
 *  Convert a GENPRO input file to a netCDF output file
 *
 *  Written by Ronald L. Ruth
 *             RAF Data Manager
 *             Research Aviation Facility and Research Data Program
 *             Atmospheric Technology Division
 *             National Center for Atmospheric Research
 */
/*  Copyright (C) 1994, 1997, 2004 by UCAR
 *  University Corporation for Atmospheric Research
 *  All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 *
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties
 * of any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide
 * maintenance or updates for its software.
 */

# include <stdlib.h>
# include <stdio.h>
# include <string.h>
# include <signal.h>
# include <netcdf.h>
# include <time.h>

#define G2N
# include "g2n.h"

main (argc, argv)

int argc;
char **argv;

{
/*
 * Basic sanity checking
 *  In this initial implementation, the default is to write all variables
 *  to the output file.  All that is needed are input and output file
 *  names.  If they don't come in as part of the command line, the
 *  sane thing to do is ask for them.
 */

  int i;
  typedef void    (*VoidFunc)();

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Prevent buffered output to user */
  setbuf(stdout,NULL);

/* Enable ^C interrupt */
  (void)signal(SIGINT,(VoidFunc)cleanup);

/* Print a heading, including the revision date */
  strcpy(progname,argv[0]);
  printf("%s:  GENPRO to netCDF converter, version 2.7.2, rev. 20040407"
    " RLR\n\n", progname);

/* Add code here to decode command-line options, when implemented
 *  For example options, see the file `g2n.notes' */

/* Ask for input and output file names if not enough arguments
 *  were entered on the command line */
  if (argc < 3)
  {
    get_filenames();  /* (also opens input file) */
  }
  else
  {

/* Save input file name (second command-line argument) */
    strcpy(infile,argv[1]);

/* Open the input file */
/*    if (infoflag) */
    printf ("%s:  Input file is %s\n",progname,infile);

    if (GpOpen(infile) == FALSE)
    {
      fprintf(stderr,"%s:  Could not open %s\n",progname,infile);
      exit (1);
    }

/* At present, I'll assume that any command-line argument beyond the two
 *  file names is a request to continuously write (to stdout) the time
 *  being processed. */
    echo_time = (int)(argc > 3);

/* Save output file name */
    strcpy (outfile,argv[2]);

/* ########################################################### */
/* Check output file here before writing to it? */
/* ########################################################### */
  }
/*
 * Decode the GENPRO header
 */
  if (read_hdr() == TRUE)
  {
/*    if (infoflag) */
    printf ("%s:  Header OK.\n",argv[0]);
  }
  else
  {
    fprintf (stderr,"%s:  Header problem.  Cannot continue.\n",argv[0]);
    cleanup();
  }

/* Allocate standard GENPRO input data buffer */
  if ((G_Format == Format_NCAR) || (G_Format == Format_UW) || (G_Format == Format_N48RF))
  {
/*  Note:  For NCAR-style I'm assuming that GENPRO data records have
 *          32-bit integers. */
    d_buf = (unsigned long *)malloc((size_t)(datsiz/8));
    if (d_buf == NULL)
    {
      fprintf (stderr,"%s:  Could not allocate memory for GENPRO raw data.\n",progname);
      cleanup();
    }
  }
  else  /* if (G_Format == Format_UW_IEEE) */
  {
/*  Note:  For Wyoming IEEE-style I'm assuming that GENPRO data records
 *          have 32-bit IEEE floating-point numbers. */
    f_buf = (float *)malloc((size_t)(sizeof(float)*datsiz/32));
    if (f_buf == NULL)
    {
      fprintf (stderr,"%s:  Could not allocate memory for GENPRO raw data.\n",progname);
      cleanup();
    }
  }
/*  else (future format support)
  {
  } */

/* Calculate size of GENPRO converted data buffer */
  for (i=0;i<nvars;i++)
  {
/*  Don't test here to see if each variable is to be put into the output
 *   stream, because the converted data-buffer indexing is matched to the
 *   GENPRO input buffer
 *    if (Gpars[i].output == YES) g_outsize += Gpars[i].rate; */
    g_outsize += Gpars[i].rate;
  }
/*  Add space for base time and time_offset */
  g_outsize += 2;
/*  Add the extra variables HOUR, MINUTE, SECOND, if needed */
  if (Extra_Times == TRUE) g_outsize += 3;

/* #############  Test print */
    printf ("%s:  Allocated %i words for g_buf\n",progname,g_outsize);
/* Allocate GENPRO converted data buffer */
  g_buf = (NR_TYPE *)malloc(g_outsize * NR_SIZE);
  if (g_buf == NULL)
  {
    fprintf (stderr,"%s:  Could not allocate memory for GENPRO converted"
        " data.\n",argv[0]);
    close (Gp_fp);
    exit (1);
  }
/* #############  Test print */
    printf ("%s:  Starting address for g_buf is %i\n",progname,&g_buf[0]);
    printf ("%s:  Ending address for g_buf is %i\n",progname,&g_buf[g_outsize-1]);

/* Allocate GENPRO PMS-1D converted data buffer */
  if (p_outsize > 0)
  {
/* #############  Test print */
    printf ("%s:  Allocating %i words for p_buf\n",progname,p_outsize);
    p_buf = (NR_TYPE *)malloc(p_outsize * NR_SIZE);
    if (p_buf == NULL)
    {
      fprintf (stderr,"%s:  Could not allocate memory for GENPRO PMS-1D"
          " converted data.\n",argv[0]);
      close (Gp_fp);
      exit (1);
    }
/* #############  Test print */
    printf ("%s:  Starting address for p_buf is %i\n",progname,&p_buf[0]);
    printf ("%s:  Ending address for p_buf is %i\n",progname,&p_buf[p_outsize-1]);
  }

/* Process options here  (This portion would either use command-line
     options, read a specification file or be an interactive input area.
     The idea would be to select a subset of the time period and/or
     variables for output or process other enhancements that are developed.) */
          /* future development */

/* Create the output file (includes writing netCDF header). */
  CreateNetCDF();

/* #############  Test purposes: don't go on for now */
/*    printf ("%s:  Stopping here for test purposes.\n",progname);
    cleanup();
 */

/* Plow through the data. */
   Plow ();

/* Print a summary. */
/*  if (infoflag) */
  printf ("%s:  Wrote %u records to netCDF file %s\n",argv[0],l_d_rec,outfile);

/* Close netCDF file */
  CloseNetCDF();

/* Done! */
  exit (0);
}
