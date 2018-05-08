/* void setup_pms1d(void)
 *  Search header variables for PMS-1D data and set things up for creating
 *   new variable(s) which use the RAF conventions */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1997, 1998  */

/*            New version adds PMS-1D "ncpp" compatibilty      Dec 1998 RLR */

/*  vi editor settings for this file (no <tab>)
 * :set ts=80
 * :set ai
 */

# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "g2n.h"

void setup_pms1d(void)

{

  int i, ii, j, jj, k;     /* counters, indices */
  int st_hr_pms1d_probes;  /* start index of blocked probes */
  int probe_found;         /* "Probe found" flag */
  int new;                 /* new rate found, if TRUE */
  char ic[2] = {'A', 'C'};  /* initial characters for PMS-1D variable names */
  char *p;                 /* character pointer for "title" */
  char srch_name[12];      /* std PMS-1D variable name space for searching */
  char *srch_nm;           /* std PMS-1D variable name pointer for searching */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/*  Update array of PMS-1D GENPRO-II probe names and their corresponding
 *   netCDF output names (using file name defined in environment variable) */
/*     (future development)  */

/*    Section 1:  Standard Probes (individual cells) */
/*    ---------------------------------------------- */
  for (ii=0;ii<2;ii++)  /* for both accumulations and concentrations */
  {
    for (i=0;i<N_PMS1D_STD_NAMES;i++)
    {
      probe_found = NO;
      for (j=0;j<pms1d_std[i].cells;j++)
      {
        jj = j+1;
        if (probe_found == YES)
        {
/*  Make it easy to find missing cells */
          pms1d[n_pms1d_probes].cell_map[j] = -1;
        }
        for (k=0;k<nvars;k++)
        {
          sprintf (srch_name, "%s%02d",pms1d_std[i].name[ii],jj);
          if ( ( strncmp(Gpars[k].name, srch_name, strlen(srch_name)) == 0 )
            && ( strlen(srch_name) == strlen(Gpars[k].name) ) )
          {
/*  ###########  Diagnostic print */
            printf ("setup_pms1d:  Probe name = %s, variable = %s, index = %i, fstpt = %i\n",srch_name,Gpars[k].name,k,Gpars[k].fstpt);
            if (probe_found == NO)
/*   Save initial parameters for this probe */
            {
              probe_found = YES;
              Pgpars[n_pms1d_probes] = Gpars[k];
              pms1d[n_pms1d_probes].num_defined_cells = pms1d_std[i].cells;
              pms1d[n_pms1d_probes].code = i;
              pms1d[n_pms1d_probes].type = 1;
              pms1d[n_pms1d_probes].vectorID = pms1d_std[i].vectorID;
              printf ("setup_pms1d:  Prefix for renamed variable = %c\n",ic[ii]);
              printf ("setup_pms1d:  Reference name for netCDF file = %s\n",pms1d_std[i].ref_name);
              printf ("setup_pms1d:  Suffix for renamed variable = _%02d\n",i+1);
              sprintf (Pgpars[n_pms1d_probes].name,"%c%s_%02d",ic[ii],pms1d_std[i].ref_name,i+1);
/*  ###########  Diagnostic print */
              printf ("setup_pms1d:  Probe name for netCDF file = %s\n",Pgpars[n_pms1d_probes].name);
              printf ("setup_pms1d:  Probe long name = %s\n",Pgpars[n_pms1d_probes].title);
              p = strstr (Pgpars[n_pms1d_probes].title,"CELL");
              if (p == 0)
              {
                sprintf (srch_name, "%02d",jj);
/*  ###########  Diagnostic print */
                printf ("setup_pms1d:  p=0; srch_name for strstr = %s\n",srch_name);
                p = strstr (Pgpars[n_pms1d_probes].title,srch_name);
              }
              if (p != 0) memcpy (p," (per cell)",12);
/*  ###########  Diagnostic print */
              printf ("setup_pms1d:  New probe long name = %s\n",Pgpars[n_pms1d_probes].title);
              Pgpars[n_pms1d_probes].output = YES;
            }
            Gpars[k].output = NO;
            pms1d[n_pms1d_probes].cell_map[j] = Gpars[k].fstpt;
            pms1d[n_pms1d_probes].num_found_cells = jj;
            break;
          }
        }
      }
/* Finished searching for all the individual cells of a particular probe */

/* A probe that was found gets set up here for processing */
      if (probe_found == YES)
      {
/*  ###########  Diagnostic print (begin) */
        printf ("setup_pms1d:  maximum cell number found = %i\n", pms1d[n_pms1d_probes].num_found_cells);
        printf ("setup_pms1d:  defined number of cells = %i\n", pms1d[n_pms1d_probes].num_defined_cells);
        for (k=0;k<pms1d[n_pms1d_probes].num_defined_cells;k++)
        {
          printf ("setup_pms1d:  cell map (%i) = %i\n", k, pms1d[n_pms1d_probes].cell_map[k]);
        }
/*  ###########  Diagnostic print (end) */

/* If there are 16, 32 or 64 cells, indexing maps perfectly (1-15, 1-31, 1-63);
     If fewer, one must offset the indices by one */
        pms1d[n_pms1d_probes].cell_offset = (pms1d[n_pms1d_probes].num_found_cells == pms1d[n_pms1d_probes].num_defined_cells) ? NO : YES;

/*  Need to define PMS-1D output buffer space so data can be found */
        Pgpars[n_pms1d_probes].fstpt = (int)p_outsize;
        p_outsize += (pms1d[n_pms1d_probes].num_defined_cells * Pgpars[n_pms1d_probes].rate);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe output index = %i\n",Pgpars[n_pms1d_probes].fstpt);
        printf ("setup_pms1d:  Current PMS-1D output buffer size = %i\n",p_outsize);

/*  Get ready for next probe */
        n_pms1d_probes++;
        if (n_pms1d_probes >= PMS1D_PROBE_MAX)
        {
          fprintf (stderr,"%s: setup_pms1d:  Reached the max # probes (%i) I can handle.\n                       If more exist, they will be ignored.",progname,n_pms1d_probes);
          return;
        }
      }
    }
  }

/*  Assign suffix codes to all PMS-1D probes' Associated Variables */
/*  -------------------------------------------------------------- */
  for (i=0;i<nvars;i++)
  {
    for (j=0;j<n_pms1d_probes;j++)
    {
      k = pms1d[j].code;
      srch_nm = pms1d_std[k].accums;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].accums,k+1);
      }
      srch_nm = pms1d_std[k].concs;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].concs,k+1);
      }
      srch_nm = pms1d_std[k].mean_diam;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].mean_diam,k+1);
      }
      srch_nm = pms1d_std[k].dispersion;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].dispersion,k+1);
      }
      srch_nm = pms1d_std[k].lwc;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].lwc,k+1);
      }
      srch_nm = pms1d_std[k].activity;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].activity,k+1);
      }
      srch_nm = pms1d_std[k].dbz;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].dbz,k+1);
      }
      srch_nm = pms1d_std[k].range;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].range_ref,k+1);
/*  For GENPRO files, FSSP range must be adjusted from 1-4 to 0-3
 *    to conform to nimbus conventions */
         Gpars[i].term += 1.;
      }
      srch_nm = pms1d_std[k].beam_fract;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].beam_fract,k+1);
      }
      srch_nm = pms1d_std[k].fast_reset;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].fast_reset,k+1);
      }
      srch_nm = pms1d_std[k].strobes;
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_std[k].strobes,k+1);
      }
    }
  }

/*    Section 2:  Blocked (hr) Probes */
/*    ------------------------------- */
/*  Save starting index for blocked probes  */
  st_hr_pms1d_probes = n_pms1d_probes;
  for (ii=0;ii<2;ii++)  /*  for both accumulations and concentrations  */
  {
    for (i=0;i<N_PMS1D_HR_NAMES;i++)
    {
      probe_found = NO;
      srch_nm = pms1d_hr[i].name[ii];
      for (k=0;k<nvars;k++)
      {
        if ( ( strncmp(Gpars[k].name, srch_nm, strlen(srch_nm)) == 0 )
          && ( strlen(srch_nm) == strlen(Gpars[k].name) )
          && ( Gpars[k].rate > 10 ) )
        {
          probe_found = YES;
          break;
        }
      }
      if (probe_found == YES)
      {
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  PMS probe name = %s, variable = %s\n",pms1d_hr[i].name,Gpars[k].name);
/*   Save initial parameters for this probe */
        Pgpars[n_pms1d_probes] = Gpars[k];
        pms1d[n_pms1d_probes].num_defined_cells = pms1d_hr[i].cells;
        pms1d[n_pms1d_probes].type = 2;
        pms1d[n_pms1d_probes].code = i;
        pms1d[n_pms1d_probes].vectorID = pms1d_hr[i].vectorID;
        sprintf (Pgpars[n_pms1d_probes].name,"%c%s_%02d",ic[ii],pms1d_hr[i].ref_name,i+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe name for netCDF file = %s\n",Pgpars[n_pms1d_probes].name);
        printf ("setup_pms1d:  Probe long name = %s\n",Pgpars[n_pms1d_probes].title);
/*  Ignore the variable's long_name change, since I doubt it is necessary
        p = strstr (Pgpars[n_pms1d_probes].title,"CELL");
        memcpy (p," (per cell)",12);
   *  ###########  Diagnostic print *
        printf ("setup_pms1d:  New probe long name = %s\n",Pgpars[n_pms1d_probes].title);
   */
        Pgpars[n_pms1d_probes].output = YES;
        Gpars[k].output = NO;
        pms1d[n_pms1d_probes].cell_map[0] = Gpars[k].fstpt;
/* Probe's sample rate is based upon its blocked size and # of defined cells */
        Pgpars[n_pms1d_probes].rate = (Gpars[k].rate+pms1d[n_pms1d_probes].num_defined_cells-1)/pms1d_hr[i].cells;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe's rate = %i\n",Pgpars[n_pms1d_probes].rate);
/* Once the probe's rate has been determined, I must search the extant rate
   *  list for a match.  If none is found, this rate must be added to the list
   *  and nRates incremented.
   */
        new = TRUE;
        jj = Pgpars[n_pms1d_probes].rate;
        for (j=0;j<nRates;j++)
        {
          if (Rates[j] == jj)
          {
/*  ###########  Diagnostic print */
            printf ("setup_pms1d:  found new probe's rate(%i) = %i\n",j,jj);
            new = FALSE;
            break;
          }
        }
        if (new == TRUE)
        {
          if (nRates < NRATES)
          {
            Rates[nRates] = jj;
            nRates++;
/*  ###########  Diagnostic print */
            printf ("setup_pms1d:  creating new probe's rate(%i) = %i\n",nRates,jj);
          }
          else
          {
            fprintf (stderr,"%s:read_hdr:  Exceeded maximum number"
             " of unique output rates.",progname);
            fprintf (stderr,"  See Ron Ruth (ruth@ucar.edu).\n");
            cleanup();
          }
        }
        pms1d[n_pms1d_probes].num_found_cells = Gpars[k].rate/Pgpars[n_pms1d_probes].rate;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  maximum cell number found = %i\n", pms1d[n_pms1d_probes].num_found_cells);
        printf ("setup_pms1d:  defined number of cells = %i\n", pms1d[n_pms1d_probes].num_defined_cells);
/*      printf ("setup_pms1d:  i = %i, k = %i\n",i,k); */

/* If there are 16, 32 or 64 cells, indexing maps perfectly (1-15, 1-31, 1-63);
     If fewer, one must offset the indices by one */
        pms1d[n_pms1d_probes].cell_offset = (pms1d[n_pms1d_probes].num_found_cells == pms1d[n_pms1d_probes].num_defined_cells) ? NO : YES;

/*  Need to define PMS-1D output buffer space so data can be found */
        Pgpars[n_pms1d_probes].fstpt = (int)p_outsize;
        p_outsize += pms1d[n_pms1d_probes].num_defined_cells * Pgpars[n_pms1d_probes].rate;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe output index = %i\n",Pgpars[n_pms1d_probes].fstpt);
        printf ("setup_pms1d:  Current PMS-1D output buffer size = %i\n",p_outsize);

/*  Get ready for next probe */
        n_pms1d_probes++;
        if (n_pms1d_probes >= PMS1D_PROBE_MAX)
        {
          fprintf (stderr,"%s: setup_pms1d:  Reached the max # probes (%i) I can handle.\n                       If more exist, they will be ignored.",progname,n_pms1d_probes);
          return;
        }
      }
    }
  }

/*  Assign suffix codes to all PMS-1D probes' Associated Variables */
/*  -------------------------------------------------------------- */
  for (i=0;i<nvars;i++)
  {
/*  ###########  Diagnostic print */
    printf ("setup_pms1d:  Blocked Probe start/end indices = %i,%i\n",st_hr_pms1d_probes,n_pms1d_probes);
    printf ("setup_pms1d:  GENPRO variable name to check = %s\n",Gpars[i].name);
    for (j=st_hr_pms1d_probes;j<n_pms1d_probes;j++)
    {
      k = pms1d[j].code;
      srch_nm = pms1d_hr[k].accums;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe ref name = %s\n",pms1d_hr[k].name[0]);
        printf ("setup_pms1d:  Probe index  = %i\n",k);
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
        printf ("setup_pms1d:   for variable = %s\n",Pgpars[j].name);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].accums,k+N_PMS1D_STD_NAMES+1);
      }
      srch_nm = pms1d_hr[k].concs;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].concs,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].mean_diam;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].mean_diam,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].dispersion;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].dispersion,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].lwc;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].lwc,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].activity;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].activity,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].dbz;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].dbz,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].range;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].range_ref,k+N_PMS1D_STD_NAMES+1);
/*  For GENPRO files, FSSP range must be adjusted from 1-4 to 0-3
 *    to conform to nimbus conventions */
         Gpars[i].term += 1.;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].beam_fract;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].beam_fract,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].fast_reset;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].fast_reset,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
      srch_nm = pms1d_hr[k].strobes;
/*  ###########  Diagnostic print */
        printf ("setup_pms1d:  Probe associated variable = %s\n",srch_nm);
      if ( ( strncmp(Gpars[i].name, srch_nm, strlen(srch_nm)) == 0 )
        && ( strlen(srch_nm) == strlen(Gpars[i].name) ) )
      {
        sprintf (Gpars[i].name,"%s_%02d",pms1d_hr[k].strobes,k+N_PMS1D_STD_NAMES+1);
/*  ###########  Diagnostic print */
        printf ("setup_pms1d: appended suffix to variable %s #################\n",Gpars[i].name);
      }
    }
  }
}
