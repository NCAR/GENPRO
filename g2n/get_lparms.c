/* int get_lparms(char *line)
 *  Decode LETVAR parameters on one GENPRO header line which have been
 *   defined by the previous ORDVAR line */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 1997 */

/*   How to do it:
 *   Use the ORDVAR order list to decode each parameter and save in local
 *    storage.  The last part of the line will include the variable's name
 *    to which the parameters apply.  Find the variable's index in the
 *    parameter array and save the parameters in the appropriate places.
 *
 *   Using the Osklist array having nOsk keywords, use a case statement to
 *    determine the format expected for the next parameter.
 *   Use strtok to get the value of each parameter, remembering any unique
 *    tokens needed to decode it.
 *   After the last parameter has been decoded, skip the ",%FOR," string
 *    and decode the variable's name to which the parameters apply.
 *   Search the master variable list for the name and then put all the
 *    decoded parameters in their appropriate place in the Gpars array.
 */

# include <stdio.h>
# include <string.h>
# include <stdlib.h>

# include "g2n.h"

int get_lparms(char *line)

{

  static int f = TRUE;     /* first time flag */
  char *p;                 /* pointer into current line */
  char ltitle[HDR_LEN+1];  /* local space for TITLE */
  char lunits[UNIT_LEN+1]; /* local space for UNITS */
  char *lt, *lu;           /* pointers to ltitle, lunits  */
  int i, idx, j;           /* counters, indices */
  int lsample;             /* local space for SAMPLE */
  int lrate;               /* local space for RATE */
  int lbits;               /* local space for BITS */
  int lfstbit;             /* local space for FSTBIT */
  int lskip;               /* local space for SKIP */
  int lconkey;             /* local space for CONKEY */
  int lsclkey;             /* local space for SCLKEY */
  NR_TYPE lterm;           /* local space for TERM */
  NR_TYPE lfactor;         /* local space for FACTOR */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */

/* Assign pointers */
  lt = &ltitle[0];
  lu = &lunits[0];

/* Skip past the keyword and its delimiter */
  p = strtok(line,"=");
  if (p != NULL)
  {
    for (i=0;i<nOsk;i++)
    {
      switch (Osklist[i])
      {
        case  TITLE:
/*          Skip to TITLE if leading <space> */
          j = 7;
          while (*(p+j) != '\0') j++;
          if (*(p+j+1) != '"') p = strtok(NULL,"\"");
          p=strtok(NULL,"\"");
          if (p != NULL)
          {
            TrimTrailingBlanks(p);
            strcpy(ltitle,p);
          }
          break;
        case  UNITS:
  /*          Skip to UNITS if leading <space> */
          j = 7;
          while (*(p+j) != '\0') j++;
          if (*(p+j+1) != '"') p = strtok(NULL,"\"");
          p=strtok(NULL,"\"");
          if (p != NULL)
          {
            TrimTrailingBlanks(p);
            strcpy(lunits,p);
          }
          break;
        case  SAMPLE:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lsample = atoi(p);
          }
          break;
        case  RATE:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lrate = atoi(p);
          }
          break;
        case  BITS:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lbits = atoi(p);
          }
          break;
        case  FSTBIT:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lfstbit = atoi(p);
          }
          break;
        case  SKIP:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lskip = atoi(p);
          }
          break;
        case  CONKEY:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lconkey = atoi(p);
  /*  If first time through, see if I have UW IEEE format data. */
            if (f == TRUE)
            {
              f = FALSE;
              if ((lconkey == 0) && (G_Format == Format_UW))
                G_Format = Format_UW_IEEE;
            }
          }
          break;
        case  SCLKEY:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lsclkey = atoi(p);
          }
          break;
        case  TERM:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lterm = (NR_TYPE)atof(p);
          }
          break;
        case  FACTOR:
          p=strtok(NULL,", ");
          if (p != NULL)
          {
            lfactor = (NR_TYPE)atof(p);
          }
          break;
      }
    }
  }
/*  Skip past the %FOR keyword */
  if ((p=strtok(NULL,", \n")) != NULL)
  {
    p=strtok(NULL,", \n");
    if (p != NULL)
    {
      if ((idx = name_idx(p)) < 0)
/*  If I didn't find a variable's name, add it to the list. */
      {
        strcpy(Gpars[idx=nvars++].name,p);
        par_preset(idx);
        fprintf (stderr,"%s:get_lparms:  Warning:  Variable name %s was"
            " not in the APPVAR list.\n",progname,Gpars[idx].name);
        fprintf (stderr,"   It was added from a LETVAR directive.\n");
      }
    }
    for (i=0;i<nOsk;i++)
    {
      switch (Osklist[i])
      {
        case  TITLE:
          strcpy(Gpars[idx].title,lt);
          break;
        case  UNITS:
          strcpy(Gpars[idx].units,lu);
          break;
        case  SAMPLE:
          Gpars[idx].sample = lsample;
          break;
        case  RATE:
          Gpars[idx].rate = lrate;
          break;
        case  BITS:
          Gpars[idx].bits = lbits;
          break;
        case  FSTBIT:
          Gpars[idx].fstbit = lfstbit;
          if ((Gpars[idx].bits == PRE_BITS)
           || (Gpars[idx].bits == 0))
          {
/*  Because, in GENPRO-II, data are always saved as unsigned 32-bit
 *   integers, instead of quitting as fatal if BITS is missing, I will,
 *   instead, assume BITS = 32 and warn the user. */
/*            if (infoflag) */
            fprintf (stderr,"%s:get_lparms:  Warning:  FSTBIT read"
                " before BITS.\n   Assuming that BITS = 32.\n",
                progname);
            Gpars[idx].fstpt = (lfstbit-1)/32;
          }
          else
          {
            Gpars[idx].fstpt = (lfstbit-1)/Gpars[idx].bits;
          }
          break;
        case  SKIP:
          Gpars[idx].skip = lskip;
          break;
        case  CONKEY:
          Gpars[idx].conkey = lconkey;
          break;
        case  SCLKEY:
          Gpars[idx].sclkey = lsclkey;
          break;
        case  TERM:
          Gpars[idx].term = lterm;
          break;
        case  FACTOR:
          Gpars[idx].factor = lfactor;
          break;
      }
    }
  }
  else
/*  This LETVAR doesn't have a variable name associated with it! */
  {
     fprintf(stderr,"%s:get_lparms:  Warning:  Found LETVAR record without"
        " a variable name",progname);
     return -1;
  }
  return idx;
}
