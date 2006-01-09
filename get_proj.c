/* void get_proj(char *line)
 *  Decode the PROJECT line */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 2004 */

/*  How to do it:
 *  This may be an ongoing problem, since no real standard existed for how
 *   to format this line.  In the most-recent NCAR GENPRO projects, the
 *   format was fixed, so it will be easier.  A lot depends upon what
 *   information one wants to get from this line.  For University of Wyoming
 *   headers, only a brief project description was saved here (e.g. WP4A is
 *   the entire project line for a WISP flight on 8 February 1994).  */

/*  For NCAR GENPRO files:
 *  netCDF wants a 3-digit project number.  I can get that information
 *   for all the GENPRO-II examples I have.  The line either begins
 *   with the project number or has a preceding P or minus sign.
 *   I suppose it would be good to skip white space, even though none
 *   of my examples has any before the information starts. */
/*  In every example I have, the flight number immediately follows the
 *   project number separated from it with a minus sign.  If only digits
 *   are desired, then one will have to skip the characters until a digit
 *   appears.  (The characters can be R, RF, T, TF, F, FF, etc.)  Save
 *   the flight type.  (Default flight type to "R" if none appears.)
 *   The flight number may be followed by a segment letter.  It may be
 *   important to have the segment information available, but I don't
 *   know whether that can be put into the netCDF header.  For now,
 *   decode it and make it available. */
/*  Following the flight number/segment is the project description.  It
 *   would seem reasonable to just grab the rest of the string of text
 *   (until the second double quote) even though the last part of the
 *   description, in all the examples I have, includes the flight's date. */

/*  For University of Wyoming GENPRO files:
 *  The tack to take for now is to read the first 40 characters of the
 *   line's text and use them as a project description.  Use the default
 *   project number, flight number and segment; assume project type is
 *   research. */

# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_proj(char *line)

{

  char *p;           /* pointer into current line */
  int j;             /* pointer offset */

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
/* Skip past the keyword and its delimiter */
  p = strtok(line,"=");
  if (p != NULL)
  {
/* Skip to project delimiter, if necessary */
    j = 8;
    while (*(p+j) != '\0') j++;
    if (*(p+j+1) != '"') p = strtok(NULL,"\"");
/* Get project information string */
    p = strtok(NULL,"\"");
    if (p != NULL)
    {
/* By now I should know the data format */
      if (G_Format == Format_NCAR)
/*  NCAR format */
      {
/*   Decode the project number, flight type, flight number and segment */
/*    First format:   Pppp-tFnn projname ddmmmyyyy    (ppp = project number)
 *     and variants   Pppp-tFnns projname ddmmmyyyy   (t = project type)
 *                    Pppp-tnn projname ddmmmyyyy     (nn = flight number)
 *                    Pppp-tnns projname ddmmmyyyy    (s = flight segment)
 *                    Pppp-nn projname ddmmmyyyy      (projname = project name)
 *                    Pppp-nns projname ddmmmyyyy     (dd = day of month)
 *    Second format:  ppp-tFnn projname ddmmmyyyy     (mmm = month)
 *     and variants   ppp-tFnns projname ddmmmyyyy    (yyyy = year)
 *                    ppp-tnn projname ddmmmyyyy
 *                    ppp-tnns projname ddmmmyyyy
 *                    ppp-nn projname ddmmmyyyy
 *                    ppp-nns projname ddmmmyyyy
 *    Third format:   f-ppp-tFnn projname ddmmmyyyy   (f = fiscal year)
 *     and variants   f-ppp-tFnns projname ddmmmyyyy
 */
/*   Project number */
/*    If the second character is a -, skip the fiscal year symbol */
        if (*(p+1) == '-')
        {
/*    For some reason strtok fails in its normal operation (bug?)
 *     so I had to fudge it to work.  It may fail on other machines.
 *     Instead, it may be that I don't understand how strtok works. */
          projno = atoi(p+2);
/*    For some reason this didn't work:  p=strtok(NULL,"- "); */
          if ((p=strtok(p,"- ")) == NULL) return;
          if ((p=strtok(NULL,"-")) == NULL) return;
          if ((p=strtok(NULL,"- ")) == NULL) return;
        }
        else
        {
/*    If the first character is a "P", skip it first. */
          if (*(p) == 'P')
          {
            projno = atoi (p+1);
          }
          else
          {
            projno = atoi(p);
          }
/*   Flight number (including type and segment) */
/*    For some reason this doesn't work:  p=strtok(NULL,"- "); */
          if ((p=strtok(p,"- ")) == NULL) return;
          if ((p=strtok(NULL,"- ")) == NULL) return;
        }
/*   Aircraft tail number */
/*        if      (projno > 899) ;  *  Aircraft unknown *
        else */ if (projno > 799) strcpy(aircraft,"N308D");
        else if (projno > 699) strcpy(aircraft,"N307D");
        else if (projno > 599) strcpy(aircraft,"N306D");
        else if (projno > 499) strcpy(aircraft,"N595KR");
        else if (projno > 399) strcpy(aircraft,"N304D");
        else if (projno > 299) strcpy(aircraft,"N303D");
        else if (projno > 199) strcpy(aircraft,"N312D");
        else if (projno >  99) strcpy(aircraft,"N130AR");
/*   Flight number */
        if (isdigit(*p) == NO) ftype = *p++;
        if (isdigit(*p) == NO) *p++;
        fltno = atoi(p);
/*    Skip up to a 3-digit flight number */
        if (isdigit(*p) != NO) p++;
        if (isdigit(*p) != NO) p++;
        if (isdigit(*p) != NO) p++;
        if (*p != ' ') segment = *p;
/*   Project's name and possible date */
        if ((p=strtok(NULL,"\"")) == NULL) return;
        TrimTrailingBlanks(p);
        strcpy(project,p);
/*  ###########  Diagnostic print */
        printf ("get_proj:  Format is %s\n","Format_NCAR");
        printf ("get_proj:  Aircraft = %s\n",aircraft);
      }
      else if ((G_Format == Format_UW) || (G_Format == Format_UW_IEEE))
/*  University of Wyoming format */
      {
/*  For now, use the first 43 characters of the project text as a project
 *   description and use default values for project number, flight type,
 *   flight number and segment. */
    *(p+43) = '\0';
        TrimTrailingBlanks(p);
        strcpy(project,p);
        strcpy(aircraft,"N2UW");
        projno = 1;
        fltno = 1;
/*  ###########  Diagnostic print */
        if (G_Format == Format_UW) printf ("get_proj:  Format is %s\n","Format_UW");
        if (G_Format == Format_UW_IEEE) printf ("get_proj:  Format is %s\n","Format_UW_IEEE");
        printf ("get_proj:  Aircraft = %s\n",aircraft);
      }
      else if (G_Format == Format_N48RF)
/*  NOAA Twin Otter format */
      {
/*  For now, use the first 43 characters of the project text as a project
 *   description and use default values for project number, flight type,
 *   flight number and segment. */
    *(p+43) = '\0';
        TrimTrailingBlanks(p);
        strcpy(project,p);
        strcpy(aircraft,"N48RF");
        projno = 1;
        fltno = 1;
/*  ###########  Diagnostic print */
        printf ("get_proj:  Format is %s\n","Format_N48RF");
        printf ("get_proj:  Aircraft = %s\n",aircraft);
      }
/*  else if ((G_Format == Format_HK))
 *  Add other formats here */
    }
  }
  return;
}
