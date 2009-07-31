/* void get_filenames(void)
 *  Query the user for input (GENPRO) and output (netCDF) file names */

/* COPYRIGHT:  University Corporation for Atmospheric Research, 1994, 2004 */

# include <stdlib.h>
# include <stdio.h>
# include <string.h>

# include "g2n.h"

void get_filenames(void)

{

  int c = FALSE;
  size_t len;
  char entry[INSIZE];

/*

             * * * * * * * * * * * * * * *
            *                             *
           *  Executable code starts here  *
            *                             *
             * * * * * * * * * * * * * * *
 */
  while (c==FALSE)
  {
    printf ("%s:  GENPRO input file name ([r] = quit)? ",progname);
    (void)fgets(entry,INSIZE,stdin);
    len = strlen(entry);
/*   If user entered only the [r] key, exit  */
    if (len < 2) exit (0);
/*   Replace the "newline" character with "end-of-line" */
    entry[len-1] = '\0';
    strncpy(infile,entry,len);
    c = GpOpen(infile);
    if (c == FALSE) printf("%s:  Could not open %s\n",progname,infile);
  }
  printf ("%s:  netCDF output file name ([r] = quit)? ",progname);
  (void)fgets(entry,INSIZE,stdin);
/*   If user entered only the [r] key, clean up and exit  */
  len = strlen(entry);

  if (len < 2) cleanup();
/*   Replace the "newline" character with "end-of-line" */
  entry[len-1] = '\0';
  strncpy(outfile,entry,len);

/* #################################################################### */
/*    Check for valid output file path now? */
/* #################################################################### */

/* Ask whether to continuously output time being processed.  */
  printf ("%s:  Continuous output of GENPRO processor time (y, n, quit)? ",progname);
  (void)fgets(entry,3,stdin);
  len = strlen(entry);
/*   If user entered only the [r] key, assume a "no" answer  */
  if (len < 2)  entry[0] = 'n';
  if (entry[0] == 'q') cleanup();
  echo_time = (int)((entry[0] == 'y') || (entry[0] == 'Y'));
  return;
}
