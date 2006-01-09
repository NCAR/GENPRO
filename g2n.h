 /* Definitions for g2n.c:  GENPRO to netCDF conversion software */

/*  Ronald L. Ruth */

/* COPYRIGHT:  University Corporation for Atmospheric Research,
 *              1994, 1997, 1998, 2004 */

/*   Version 2.3 Add support for SCLKEY parameter             Mar 1997  RLR */
/*   Version 2.3.1 Check for legal time vars in UW datasets   Aug 1997  RLR */
/*   Version 2.4 Add PMS-1D conversion sections    Nov 1997 - Jan 1998  RLR */
/*   Version 2.5 Add COS-block file read sections         Mar-Sep 1998  RLR */
/*   Version 2.6 Add PMS-1D "ncpp" compatibilty               Dec 1998  RLR */

/*  ###################  <-- Search for this string to locate ??? areas */


/*  Logical definitions  */
# define TRUE      1
# define YES       1
# define FALSE     0
# define NO        0

/*  User input buffer size (for keyboard entry of file names)  */
# define INSIZE  129
 

/* GENPRO Header Record Info */
# define HDR_LEN        80  /* byte length of GENPRO logical header record */
# define UNIT_LEN       12  /* byte length of GENPRO UNITS label */
# define NVAR          500  /* maximum number of GENPRO variables */
# define Format_NCAR     0  /* header format is NCAR's */
# define Format_UW       1  /* header format is University of Wyoming's */
# define Format_UW_IEEE  2  /* header format is U. of Wyoming's IEEE */
# define Format_N48RF    3  /* header format is NOAA Twin Otter N48RF */
# define Format_HK       4  /* header format is Hong Kong project (future) */
# define NRATES         25  /* maximum number of unique GENPRO data rates */

/*  Kwfound = bit flag for keywords found in the GENPRO header
 *   Note:  Add new keywords here and at other places in this header
 *           marked with the string #kw#.
 *
 *       Keyword   Value    Bit   Critical?          */
# define BEGINHD      1 /*   0   yes (for GENPRO-II) */
# define PROJECT      2 /*   1   no; use preset      */
# define PRDATE       4 /*   2   no; use preset      */
# define COMMENT      8 /*   3   no; use preset      */
# define EXDATE      16 /*   4      no               */
# define EXTIME      32 /*   5      no               */
# define JOBID       64 /*   6      no               */
# define LOGBIT     128 /*   7     yes               */
# define DATLOG     256 /*   8     yes               */
# define DATSIZ     512 /*   9     yes               */
# define APPVAR    1024 /*  10     yes               */
# define ORDVAR    2048 /*  11     yes               */
# define LETVAR    4096 /*  12     yes               */
# define ENDHD     8192 /*  13     yes               */

/* index into master keyword list */
# define TITLE        0
# define UNITS        1
# define SAMPLE       2
# define RATE         3
# define BITS         4
# define FSTBIT       5
# define SKIP         6
# define CONKEY       7
# define SCLKEY       8
# define TERM         9
# define FACTOR      10

/* Maximum number of active ORDVAR sub-keywords */
# define MAXOsk       8

/* preset values for variable parameters */
# define PRE_LOGBIT  -1   /* preset to bogus value */
# define PRE_DATLOG  -1   /* preset to bogus value */
# define PRE_DATSIZ  -1   /* preset to bogus value */

# define PRE_TITLE   "No title"
# define PRE_UNITS   "Unk"
# define PRE_SAMPLE  -1    /* preset to bogus value */
# define PRE_RATE    -1    /* preset to bogus value */
# define PRE_BITS    -1    /* preset to bogus value */
# define PRE_FSTBIT  -1    /* preset to bogus value */
# define PRE_FSTPT   -1    /* preset to bogus value */
# define PRE_SKIP     0    /* preset to default value */
# define PRE_CONKEY   1    /* preset to default value */
# define PRE_SCLKEY   2    /* preset to default value */
# define PRE_TERM     0.0  /* preset to default value */
# define PRE_FACTOR   0.0  /* preset to default value */

# define KEYLEN       8  /* maximum number of characters in GENPRO keywords */

/* COS-blocked data info */
# define CRAY_INVALID  -2  /* WORD_LENGTH returned if invalid file structure */
# define CRAY_EOF      -4  /* WORD_LENGTH returned if EOF encountered */
# define CRAY_EOD      -5  /* WORD_LENGTH returned if EOD encountered */
# define CRAY_DISK_ERR -6  /* WORD_LENGTH returned if disk read error */
# define COS_BLOCKED    2  /* DiskData flag value:  COS-blocked GENPRO file */
# define STRIPPED       3  /* DiskData flag value:  stripped GENPRO file */

/* Nimbus Record Info */
# define NR_TYPE float
# define NR_SIZE sizeof(NR_TYPE)
# define DIMMAX         3  /* max number of netCDF dimensions (used below) */

# define NSCRATCH    2000  /* size of scratch buffer space */

/*  Define parameters for PMS-1D conversion */
# define N_P_CELLS                 3  /* max # of unique PMS-1D cell arrays */
# define N_PMS1D_STD_NAMES        13  /* number of std PMS-1D probe types */
# define N_PMS1D_HR_NAMES         16  /* number of blocked PMS-1D probe types */
# define PMS1D_PROBE_MAX          30  /* max number of PMS-1D probes */
# define PMS1D_PROBE_CHANNEL_MAX  64  /* max number of PMS-1D probe channels */

/*   netCDF definitions (drawn from extant code) */
#define MISSING_VALUE  (-32767.0)  /* /home/local/nimbus/include/decode.h */

/*   Define parameters for a GENPRO variable */
struct _Gpars
{
  char name[12];           /* (up to) 8-character name */
  char title[HDR_LEN+1];   /* (up to) 80-character description */
  char units[UNIT_LEN+1];  /* (up to) 12-character units */
  int sample;           /* number of measurements in one cycle (1 second) */
  int rate;             /* number of measurements to output in one cycle */
  int bits;             /* number of bits in each sample datum */
  int fstbit;           /* start bit position in logical data record */
  int fstpt;            /* start offset in GENPRO logical data record */
  int skip;             /* # bits between successive data values (usually 0) */
  int conkey;           /* conversion key */
  int sclkey;           /* scale key (1=unscaled; 2=scaled; usually 2) */
  float term;           /* offset, if scaled */
  float factor;         /* divisor, if scaled */
  int ofstpt;           /* start offset in netCDF output data record */
  int output;           /* output flag (1=yes, 0=no) */
};

/*   Define parameters for a PMS-1D variable */
struct _PMS1D
{
  int num_defined_cells;   /* number of defined channels (cells) */
  int num_found_cells;     /* number of channels (cells) found */
  int cell_offset;         /* offset probe's output data by 1 cell (YES, NO) */
  int type;                /* probe type (1 = standard, 2 = blocked) */
  int code;                /* probe code (index into defined probe list) */
  int vectorID;            /* vector index pointing to number of cells */
  int cell_map[PMS1D_PROBE_CHANNEL_MAX];  /* PMS-1D cell index map (mainly
 *        used by type 1 probes, but type 2 probes use the first one) */
};

/*   Define PMS-1D variable types for searching */
/*   (changed by RLR in Dec 1998 to combine accumulations, concentrations
 *      and associated variables) */
struct _PMS1D_type
{
  char *name[2];    /* GENPRO-II PMS-1D variable-name prefixes (acc + conc) */
  int  cells;       /* number of cells for referenced variable */
  int  vectorID;    /* vector index (into DimVectorID array) with # of cells */
  char *ref_name;   /* netCDF PMS-1D reference name (without prefix letter) */
  char *accums;     /* GENPRO-II associated name:  accumulation (all cells) */
  char *concs;      /* GENPRO-II associated name:  concentration (all cells) */
  char *mean_diam;  /* GENPRO-II associated name:  mean diameter */
  char *dispersion; /* GENPRO-II associated name:  dispersion */
  char *lwc;        /* GENPRO-II associated name:  liquid water content */
  char *activity;   /* GENPRO-II associated name:  activity */
  char *dbz;        /* GENPRO-II associated name:  DBZ */
  char *range;      /* GENPRO-II associated name:  FSSP range */
  char *range_ref;  /* netCDF reference name for:  FSSP range */
  char *beam_fract; /* GENPRO-II associated name:  FSSP beam fraction */
  char *fast_reset; /* GENPRO-II associated name:  FSSP fast resets */
  char *strobes;    /* GENPRO-II associated name:  FSSP total strobes */
};

# ifdef G2N

  
/*  G L O B A L   D E F I N I T I O N S  */
/*  -----------------------------------  */

/*  Debug print flag */
int d_bug_print = NO;

/*  Program's name  */
/*  --------------  */
char progname[128];    /* name of executing program (g2n) */


/*  Flags for command-line options  */
/*  ------------------------------  */
/*   This could include:
 *    batch mode (stdout and stderr to file) */
  int batch_flag = NO;
/*    print informational messages (y/n)
  int infoflag = YES;
 *    print warning
  int warnflag = YES;
 *    print fatal messages (y/n)
  int fatalflag = YES;
 *    print complete (partial) GENPRO header to stdout (y/n)
  int headerflag = YES;
 *    print input variable list to stdout (y/n)
  int Gvarflag = YES;
 *    print output variable list to stdout (y/n)
  int CDFvarflag = YES;
 *    read output variable list from file (y/n)
  int Ovarlistflag = YES;
 *    echo time being processed to stdout */
  int echo_time = NO;
/*  (When activating, don't forget to add these in the "extern" part, too.)
 */


/*  GENPRO-II (Input) File  */
/*  ----------------------  */

FILE *Gp_fp;          /* GENPRO file pointer */
char infile[INSIZE];  /* GENPRO file name */


/*  GENPRO-II Header  */
/*  ----------------  */

/*   leading keywords #kw# */
char *Keyword[] =  {"BEGINHD", "PROJECT", "PRDATE", "COMMENT", "EXDATE",
                    "EXTIME",  "JOBID",   "LOGBIT", "DATLOG",  "DATSIZ",
                    "APPVAR",  "ORDVAR",  "LETVAR", "ENDHD",     NULL };

unsigned int Kwfound = 0;          /* keywords found in the GENPRO header */
unsigned int Kwcritical = 037600;  /* keywords needed for processing #kw# */

/*   sub-keywords for the ORDVAR keyword */
char *Oskeyword[] = {"TITLE", "UNITS",  "SAMPLE", "RATE", "BITS",   "FSTBIT",
                     "SKIP",  "CONKEY", "SCLKEY", "TERM", "FACTOR",   NULL };

int G_Format = Format_NCAR;  /* header format in use (NCAR or Wyoming) */

int nhrec = 0;        /* number of header records read */
int nOsk = 0;         /* number of active ORDVAR sub-keywords */ 
int Osklist[MAXOsk];  /* list of active ORDVAR sub-keyword indices */

int nvars = 0;  /* total number of GENPRO variables (from header) */
int Nvars = 0;  /* number of GENPRO variables to be output */

char aircraft[8] = {"Unknown"};  /* aircraft tail number */
int projno = 1;                  /* project number */
int fltno = 1;                   /* flight number */
char ftype = 'R';                /* flight type (Research, Ferry, Test) */
char segment = ' ';              /* segment letter (if any) */
char project[44] = {"Unknown"};  /* project name and, possibly, flight date */
int prdate[3] = {1,1,70};        /* project flight date (d,m,y) */
/*  Note:  Flight times will come from the actual data records. */
int exdate[3] = {1,1,70};        /* process date (d,m,y) */
int extime[3] = {0,0,0};         /* process time (h,m,s) */
char jobid[8] = {"Unknown"};     /* process serial number */
int logbit = -1;                 /* number of bits per logical data record */
int datlog = -1;                 /* # of logical records per physical record */
int datsiz = -1;                 /* # of bits per physical data record */


/*   Define parameters for each GENPRO variable */
struct _Gpars Gpars[NVAR];

int Rates[NRATES];  /* array of unique GENPRO variable sample rates */
int nRates = 0;     /* number of unique Rates */


/*   Definitions for PMS-1D conversion  */
/*  ----------------------------------  */

/* standard PMS-1D variable definitions (add more, as necessary) */
struct _PMS1D_type pms1d_std[N_PMS1D_STD_NAMES] =
  {
/*  PMS-1D std name prefixes          <-----------------------------------------  Associated Variables  ------------------------------------------> */
/*                                          (Due to GENPRO naming variations, this list is not guaranteed to be accurate for every project.)        */
/*  aname     cname  cells vID  rname  AccumA    ConcA    MnDiam   Disp     LWC      Act     DBZ     Range     Rng_Ref  BmFract  FReset    Strobes  */
   "AFSP"   ,"CFSP"   ,16  ,0 ,"FSSP","SUM15F" ,"CONCF" ,"DBARF" ,"DISPF" ,"PLWCF" ,"FACT" ,"-"    ,"FRANGE" ,"FRNG"  ,"FBMFR" ,"FRESET" ,"FSTROB" ,
   "AFSP2"  ,"CFSP2"  ,16  ,0 ,"FSSP","SUM15F2","CONCF2","DBARF2","DISPF2","PLWCF2","FACT2","-"    ,"FRANGE2","FRNG"  ,"FBMFR2","FRESET2","FSTROB2",
   "ACCUMF" ,"CONCF"  ,16  ,0 ,"FSSP","SUM15F" ,"CONCF" ,"DBARF" ,"DISPF" ,"FLWC"  ,"TFACT","-"    ,"FRANGE" ,"FRNG"  ,"FBMFR" ,"TFRESET","TFSTROB",
   "ACCUMFS","CONCFS" ,16  ,0 ,"FSSP","SUM15F" ,"CONCF" ,"DBARF" ,"FDISP" ,"FLWC"  ,"TFACT","-"    ,"FRANGE" ,"FRNG"  ,"FBMFR" ,"TFRESET","TFSTROB",
   "AFS3"   ,"CFS3"   ,32  ,1 ,"F300","SUM3"   ,"CONC3" ,"DBAR3" ,"DISP3" ,"PLWC3" ,"-"    ,"-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "A20X"   ,"C20X"   ,16  ,0 ,"200X","SUM15X" ,"CONCX" ,"DBARX" ,"DISPX" ,"PLWCX" ,"-"    ,"DBZX" ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "ACCUMX" ,"CONCX"  ,16  ,0 ,"200X","SUM15X" ,"XCONC" ,"XDBAR" ,"XDISP" ,"XLWC"  ,"-"    ,"XDBZ" ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "A20Y"   ,"C20Y"   ,16  ,0 ,"200Y","SUM15Y" ,"CONCY" ,"DBARY" ,"DISPY" ,"PLWCY" ,"-"    ,"DBZY" ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "A26X"   ,"C26X"   ,64  ,2 ,"260X","XSUMX6" ,"XCONC6","XDBAR6","XDISP6","XPLWC6","-"    ,"XDBZ6","-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "A30X"   ,"C30X"   ,32  ,1 ,"300X","SUM3"   ,"CONC3" ,"DBAR3" ,"DISP3" ,"-"     ,"-"    ,"-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "AASS"   ,"CASS"   ,16  ,0 ,"ASAS","SUM15A" ,"CONCA" ,"DBARA" ,"DISPA" ,"PLWCA" ,"AACT" ,"-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "ACCUMA" ,"CONCA"  ,16  ,0 ,"ASAS","SUM15A" ,"ACONC" ,"ADBAR" ,"ADISP" ,"PLWCA" ,"CAACT","-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "APCS"   ,"CPCS"   ,16  ,0 ,"PCAS","SUM15A" ,"CONCA" ,"DBARA" ,"DISPA" ,"PLWCA" ,"PACT" ,"-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"
  };

/*      New entries not yet activated (from project 1984-263 Lake Effect Snow Storms, Braham)
   "FS"     ,"CFSC"   ,16  ,1 ,"FSSP","-"      ,"FCONC" ,"FDBAR" ,"FDISP" ,"FLWC"  ,"FACT" ,"-"    ,"FRANGE" ,"-"     ,"FBMFR" ,"FRESET" ,"FSTROB  ,
   "YS"     ,"CYSC"   ,16  ,1 ,"200Y","-"      ,"YCONC" ,"YDBAR" ,"YDISP" ,"YLWC"  ,"-"    ,"YDBZ" ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
 */

/* ------------------------- Future development ------------------------- */

/*  Wyoming has some, too, but not paired and without associated variables:
 * "nfssp", "fssp",  16, 0,  * "FSSP velocity accepted cnts - channel 1 " to 15
 *                           * "FSSP concentration (old) - channel 1    " to 15
 * "nfssp", "fsspc", 16, 0,  * "FSSP velocity accepted cnts - channel 1 " to 15
 *                           * "FSSP coincidence corrected conc-chan 1  " to 15
 * "  ?  ", "oned",  16, 0,  * "1D-C concentration - 12.5 um            " to 15
 * "  ?  ", "nszic", 20, 0,  * "2D-C ice <= 50 um                       " to 20
 * "  ?  ", "nszip", 20, 0   * "2D-P ice <= 200 um                      " to 20
 *   I can handle the last two if I define a new vector variable for 20
 *    cells.  For the last three probe definitions, each cell has a size
 *    range associated with it in the long_name field, and it would be nice
 *    to create a netCDF attribute that includes them.  (Future development)
 *   Above names came from MSS bitfile:  /RAPDMG/WISP94/N2UW/INTEGER/CB/08FEB94
 */

/* blocked PMS-1D variable definitions (add more, as necessary) */
struct _PMS1D_type pms1d_hr[N_PMS1D_HR_NAMES] =
  {
/*  PMS-1D blocked name prefixes             */
/*  No g2n code yet designed to assign Associated Variables to block probes (cannot duplicate or overwrite those for the std data) */
/*  aname      cname    cells vID  rname   AccumA   ConcA    MnDiam   Disp     LWC      Act     DBZ     Range    Rng_Ref   BmFract  FReset    Strobes */
   "ACCFSPL" ,"CONFSPL" , 16,  0, "FSSP" ,"-","-","-","-","-","-","-","-","-","-","-","-",
   "ACCFSPH" ,"CONFSPH" , 16,  0, "FSSP" ,"-","-","-","-","-","-","-","-","-","-","-","-",
   "ACCFSPH2","CONFSPH2", 16,  0, "FSSP" ,"-","-","-","-","-","-","-","-","-","-","-" ,"-",
/* "ACCUMF"  ,"CONCF"   , 16,  0, "FSSP" ,"SUM15F","CONCF" ,"DBARF" ,"DISPF" ,"PLWCF" ,"TFACT","-"    ,"FRANGE","FRNG"   ,"TFBMFR","TFRESET","TFSTROB",*/
   "ACCUMF"  ,"CONCF"   , 16,  0, "FSSP" ,"SUM15F","FCONC" ,"FDBAR" ,"FDISP" ,"FLWC"  ,"FACT" ,"-"    ,"FRANGE","FRNG"   ,"FBMFR" ,"FRESET" ,"FSTROB" ,
   "ACCUMFS" ,"CONCFS"  , 16,  0, "FSSP" ,"SUM15F","CONCF" ,"DBARF" ,"DISPF" ,"PLWCF" ,"FACT" ,"-"    ,"FRANGE","FRNG"   ,"FBMFR" ,"FRESET" ,"FSTROB" ,
   "ACCUMX"  ,"CONCX"   , 16,  0, "200X" ,"SUM15X","XCONC"  ,"XDBAR","XDISP" ,"XLWC"  ,"-"    ,"XDBZ" ,"-","-","-","-","-",
   "ACCUMY2" ,"CONCY2"  , 16,  0, "200Y" ,"SUM15Y","CONCY" ,"DBARY" ,"DISPY" ,"PLWCY" ,"-"    ,"DBZY","-","-","-","-","-",
   "ACC260L" ,"CON260L" , 64,  2, "260X" ,"-","-","-","-","-","-","-","-","-","-","-","-",
   "ACC260H" ,"CON260H" , 64,  2, "260X" ,"-","-","-","-","-","-","-","-","-","-","-","-",
   "ACCUMX6L","CONCX6L" , 64,  2, "260X" ,"-"     ,"CONC6" ,"DBAR6" ,"-","-","-","-","-","-","-","-","-",
   "ACCUMX6" ,"CONCX6"  , 64,  2, "260X" ,"SUMX6" ,"CONC6" ,"DBAR6" ,"DISP6" ,"PLWC6" ,"-"    ,"DBZ6" ,"-","-","-","-","-",
   "XACC260L","XCON260L", 64,  2, "260X" ,"XSUMX6","XCONC6","XDBAR6","XDISP6","XPLWC6","-"    ,"XDBZ6","-","-","-","-","-",
   "AFS300H" ,"CFS300H" , 16,  0, "300X" ,"-","-","-","-","-","-","-","-","-","-","-","-",
   "ACCUMA"  ,"CONCA"   , 16,  0, "ASAS" ,"SUM15A","ACONC" ,"ADBAR" ,"ADISP" ,"PLWCA" ,"CAACT","-"    ,"-"      ,"-"     ,"-"     ,"-"      ,"-"      ,
   "ACCUMAS" ,"CONCAS"  , 16,  0, "ASAS" ,"SUM15A","CONCA" ,"DBARA" ,"DISPA" ,"-","-","-","-","-","-","-","-",
   "ACCU3"   ,"CONC3"   , 16,  0, "PCAS" ,"-","-","-","-","-","-","-","-","-","-","-","-"
  };
/*  It would be nice to update the GENPRO-II names defined above by
 *   overwriting them with an extant file, if possible.  (One would need
 *   to increase the values of N_PMS1D_STD_NAMES and N_PMS1D_HR_NAMES to
 *   accomodate the added ones.)
 */

int n_pms1d_probes = 0;  /* number of PMS-1D probes to process */

/*   Define unique parameters for each converted PMS-1D variable */
struct _Gpars Pgpars[PMS1D_PROBE_MAX]; /* stuff from GENPRO-II header */
struct _PMS1D pms1d[PMS1D_PROBE_MAX];  /* extra stuff needed for conversion */


/*  GENPRO-II Data  */
/*  --------------  */

char h_buf[32768] = "";     /* GENPRO header record buffer */
/*char h_buf[800] = "";  */ /* GENPRO header record buffer */
/*  Following may not work on the Cray because of its 64-bit word size */
unsigned long *d_buf;       /* GENPRO input integer data buffer pointer */
unsigned long *ig;          /* logical input integer data record pointer */
float *f_buf;               /* GENPRO input IEEE data buffer pointer */
float *igf;                 /* logical input IEEE data record pointer */
NR_TYPE *g_buf;             /* GENPRO converted-data buffer pointer */
NR_TYPE *p_buf;             /* GENPRO PMS-1d converted-data buffer pointer */
unsigned int l_d_rec = 0;   /* logical input data record number */
unsigned int p_d_rec = 0;   /* physical input data record number */


/*  COS-block variables  */
/*  -------------------  */

int infd;                   /* COS-block functions' file descriptor pointer */
int DiskData;               /* Data type flag (COS-blocked or stripped) */


/*  netCDF Output  */
/*  -------------  */

char outfile[128];          /* netCDF output file name */
int ncid = -1;              /* netCDF file descriptor (preset to none) */
unsigned int g_outsize = 0; /* size of netCDF output buffer (bytes) */
unsigned int p_outsize = 0; /* size of netCDF PMS-1D output buffer (bytes) */
char *time_vars[] = {"HOUR", "MINUTE", "SECOND"};  /* required netCDF vars. */
char *time_units[] = {"hr", "min", "sec"}; /* units for required netCDF vars. */
int tbt;                    /* output index of base_time */
int tof;                    /* output index of time_offset */
int time_index;             /* output index of reference time variable,
                                if HOUR, MINUTE and SECOND don't exist */
int Extra_Times = FALSE;    /* created extra time variables, if TRUE */
int ihour;                  /* output index of HOUR */
int iminute;                /* output index of MINUTE */
int isecond;                /* output index of SECOND */
struct tm StartFlight;      /* date/time space for base_time and start time */
struct tm EndFlight;        /* date/time space for end time */
int baseTimeID;             /* netCDF base_time ID */
int timeOffsetID;           /* netCDF time_offset ID */
int varid[NVAR];            /* netCDF variable IDs */
int ndims;                  /* number of netCDF dimensions */
int dims[DIMMAX] = {0,0,0}; /* netCDF dimensions */
int TimeDim;                /* netCDF time dimension ID */
int DimRateID[NRATES];      /* netCDF output rate IDs */
int DimVectorID[N_P_CELLS]; /* netCDF output Vector size IDs */
NR_TYPE missing_val = MISSING_VALUE;

void *data_p[NVAR];     /* output data buffer pointers for netCDF */
char buffer[NSCRATCH];  /* scratch space */


/*  Function Prototypes  */
/*  -------------------  */

int read_hdr(void);                   /* read GENPRO header */
/*int intcompare(int *i, int *j);*/   /* function for library function qsort */
int get_hrec(char line[], size_t num);/* read logical GENPRO header record */
int get_phrec(void);                  /* read physical GENPRO header record */
void get_proj(char *line);            /* get project info */
void get_prd(char *line);             /* get flight date */
void get_comment(char *line);         /* get data source (NCAR or UW) */
void get_exd(char *line);             /* get date processed */
void get_ext(char *line);             /* decode time processed */
void get_filenames(void);             /* query user for I/O file names */
void get_jobid(char *line);           /* get job serial number */
void get_logbit(char *line);          /* get # of bits in logical record */
void get_datlog(char *line);          /* get # of logical recs/physical rec */
void get_datsiz(char *line);          /* get # of bits in physical record */
int get_lkey(char *line, char *key);  /* return leading keyword, if any */
int get_oskeys(char *line);           /* get and save ORDVAR sub-keywords */
int get_lparms(char *line);           /* get decode LETVAR parameters */
int get_names(char *line);            /* get GENPRO names from APPVAR */
int name_idx(char *name);             /* find index of GENPRO variable name */
int nprint(char *line);               /* Check for non-printing characters */
void par_preset(int n);               /* preset variable parameters */
int par_validate(void);               /* validate variable parameters */
int GpOpen(char *file);               /* open GENPRO input file */
int get_ldrec(void);                  /* read logical GENPRO data record */
int get_pdrec(void);                  /* read physical GENPRO data record */
void d_convert(int n);                /* convert raw data to real values */
void d_P_convert(int n);              /* convert raw PMS-1D data to real vals */
int time_var(void);                   /* prepare time variables for netCDF */
void CreateNetCDF(void);              /* create netCDF header */
void to_hms(NR_TYPE *sam, NR_TYPE *hr, NR_TYPE *mi, NR_TYPE *se);
                                  /* convert seconds after midnight to h,m,s */
void setup_pms1d(void);           /* set up PMS-1D variables for conversion */
void Plow(void);                  /* read GENPRO, convert, write netCDF data */
void cleanup(void);               /* Cleanup routine */


#else


/*  Delcarations for functions other than "main"  */
/*  --------------------------------------------  */

extern int d_bug_print;

extern char progname[];

extern int batch_flag;
extern int echo_time;

extern FILE *Gp_fp;
extern char infile[];

extern char *Keyword[];
extern unsigned int Kwfound;
extern unsigned int Kwcritical;
extern char *Oskeyword[];
extern int G_Format;

extern int nhrec;
extern int nOsk;
extern int Osklist[];
extern int nvars;
extern int Nvars;

extern char aircraft[];
extern int projno;
extern int fltno;
extern char ftype;
extern char segment;
extern char project[];
extern int prdate[];
extern int exdate[];
extern int extime[];
extern char jobid[];
extern int logbit;
extern int datlog;
extern int datsiz;

extern struct _Gpars Gpars[];

extern int nRates;
extern int Rates[];

extern int n_pms1d_probes;
extern struct _Gpars Pgpars[];
extern struct _PMS1D pms1d[];
extern struct _PMS1D_type pms1d_std[];
extern struct _PMS1D_type pms1d_hr[];

extern char h_buf[];
extern unsigned long *d_buf;
extern unsigned long *ig;
extern float *f_buf;
extern float *igf;
extern NR_TYPE *g_buf;
extern NR_TYPE *p_buf;
extern unsigned int l_d_rec;
extern unsigned int p_d_rec;

extern char outfile[];
extern int ncid;
extern unsigned int g_outsize;
extern unsigned int p_outsize;
extern char *time_vars[];
extern char *time_units[];
extern int tbt;
extern int tof;
extern int time_index;
extern int Extra_Times;
extern int ihour;
extern int iminute;
extern int isecond;
extern struct tm StartFlight;
extern struct tm EndFlight;
extern int baseTimeID;
extern int timeOffsetID;
extern int varid[];
extern int ndims;
extern int dims[];
extern int TimeDim;
extern int DimRateID[];
extern int DimVectorID[];
extern NR_TYPE missing_val;

extern void *data_p[];
extern char buffer[];

extern int infd;
extern int DiskData;

#endif
