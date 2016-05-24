      PROGRAM MCOPY
C    &          , Multiple-tape data set copy          RLR <910222.1000>
C
C    * * * * * * * * * * * * * * * * * * * * * * * * *
C    *  Copyright 1993, University Corporation for   *
C    *  Atmospheric Research, All Rights Reserved.   *
C    * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C  Copy a GENPRO MSS data set to multiple output data sets which will fit on
C    physical magnetic tapes.  Uses Cray blocked data sets.
C  Written by Ron Ruth   5-22 February 1991
C
C  Questions or comments about this program can be directed to Ron Ruth
C    at (303) 497-1048 or (303) 497-8815.
C
C
C  Parameters:
C     Name     Type               Description
C    ------   --------  ----------------------------------------------------
C    MAXVAR   Integer   Maximum number of variables
C    MAXHDR   Integer   Maximum number of header records
C    HDRSIZ   Integer   Size of header record (characters)
C    IBUFSZ   Integer   Size of GENPRO data record buffer (Cray words)
C    KEYSIZ   Integer   Size of a keyword (characters)
C    DREGS    Real      Extra tape saved for errors or rewrite (per cent)
C    FILENM   Charactr  Default name prefix for output files
C
C  Local variables:
C     Name     Type                     Description
C    ------   --------  ----------------------------------------------------
C    DDAREC   Real      Length of data record (inches), including IRG
C    DENS     Real      Tape density 
C    DHDREC   Real      Length of header record (inches), including IRG
C    I        Integer   DO loop index
C    IBUF     Integer   Input buffer for GENPRO data
C    IDENS    Integer   Requested tape density 
C    IERR     Integer   Returned error code
C    IHEADR   Charcatr  Input buffer for GENPRO header
C    IRSTR    Charactr  Decoded command line parameter
C    ISTS     Integer   Status of read error
C    IWSIZ    Integer   Number of records on last output tape
C    J        Integer   Do implied index
C    JIRG     Integer   Index into IRGSIZ array
C    LENGTH   Integer   Length of requested output tapes
C    LNGTH    Integer   Adjusted length of requested output tapes
C    LQUIT    Logical   Flag to quit processing, if .TRUE.
C    LUIN     Integer   Input data unit number
C    LUOUT    Integer   Output data unit number
C    NCHRUN   Integer   Number of characters in a command-line argument
C    NRCLST   Integer   Number of data records written to last tape
C    NREC2W   Integer   Number of data records to write on one output tape
C    NTAPES   Integer   Number of output tapes required
C    ONUM     Charctr   Output file number
C    OSTS     Integer   Status of write error
C    OUTFIL   Charactr  Output file name
C    IWSIZ    Integer   Number of records on last output tape
C    RVDATE   Charactr  Revision date
C    RVTIME   Charactr  Revision time
C    SIZMRK   Real      Size of file mark (inches)
C    SIZIRG   Real      Sizes of inter-record gap (inches)
C    STDOUT   Real      Unit number of standard output
C    XLENRM   Real      Length of tape available for data records (inches)
C
C  Common Block Variables changed by this routine:
C   Labeled  Variable
C   COMMON   Name     Type      Description
C   ------   ------   -------   ------------------------------------------
C    none
C
C  Non-UNICOS functions and subroutines used:
C    Name     S/F   Type                    Usage/Author
C    -----    ----  -------  -----------------------------------------------
C    GENHDR   Subr           Read, print and decode GENPRO header records
C
C  UNICOS functions and subroutines used:
C    Name     S/F   Type                    Usage
C    -----    ----  -------  -----------------------------------------------
C    EXIT     Subr           Quit processing
C    GETARG   Subr           Retrieve command-line argument
C    IARGC    Fcn   Integer  Number of command-line arguments
C
C  NCAR Graphics subroutines used:
C    Name                            Usage
C    -----                   -----------------------------------------------
C    none
C
C  Written by Ronald L. Ruth                               <910205.1000> **
C  Revised by RLR 
C%  Add revision information above this line.
C
C
C     IMPLICIT UNDEFINED [A-Z]
      IMPLICIT NONE
C
C
C-----> SET PARAMETERS
      INTEGER MAXVAR, MAXHDR, HDRSIZ, HDRSZD, KEYSIZ, IBUFSZ
      REAL DREGS
      CHARACTER*6 FILENM
      PARAMETER (MAXVAR=300, MAXHDR=150, HDRSIZ=100, KEYSIZ=8,
     &           HDRSZD=HDRSIZ/10, IBUFSZ=5000, DREGS=10.,
     &           FILENM="OUTPUT")
C
C----->  GENPRO header data block
      COMMON / GENBLK / ILGBIT,IDATLG,IRATE(MAXVAR),IPOS(MAXVAR),
     &                  TERM(MAXVAR),FACTR(MAXVAR),NVAR,NLOGRC,IDSIZ,
     &                  INBUF(HDRSIZ,MAXHDR), NHDREC
      INTEGER ILGBIT,IDATLG,IRATE,IPOS,NVAR,NLOGRC,IDSIZ, NHDREC
      REAL TERM,FACTR
      CHARACTER*(KEYSIZ) INBUF
C
C----->  Local variables
      INTEGER  I, IERR, IBUF(IBUFSZ), LUIN, LUOUT, LENGTH
      INTEGER  NCHRUN, LNGTH, IDENS, JIRG, STDOUT
      INTEGER  NREC2W, IWSIZ, NTAPES, J, ISTS, NRCLST, OSTS
      REAL DENS, DDAREC, DHDREC, XLENRM, SIZMRK, SIZIRG(3) 
      LOGICAL LQUIT
      CHARACTER*2 ONUM
      CHARACTER*4 RVTIME
      CHARACTER*6 RVDATE
      CHARACTER*8 OUTFIL
      CHARACTER*256 IRSTR
C
C----->  External references
      EXTERNAL EXIT, GENHDR, GETARG, IARGC
      INTEGER IARGC, GETARG
C
C----->  Initial values
C   Inter-record gap sizes (inches) for 6250, 1600 and lower-density tapes.
      DATA SIZIRG /                     0.3,  0.6,     0.75 /
C   Maximum file mark size (inches).
      DATA SIZMRK / 6.0 /
C   Initialize "quit" flag.
      DATA LQUIT / .FALSE. /
C   Define standard output unit.
      DATA STDOUT / 6 /
C%  Define latest revision
      RVDATE = "910222"
      RVTIME = "1000"
C
C
C             * * * * * * * * * * * * * * *
C            *                             *
C           *  Executable code starts here  *
C            *                             *
C             * * * * * * * * * * * * * * *
C
C  Give user a startup message.
      WRITE (STDOUT,10100) RVDATE, RVTIME
10100 FORMAT(//"  MCOPY:   RDP Multiple-tape copy program  RLR  rev. <"
     &       A6"."A4">"//)
C
C  Do I have the right number of command-line arguments ?
      IF (IARGC().NE.4) THEN
C   No.  Give user argument format and quit.
        WRITE (STDOUT,10200)
10200   FORMAT ("  MCOPY Usage:  mcopy inunint outunit tapelength tapede
     &nsity"/
     &  "   where:  inunit = FORTRAN unit number (1-99) of input data"/
     &  "          outunit = FORTRAN unit number (1-99) of output data"/
     &  "       tapelength = length of destination tapes (<=3600 feet)"/
     &  "      tapedensity = 800, 1600 or 6250 cpi")
        STOP 00001
      END IF
C
C   Get first argument (input unit).
      NCHRUN = GETARG (1,IRSTR)
CD
C     WRITE (STDOUT,1111) NCHRUN, IRSTR(1:NCHRUN)
C1111 FORMAT ("  MCOPY:  First argument has"I6" chars = "A)
CD
      READ (IRSTR,10250,IOSTAT=IERR,ERR=5000) LUIN
10250 FORMAT (I5)
C   Check for validity.
      IF (LUIN.LT.1 .OR. LUIN.GT.99) THEN
        WRITE (STDOUT,10300) LUIN
10300   FORMAT ("  MCOPY:  Requested input unit number ("I6") illegal.")
        LQUIT = .TRUE.
      END IF
C   Get second argument (output unit).
      NCHRUN = GETARG (2,IRSTR)
      READ (IRSTR,10250,IOSTAT=IERR,ERR=5000) LUOUT
C   Check for validity.
      IF (LUOUT.LT.1 .OR. LUOUT.GT.99) THEN
        WRITE (STDOUT,10400) LUIN
10400   FORMAT ("  MCOPY:  Requested output unit number ("I6") illegal."
     &)
        LQUIT = .TRUE.
      END IF
C   Get third argument (tape length in feet).
      NCHRUN = GETARG (3,IRSTR)
      READ (IRSTR,10250,IOSTAT=IERR,ERR=5000) LENGTH
C   Check for validity.
      IF (LUOUT.LT.1 .OR. LUOUT.GT.3600) THEN
        WRITE (STDOUT,10500) LENGTH
10500   FORMAT ("  MCOPY:  Requested output tape length ("I8") illegal."
     &)
        LQUIT = .TRUE.
      END IF
C   Adjust tape length to standard values - DREGS (per cent) for safety.
      LNGTH = 3600*(1.-0.01*DREGS)
      IF (LENGTH.LT.3550) LNGTH = 2400*(1.-0.01*DREGS)
      IF (LENGTH.LT.2350) LNGTH = 1200*(1.-0.01*DREGS)
      IF (LENGTH.LT.1150) LNGTH = 600*(1.-0.01*DREGS)
C   Get fourth argument (tape density in cpi).
      NCHRUN = GETARG (4,IRSTR)
      READ (IRSTR,10250,IOSTAT=IERR,ERR=5000) IDENS
C   Check for validity.
      IF      (IDENS.EQ.6250) THEN
        JIRG = 1
      ELSE IF (IDENS.EQ.1600) THEN
        JIRG = 2
      ELSE IF (IDENS.EQ.800) THEN
        JIRG = 3
      ELSE
        WRITE (STDOUT,10600) IDENS
10600   FORMAT ("  MCOPY:  Cannot handle requested tape density ("I8")."
     &)
        LQUIT = .TRUE.
      END IF
      DENS = IDENS
C  If erroneous input, quit.
      IF (LQUIT) THEN
        WRITE (STDOUT,10200)
        STOP 00002
      END IF
CD
C     WRITE (STDOUT,1234) LUIN, LUOUT, LENGTH, IDENS
C1234 FORMAT ("  MCOPY:  LUIN="I6", LUOUT="I6", LENGTH="I6", IDENS="I6)
CD
      WRITE (STDOUT,10700) LNGTH
10700 FORMAT ("  MCOPY:  Tape length to be written (minus safety factor)
     & = "I4" feet."//)
C
C Open input data file for reading.
      OPEN (UNIT=LUIN,FILE='LDATA',STATUS='OLD',FORM='UNFORMATTED')
C
C  Read and decode GENPRO header.
      CALL GENHDR(LUIN,STDOUT)
C
C  Calculate number of data sets required.
C
C    Length of one tape equals:
C      Number of header records * (length of header record + IRG)
C             plus
C      Length of a file mark
C             plus
C      Number of data records * (Length of data record + IRG)
C             plus
C      Length of a file mark
C             plus
C      Length of a file mark
C
C  Length of header record (inches)
      DHDREC = HDRSIZ/DENS + SIZIRG(JIRG)
C  Length remaining for data records (inches)
      XLENRM = LNGTH*12. - NHDREC*DHDREC - 3.0*SIZMRK
C  Length of data record (8 bits/character) (inches)
      DDAREC = IDSIZ/8./DENS + SIZIRG(JIRG)
C  Number of data records to write on one tape
      NREC2W = XLENRM/DDAREC
C  Word size of data record (Cray words are 64 bits)
      IWSIZ = IDSIZ/64
CD
C     WRITE (STDOUT,2345) DHDREC, XLENRM, DDAREC, NREC2W, IWSIZ
C2345 FORMAT ("  MCOPY:  Length of header record (inches) ="F9.3/
C    &        "          Length remaining for data records (inches) ="
C    & F10.3/
C    &        "          Length of data record (8 bits/character) (inche
C    &s) ="F9.3/
C    &        "          Number of data records to write on one tape ="
C    & I6
C    &        "          Word size of data record (Cray words are 64 bit
C    &s) ="I6)
CD
C
C  Copy data.
      NTAPES = 1
C
C  Entry point for another tape.
 1000 CONTINUE
C
C  Get new file name.
      IF (NTAPES.LT.10) THEN
        WRITE (ONUM,'(I1)') NTAPES
        OUTFIL = FILENM//"0"//ONUM
        WRITE (STDOUT,11010) OUTFIL
11010   FORMAT ("  MCOPY:  New output file name = "A)
      ELSE IF (NTAPES.LT.100) THEN
        WRITE (ONUM,'(I2)') NTAPES
        OUTFIL = FILENM//ONUM
        WRITE (STDOUT,11010) OUTFIL
      ELSE
        WRITE (STDOUT,11020)
11020   FORMAT ("  MCOPY:  Cannot handle more than 99 files."/
     &          "    Quitting.")
        LQUIT = .TRUE.
        GO TO 4500
      END IF
C  Open new output data file for writing.
      OPEN (UNIT=LUOUT,FILE=OUTFIL,STATUS='NEW',FORM='UNFORMATTED',
     &      IOSTAT=IERR,ERR=6000)
C  Copy header
      DO 2000 I=1,NHDREC
        WRITE (LUOUT) (INBUF(J,I),J=1,HDRSIZ)
 2000 CONTINUE
      ENDFILE (LUOUT)
C  Copy data
      DO 3000 I=1,NREC2W
        BUFFER IN (LUIN,0) (IBUF(1),IBUF(IWSIZ))
        ISTS = UNIT(LUIN)
        IF (ISTS.GE.0) GO TO 4000
        BUFFER OUT (LUOUT,0) (IBUF(1),IBUF(IWSIZ))
        OSTS = UNIT(LUOUT)
        IF (OSTS.GE.0) GO TO 3500
 3000 CONTINUE
      ENDFILE (LUOUT)
      ENDFILE (LUOUT)
      CLOSE (UNIT=LUOUT,IOSTAT=IERR,ERR=6500)
C  Finished with one tape; go do another.
      NTAPES = NTAPES + 1
      GO TO 1000
C  Problem with output write.
 3500 CONTINUE
      WRITE (STDOUT,35100) OSTS, NTAPES, OUTFIL
35100 FORMAT ("  MCOPY:  Status ="I6" writing output tape #"I2" (filenam
     &e = "A".")
      LQUIT = .TRUE.
      GO TO 4000
C
C  Done.
 4000 CONTINUE
C   Write endfile on the last tape.
      ENDFILE (LUOUT)
      CLOSE (LUOUT)
C   Give user data about tape writes.
 4500 CONTINUE
      NRCLST = I - 1
      WRITE (STDOUT,45100) NHDREC, NREC2W, NRCLST
45100 FORMAT (/"  MCOPY processing summary:"/
     &        4XI6" header records on a tape."/
     &        4XI6" data records on a full tape."/
     &        4XI6" data records on the last tape.")
      IF (NTAPES.GT.1) THEN
        WRITE (STDOUT,45200) NTAPES
45200   FORMAT (4XI6" tapes are required to copy this data set.")
      ELSE
        WRITE (STDOUT,45300) NTAPES
45300   FORMAT (4XI6" tape is required to copy this data set.")
      END IF
      IF (LQUIT) THEN
        WRITE (STDOUT,45400)
45400   FORMAT (4X" Quitting.")
        STOP 00007
      ELSE
        WRITE (STDOUT,45500)
45500   FORMAT (4X"  Successful completion.")
        CALL EXIT
      END IF
C
 5000 CONTINUE
C  Process erroneous internal read.
      WRITE (STDOUT,50100) IERR, IRSTR
50100 FORMAT ("  MCOPY:  Error"I6" decoding command line string = "A)
      LQUIT = .TRUE.
      GO TO 4500
 6000 CONTINUE
C  Process file open error.
      WRITE (STDOUT,60100) IERR
60100 FORMAT ("  MCOPY:  Error"I6" opening file = "A)
      LQUIT = .TRUE.
      GO TO 4500
 6500 CONTINUE
C  Process file close error.
      WRITE (STDOUT,65100) IERR
65100 FORMAT ("  MCOPY:  Error"I6" closing file = "A)
      LQUIT = .TRUE.
      GO TO 4500
C
      END
      SUBROUTINE GENHDR (LUIN,STDOUT)
C
C   SUBROUTINE GENHDR WILL READ A GENPRO II HEADER, WRITE IT OUT (TO
C    LOGICAL UNIT NUMBER STDOUT), STORE THE VALUES NECESSARY TO READ THE
C    DATA FILE, AND WRITE (TO LOGICAL UNIT NUMBER STDOUT) A TABLE OF THE
C    VARIABLES, THEIR INDICES WITHIN THE DATA ARRAY (WHEN CREATED) AND
C    THEIR RATES.
C
C   Modified from standard code by Ron Ruth to save an image of the
C    header in memory.  To be used with a data copy program which
C    needs to write more than one header file to output media.
C
C  Arguments:
C     Name     Type     Usage          Description
C    ------   -------   -----   --------------------------------------------
C    LUIN     Integer   Input   Unit number for source data
C    STDOUT   Integer   Input   Unit number for standard output
C
C  Parameters:
C     Name     Type                    Description
C    ------   -------   ----------------------------------------------------
C    MAXVAR   Integer   Maximum number of variables
C    MAXHDR   Integer   Maximum number of header records
C    HDRSIZ   Integer   Size of header record (characters)
C    KEYSIZ   Integer   Size of keywords (characters)
C    HDRLRS   Integer   Size of logical header record (characters)
C    NLHDRC   Integer   Number of logical header records in physical header rec
C    NUMKEY   Integer   Number of keywords in a physical header record
C
C  Local variables:
C     Name     Type                    Description
C    ------   -------   ----------------------------------------------------
C    DATLOG   Charctr   Eight-character identifier ' DATLOG '.
C    DATSIZ   Charctr   Eight-character identifier ' DATSIZ '.
C    DEBUF    Charctr   Array to decode numeric values from an ASCII
C                         representation of a logical header record
C                         (equivalenced to array INBUF)
C    I        Integer   DO loop index; DO implied list index
C    IERR     Integer   Error number returned by READ operation
C    IFBIT    Integer   First bit position within record for decode
C    INDX     Integer   Index into the INBUF array
C    INDXD    Integer   Index into the DEBUF array
C    LETVAR   Charctr   Eight-character identifier ' LETVAR '.
C    LHEADR   Logical   It's a GENPRO header, if .TRUE.
C    LOGBIT   Charctr   Eight-character identifier ' LOGBIT '.
C    LUIN     Integer   Input data unit number
C    NORDV    Integer   Variable to determine the number of times the
C                         ' ORDVAR ' identifier has been encountered
C    NRATE    Integer   Number of IRATE values read
C    NREC     Integer   Counter for the number of physical header
C                         records read
C    NTERM    Integer   Number of TERM and FACTR values read
C    ORDVAR   Charctr   Eight-character identifier ' ORDVAR '.
C    PROJCT   Charctr   Eight-character identifier '/PROJECT'.
C    VNAME    Charctr   Array to store the variable names
C
C  Common Block Variables changed by this routine:
C   Labeled  Variable
C   COMMON    Name     Type                  Description
C   ------   ------   -------   -----------------------------------------------
C   GENBLK   ILGBIT   Integer   Number of bits per logical data record
C   GENBLK   IDATLG   Integer   Number of logical data recs per physical rec
C   GENBLK   IRATE    Integer   Array of sample rates for each variable in a
C                                 data record array
C   GENBLK   IPOS     Integer   Array of index positions for each variable
C   GENBLK   TERM     Integer   Array of term values for each variable
C   GENBLK   FACTR    Integer   Array of factor values for each variable
C   GENBLK   NVAR     Integer   Number of variables in a data record array
C   GENBLK   NLOGRC   Integer   Counter to determine the current logical data
C   GENBLK   IDSIZ    Integer   Number of bits per physical data record
C                                 record being processed
C   GENBLK   INBUF    Integer   Array to check for an eight-character
C                                 identifier at the beginning of a logical
C                                 header record (equivalenced to array DEBUF)
C   GENBLK   NHDREC   Integer   Number of header records
C
C  Non-UNICOS functions and subroutines used:
C    Name     S/F   Type                    Usage/Author
C    -----    ----  -------  -----------------------------------------------
C    none
C
C  UNICOS functions and subroutines used:
C    Name     S/F   Type                    Usage
C    -----    ----  -------  -----------------------------------------------
C    none
C
C  NCAR Graphics subroutines used:
C    Name                            Usage
C    -----                   -----------------------------------------------
C    none
C
C  Written by Mike Daniels                                 <840930.0000> **
C  Revised by RLR to use with multiple-tape copy program   <910208.1445> **
C  Revised by RLR to eliminate unwanted standard output    <910221.1730> **
C%  Add revision information above this line.
C
C
C     IMPLICIT UNDEFINED [A-Z]
      IMPLICIT NONE
C
C
C-----> SET PARAMETERS
      INTEGER MAXVAR, MAXHDR, HDRSIZ, KEYSIZ, HDRLRS, NLHDRC
      PARAMETER (MAXVAR=300, MAXHDR=150, HDRSIZ=100, KEYSIZ=8,
     &           HDRLRS=80, NLHDRC=HDRSIZ*KEYSIZ/HDRLRS)
C
C
      COMMON / GENBLK / ILGBIT,IDATLG,IRATE(MAXVAR),IPOS(MAXVAR),
     &                  TERM(MAXVAR),FACTR(MAXVAR),NVAR,NLOGRC,IDSIZ,
     &                  INBUF(HDRSIZ,MAXHDR), NHDREC
      INTEGER ILGBIT,IDATLG,IRATE,IPOS,NVAR,NLOGRC,IDSIZ, NHDREC
      REAL TERM,FACTR
      CHARACTER*(KEYSIZ) INBUF
C
      CHARACTER*(KEYSIZ) VNAME(MAXVAR),LOGBIT,DATLOG,ORDVAR,LETVAR,
     &                   DATSIZ,PROJCT
      CHARACTER*(HDRLRS) DEBUF(NLHDRC,MAXHDR)
      EQUIVALENCE (DEBUF,INBUF)
C
C
      INTEGER NORDV, NRATE, NTERM, NREC, LUIN, IERR, I, INDXD
      INTEGER INDX, STDOUT, IFBIT
      LOGICAL LHEADR
C
C
C-----> INITIALIZE THE IDENTIFIERS
      DATA LOGBIT /' LOGBIT '/
      DATA DATLOG /' DATLOG '/
      DATA ORDVAR /' ORDVAR '/
      DATA PROJCT /'/PROJECT'/
      DATA LETVAR /' LETVAR '/
      DATA DATSIZ /' DATSIZ '/
C
C-----> INITIALIZE THE COUNTERS
      DATA NORDV /0/
      DATA NRATE /0/
      DATA NTERM /0/
      DATA NREC  /0/
C
C-----> INITIALIZE LHEADR
      DATA LHEADR / .FALSE. /
C
C  Data for subroutine RDTAPE  (6400 bits/record = 100 words = 800 chars)
C     DATA IMODE /1/, ITYPE /1/, LEN / 100 /
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
CD
C     WRITE (STDOUT,1111) MAXVAR, MAXHDR, HDRSIZ, KEYSIZ, HDRLRS,
C    &                    NLHDRC
C1111 FORMAT (" MCOPY:  MAXVAR, MAXHDR, HDRSIZ, KEYSIZ, HDRLRS, NLHDRC"
C    & /      "        ",6I8)
CD
   10 NREC=NREC+1
      IF (NREC.GT.MAXHDR) THEN
        WRITE (STDOUT,10010) MAXHDR
10010   FORMAT ("  MCOPY:  Exceeded program's header storage capacity ("
     &          I4" records).")
        STOP 00003
      END IF
      READ (LUIN,ERR=20,IOSTAT=IERR,END=150) (INBUF(I,NREC),I=1,HDRSIZ)
C     CALL RDTAPE (LUIN,IMODE,ITYPE,DEBUF(1,NREC),LEN)
C     CALL IOWAIT (LUIN,NSTATE,LHDREC)
C     IF (NSTATE.EQ.0) GO TO 40
C     IF (NSTATE.EQ.1) GO TO 150
      GO TO 40
C  Bad record
   20 CONTINUE
      WRITE (STDOUT,10020) IERR, NREC, LUIN
10020 FORMAT ("  MCOPY:  FORTRAN error number"I6" reading header record 
     &#"I4" from unit"I6".")
      STOP 00004
C
C-----> GOOD READ
   40 CONTINUE
      DO 120 INDX=1,HDRSIZ,NLHDRC
        INDXD=INDX/NLHDRC+1
C-----> WRITE OUT EACH LOGICAL RECORD WHILE SEARCHING FOR THE
CD
C-----> IDENTIFIERS
C       WRITE (STDOUT,1234) INDXD,NREC
C1234   FORMAT ("  MCOPY:  DEBUF ARRAY ("I6","I6")")
CD
C       WRITE (STDOUT,60) DEBUF(INDXD,NREC)
   60   FORMAT(1X,A80)
        IF (INBUF(INDX,NREC).EQ.LOGBIT) THEN
C
C-----> DECODE THE NUMBER OF BITS PER LOGICAL DATA RECORD
          WRITE (STDOUT,60) DEBUF(INDXD,NREC)
          READ(DEBUF(INDXD,NREC),80,IOSTAT=IERR,ERR=300) ILGBIT
   80     FORMAT(10X,I8,62X)
          LHEADR = .TRUE.
        ELSE IF (INBUF(INDX,NREC).EQ.DATLOG) THEN
C
C-----> DECODE THE NUMBER OF LOGICAL DATA RECORDS PER PHYSICAL DATA
C-----> RECORD
          WRITE (STDOUT,60) DEBUF(INDXD,NREC)
          READ(DEBUF(INDXD,NREC),90,IOSTAT=IERR,ERR=300) IDATLG
   90     FORMAT(10X,I4,66X)
          LHEADR = .TRUE.
        ELSE IF (INBUF(INDX,NREC).EQ.DATSIZ) THEN
C
C-----> DECODE THE NUMBER OF BITS PER PHYSICAL RECORD
          WRITE (STDOUT,60) DEBUF(INDXD,NREC)
          READ(DEBUF(INDXD,NREC),80,IOSTAT=IERR,ERR=300) IDSIZ
          LHEADR = .TRUE.
        ELSE IF (INBUF(INDX,NREC).EQ.PROJCT) THEN
C----->  If I find the project line in the header, it's GENPRO alright!
          LHEADR = .TRUE.
          WRITE (STDOUT,60) DEBUF(INDXD,NREC)
        ELSE IF (INBUF(INDX,NREC).EQ.ORDVAR) THEN
          LHEADR = .TRUE.
C         WRITE (STDOUT,60) DEBUF(INDXD,NREC)
          NORDV=NORDV+1
        ELSE IF (INBUF(INDX,NREC).EQ.LETVAR) THEN
          LHEADR = .TRUE.
C         WRITE (STDOUT,60) DEBUF(INDXD,NREC)
          IF (NORDV.EQ.2) THEN
C
C-----> DECODE A VARIABLE'S SAMPLE RATE, FROM THE BIT LOCATION
C-----> COMPUTE THE INDEX INTO A LOGICAL DATA RECORD AND READ THE
C-----> VARIABLE NAME
          NRATE=NRATE+1
          READ(DEBUF(INDXD,NREC),100,IOSTAT=IERR,ERR=300) IRATE(NRATE),
     &                                                    IFBIT,
     &                                                    VNAME(NRATE)
  100     FORMAT(28X,I6,5X,I8,15X,A8,10X)
          IPOS(NRATE)=((IFBIT-1)/32)+1
        ELSE IF (NORDV.EQ.3) THEN
C
C-----> DECODE A VARIABLE'S TERM AND FACTR
          NTERM=NTERM+1
          READ(DEBUF(INDXD,NREC),110,IOSTAT=IERR,ERR=300) TERM(NTERM),
     &                                                    FACTR(NTERM)
  110     FORMAT(16X,F15.8,1X,F14.8,34X)
        END IF
      END IF
  120 CONTINUE
      GO TO 10
C
C-----> EOF SENSED
  150 CONTINUE
      IF (LHEADR) THEN
        NHDREC = NREC - 1
        WRITE (STDOUT,160) NHDREC
  160   FORMAT(/,'  MCOPY:  >>> EOF SENSED ON HEADER FILE AFTER ',I4,' R
     &ECORDS',/)
C
C-----> ASSIGN THE NUMBER OF VARIABLES AND SET THE NLOGRC POINTER TO
C-----> IDATLG SO THAT WHEN GENDAT IS CALLED A PHYSICAL DATA RECORD IS
C-----> READ IMMEDIATELY
C
        NVAR=NRATE
        NLOGRC=IDATLG
C
C-----> WRITE OUT A VARIABLE TABLE
C
C       WRITE (STDOUT,170)
C 170   FORMAT (/1X"    MCOPY"/1X' NAME  ',5X,'INDEX',5X,'RATE',/)
C       DO 180 I=1,NVAR
C 180   WRITE (STDOUT,190) VNAME(I),IPOS(I),IRATE(I)
C 190   FORMAT (1X,A8,5X,I5,5X,I4)
        RETURN
      ELSE
C-----> NO VALID HEADER DATA FOUND
        WRITE (STDOUT,200)
  200   FORMAT (/1X"   MCOPY:  Header information not found."/
     &           "             Data are not in GENPRO format."/
     &           "               Quitting."//)
        STOP 00005
      END IF
C
C-----> ERRONEOUS HEADER DATA FOUND
  300 CONTINUE
      WRITE (STDOUT,350) IERR
  350 FORMAT (/1X"   MCOPY:  Error"I6" decoding header information."/
     &         "             Data probably not in GENPRO format."/
     &          "              Quitting."//)
      STOP 00006
      END
