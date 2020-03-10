C GPGTSEGS -- modified Adsraw program to pick up start/stop times
C of valid time segments from raw input tapes. To be run on the Cray
C Y-MP under UNICOS, with multiple flight tapes as input, and a SEGMNTS
C file generated as output.
C
C Revised by Ronald L. Ruth for UNICOS,                       18 September 1990
C Revised by RLR to read two records for header info (handles
C    Exabyte tapes properly)                                  12 December 1990
C
C    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
C  *                                                                         *
C  *  C A U T I O N   -   This program uses the Cray routine IEXEC which     *
C  *                       has now been superseded by PSHELL/ISHELL.         *
C  *                      SCD recommends that IEXEC be abandoned in favor    *
C  *                       of the new routines.                              *
C  *                                                                         *
C    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
C
C  Variables added for multiple-tape processing
C     DUMY -- Dummy character array for reading data file titles
C     COMMON/GTVOL/ -- variables used in processing tape volumes
C
      PROGRAM GTSEG
C
      DIMENSION NFREQ(20),NCHANL(20),ITIME(3),NDX(120),LINK(122)
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT
      COMMON/FLTX/FLTTYP
      CHARACTER*2  FLTTYP
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)
C-----                  DME(1),LORN(91)
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)
      COMMON/TAPTIM/SEGGO(3),SEGEND(3),PRTROW(10),SAVTIM,FORWRD
      LOGICAL FORWRD
      INTEGER SEGGO,SEGEND
      INTEGER DUMMY,CONKEY,XFLAG,DFLAG
C
C  Common block for acquiring a new tape volume
      COMMON /GTVOL/ NUMTAP(500), NUMVOL,IFLGHT, IERR, IEXERR, LERR, LEOT
      INTEGER        NUMTAP,     NUMVOL,IFLGHT, IERR, IEXERR
      LOGICAL                                                 LERR, LEOT
      COMMON /GTVOL/ IFMT, STRING, VOLUME(500,20), CFLT(500)
      CHARACTER*80   IFMT, STRING
      CHARACTER*6                  VOLUME
      CHARACTER*4                                 CFLT
C  COMMON /GTVOL/ Variables 
C     NUMTAP -- Number of tape volumes available (per flight number)
C     NUMVOL -- Tape volume number to process
C     IFLGHT -- Flight number to process
C     IERR   -- Volume open error return code
C     IEXERR -- Unix command Error return code
C     LERR   -- Error occurred closing IUNIT, if .TRUE.
C     LEOT   -- No more tapes to process, if .TRUE.
C     IFMT   -- Format for system call to assign the unit number
C     STRING -- UNIX command sent to UNICOS
C     VOLUME -- Array of tape labels (flight number, number within flight)
C     CFLT   -- Array of flight titles
C
C
C-----Define variable needed for multiple-tape processing.
      CHARACTER*80 DUMY
      LOGICAL LRETRY
      DATA LRETRY / .TRUE. /
C
      DATA IUNIT / 9 /
C
 1000 FORMAT(2X,'PARITY ERROR ON HEADER RECORD OF TAPE VOLUME 'A)
 1020 FORMAT(10A8)
 1030 FORMAT(I5)
 1040 FORMAT(2X,'EOF ON TAPE VOLUME 'A)
 1050 FORMAT(2X,'EOT ON TAPE VOLUME 'A)
 2200 FORMAT (1X' ERROR 'I3' CLOSING TAPE VOLUME 'A)
 2300 FORMAT (1X' ERROR 'I3' ASSIGNING TAPE 'A' TO UNIT'I2)
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
C------------  Start mod for multiple-tape processing   RLR  900918
C------------    Code within entire loop was reordered and streamlined
C
C----- Open COMPFLT file
      OPEN (UNIT=7,STATUS='OLD',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &       IOSTAT=IERR,ERR=600)
      REWIND 7
C----- I don't need the first record
      READ (7,'(A80)',END=3,ERR=2,IOSTAT=IERR) DUMY
C
C----- Initialize variables
      NUMVOL = 0
      IFLGHT = 1
C
C----- Read tape volumes to process from COMPFLT file
    1 CONTINUE
      READ (7,'(A80)',END=4,ERR=2,IOSTAT=IERR) STRING
      READ (STRING,'(A6)') DUMY
C----- Records which don't start with COMP are tape volumes
      IF (DUMY(1:4).NE.'COMP') THEN
        NUMVOL = NUMVOL + 1
        VOLUME(NUMVOL,IFLGHT) = DUMY(1:6)
      ELSE
        IF (NUMVOL.LT.1) THEN
          WRITE (6,'("  Flight "A5" has no tape assigned.")')
        END IF
        NUMTAP(IFLGHT) = NUMVOL
        CFLT(IFLGHT) = STRING(17:21)
        IFLGHT = IFLGHT + 1
        NUMVOL = 0
      END IF
      GO TO 1
C----- Error with COMPFLT file
    2 CONTINUE
      WRITE (6,'(" Error "I5" accessing COMPFLT file")') IERR
      STOP 2
    3 CONTINUE
      WRITE (6,'(" Premature EOF accessing COMPFLT file")')
      STOP 3
C
C----- Finished with COMPFLT file for now.
    4 CONTINUE
      IFLGHT = IFLGHT - 1
      IF (IFLGHT.LT.1) THEN
        WRITE (6,'("  No flights found..")')
        STOP 4
C      ELSE
C----- Informational print, for now
C        WRITE (6,10100) ((CFLT(J),VOLUME(I,J),I=1,NUMTAP(J)),J=1,IFLGHT)
C10100   FORMAT (3X" Flight "A" Tape # "A)
      END IF
      REWIND 7
C
C----- Initialize variables (again)
      MXFLT = IFLGHT
      IFLGHT = 1
      NUMVOL = 0
C
C----- Process each of the flights
    5 CONTINUE
      LRETRY = .TRUE.
      NUMVOL=NUMVOL + 1
      IF (NUMVOL.GT.NUMTAP(IFLGHT)) THEN
        IFLGHT = IFLGHT + 1
        IF (IFLGHT.GT.MXFLT) STOP 0
        NUMVOL = 1
      END IF
C----- Informational print
C      WRITE (6,10200) NUMVOL, CFLT(IFLGHT), VOLUME(NUMVOL,IFLGHT)
C10200 FORMAT (3X"GTSEGS Assigning tape # "I2" for flight "A", tape "A)
      CALL GETVOL (IUNIT)
      IF (LEOT) STOP 5
      IF (LERR) THEN
        IF (IERR.NE.0) GO TO 220
        IF (IEXERR.NE.0) GO TO 230
      END IF
C-----READ NEW HEADER RECORD
    8 CALL RDTAPE(IUNIT,1,2,NBUF,1200)
      CALL IOWAIT(IUNIT,NSTATE,LEN)
C----- CHECK QUALITY OF READ
      NSTATE=NSTATE+1
      GO TO(10, 20, 30, 40)NSTATE
C           OK EOF  PE EOT
C
C Good read
   10 CONTINUE
C----- THE INPUT TAPE HAS BEEN WRITTEN BY A 16 BIT MACHINE; THEREFORE,
C       THE ACTUAL NUMBER OF WORDS READ NEEDS TO BE MODIFIED TO
C       ACCOMMODATE THE 16-BIT SIZE.
      NWDS=64*LEN/16
C
C----- CALL THE HEADER ROUTINE WITH THE APPROPRIATE # WORDS
      CALL HEADR(NWDS)
C-----IF HEADER WAS NOT READ PROPERLY, TRY READING ANOTHER RECORD.
C      (HANDLES EXABYTE TAPES WHOSE HEADER IS THE SECOND RECORD)
      IF (NWDS.EQ.0) THEN
        IF (LRETRY) THEN
          LRETRY = .FALSE.
          GO TO 8
        ELSE
C----- Forget this tape and get another
          GO TO 5
        END IF
      END IF
C----- CALL THE DECODE ROUTINE
      CALL ADSUSD(NFREQ,NCHANL,NCH,JJ)
C----- CALL THE TAPE DUMP ROUTINE TO GENERATE THE DUMP
      CALL RWPRT(IUNIT)
      GO TO 400
C EOF sensed
   20 CONTINUE
      WRITE (6,1040) VOLUME(NUMVOL,IFLGHT)
      GO TO 400
C Parity error sensed
   30 WRITE (6,1000) VOLUME(NUMVOL,IFLGHT)
      GO TO 400
C EOT sensed
   40 CONTINUE
      WRITE (6,1050) VOLUME(NUMVOL,IFLGHT)
      GO TO 400
C Error closing IUNIT
  220 CONTINUE
      WRITE (6,2200) IERR, VOLUME(NUMVOL,IFLGHT)
      GO TO 400
C Error assigning new volume
  230 CONTINUE
      WRITE (6,2300) IERR, VOLUME(NUMVOL,IFLGHT), IUNIT
C     GO TO 400
  400 CONTINUE
C 500 CONTINUE
      GO TO 5
  600 CONTINUE
      WRITE (6,2600) IERR
 2600 FORMAT ("  Error "I4" opening COMPFLT file")
      STOP 1
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE HEADR(NWDS)
C
C     THIS SUBROUTINE WILL DECODE THE HEADER RECORD OF THE INPUT
C     TAPE AND GENERATE AN OUTPUT FILE TO BE USED AS PART OF A
C     GENPROII CALIB  UD.
C
C     HEADER INFORMATION IS DECODED USING THE FOLLOWING HEADER
C     CONVENTIONS:
C
C  ARIS5 HEADER INFORMATION   AS OF DEC 15, 1981
C
C  WORDS ARE 16 BITS, X'S REPRESENT BIT POSTIONS IN WORD
C           DASHES ARE INCLUDED FOR READABILITY
C
C  WORD     DESCRIPTION
C    1      NHEAD(1) = -1 (FLAG WORD)
C    2      NHEAD(2) = TAPE LABEL(NUMBER)
C    3      NHEAD(3) = FLT NO.
C    4      NHEAD(4) = XXX-YEAR00000000   YEAR        = IHEAD(3)
C    4      NHEAD(4) = 00000000XX-MONTH   MO          = IHEAD(1)
C    5      NHEAD(5) = XXXX-DAY00000000   DAY         = IHEAD(2)
C    5      NHEAD(5) = 00000000XXX-HOUR   HR          = IHEAD(4)
C    6      NHEAD(6) = X-MINUTE00000000   MIN         = IHEAD(5)
C    6      NHEAD(6) = 00000000X-SECOND   SEC         = IHEAD(6)
C   7&8     NHEAD(7) , NHEAD(8) = 4 8-BIT ASCII AIRCRAFT NAME
C  9&10     NHEAD(9) , NHEAD(10)--4 CHARACTER PROJECT NAME
C   11      NHEAD(11)= LENGTH OF FILE HEADER  = 1051
C   12      NHEAD(12)= NUMBER OF LOGICAL RECS PER PHYSICAL RECORD
C   13           (13)= LOGICAL RECORD LENGTH
C 14-20        (14-20)   NOT USED
C 21-36     BLKNAM(1-8)  4 CHAR DATA BLOCK NAMES
C 37-44     ISIZE(1-8)   LENGTH OF EACH BLOCK
C 45-52     IFIRST(1-8)  FIRST WORD NUMBER
C 53-60     ILAST(1-8)   LAST WORD NUMBER
C 61        MPHYSCL      MAXIMUM NUMBER OF PHYSICAL CHANNELS (100)
C 62-261    LOCATE(2,100)
C                   (1,N) PHYSICAL CHANNEL NUMBER
C                   (2,N) RELATIVE ADDRESS OF FIRST SAMPLE
C 262       MAXBAS        NUMBER OF BUFFERS (LENGTH OF IBUFBAS)
C 263-272   IBUFBAS(1-10) RELATIVE ADDRESSES OF EACH BUFFER START
C 273       MAXSDI        NUMBER OF SAMPLED CHANNELS POSSIBLE
C 274-336   ISDI(1-63)    SAMPLE RATE FOR EACH CHANNEL
C 337-588   SDINAM(1-63)  8 CHAR CHANNEL NAME
C 589       MAXANA        MAX NUMBER OF ANALOG CHANNELS
C 590-715   C1(1-63)      ZEROTH ORDER CALIBRATION COEFFICIENTS
C 716-841   C2(1-63)      FIRST ORDER CALIBRATION COEFFICIENTS
C 842-967   C3(1-63)      SECOND ORDER CALIBRATION COEFFICIENTS
C 968       NFULL         NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM
C 969-1031  ITYPE(1-63)   2 CHARACTER SPECIFICATION OF  EITHER
C                          ANALOG (AN) OR DIGITAL (DI)
C
C
C  ADS HEADER INFORMATION   AS OF NOV 11, 1983
C
C   WORD        DESCRIPTION
C      1-10  SAME AS ARIS5 HEADER OF DEC 15, 1981
C        11  LENGTH OF FILE HEADER = 1655
C     12-61  SAME AS ARIS5 HEADER OF DEC 15, 1981
C    62-301  LOCATE(2,120)
C               -  (1,N) PHYSICAL CHANNEL NUMBER
C               -  (2,N) RELATIVE ADDRESS OF FIRST SAMPLE
C       302  MAXBAS        NUMBER OF BUFFERS (LENGTH OF IBUFBAS)
C   303-312  IBUFBAS(1-10) RELATIVE ADDRESSES OF EACH BUFFER START
C       313  MAXSDI        NUMBER OF SAMPLED CHANNELS POSSIBLE
C   314-433  ISDI(1-120)   SAMPLE RATE FOR EACH CHANNEL
C   434-793  SDINAM(1-120) 8 CHAR CHANNEL NAME
C       794  MAXANA        MAX NUMBER OF ANALOG CHANNELS
C  795-1034  C1(1-120)     ZEROTH ORDER CAL COEFFICIENT
C 1035-1274  C2(1-120)     FIRST ORDER CAL COEFFICIENT
C 1275-1514  C3(1-120)     SECOND ORDER CAL COEFFICIENT
C      1515  NFULL         NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM
C 1516-1635 ITYPE(1-120)   2 CHARACTER SPECIFICATION OF EITHER
C                           ANALOG (AN) OR DIGITAL (DI)
C
C
C
C
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),
     3              NHDR(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT
      COMMON/FLTX/FLTTYP
      CHARACTER*2  FLTTYP
      DIMENSION NBIT8(720),NHEAD(3000),NCHAR(8),IBUF(10),MONTH(12)
      DATA MONTH/3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,3HJUL,3HAUG,3HSEP,
     1           3HOCT,3HNOV,3HDEC/
C
C  Common block for acquiring a new tape volume
      COMMON /GTVOL/ NUMTAP(500), NUMVOL,IFLGHT, IERR, IEXERR, LERR, LEOT
      INTEGER        NUMTAP,     NUMVOL,IFLGHT, IERR, IEXERR
      LOGICAL                                                 LERR, LEOT
      COMMON /GTVOL/ IFMT, STRING, VOLUME(500,20), CFLT(500)
      CHARACTER*80   IFMT, STRING
      CHARACTER*6                  VOLUME
      CHARACTER*4                                 CFLT
C  COMMON /GTVOL/ Variables 
C     NUMTAP -- Number of tape volumes available (per flight number)
C     NUMVOL -- Tape volume number to process
C     IFLGHT -- Flight number to process
C     IERR   -- Volume open error return code
C     IEXERR -- Unix command Error return code
C     LERR   -- Error occurred closing IUNIT, if .TRUE.
C     LEOT   -- No more tapes to process, if .TRUE.
C     IFMT   -- Format for system call to assign the unit number
C     STRING -- UNIX command sent to UNICOS
C     VOLUME -- Array of tape labels (flight number, number within flight)
C     CFLT   -- Array of flight titles
C
C
C
C   DESCRIPTION OF VARIABLES:
C     C1(120)  -- THE ZEROTH ORDER CALIBRATION COEFFICIENTS
C     C2(120)  -- THE FIRST ORDER CALIBRATION COEFFICIENTS
C     C3(120)  -- THE SECOND ORDER CALIBRATION COEFFICIENTS
C     IBUF(10) -- RELATIVE ADDRESSES OF EACH BUFFER START
C     IFIRST   -- FIRST WORD NUMBER
C     ILAST    -- LAST WORD NUMBER
C     ISDI     -- SAMPLE RATE FOR EACH CHANNEL
C     ISIZE    -- LENGTH OF EACH BLOCK
C     ITYPE    -- 2 CHAR SPEC OF ANALOG(AN) OR DIGITAL(DI)
C     KPROB    -- NUMBER OF PROBES  (PROBE COUNTER)
C     LOCATE(2,120)    (1,N)-- PHYSICAL CHANNEL NUMBERS
C                      (2,N)-- RELATIVE ADDRESS OF 1ST SAMPLE
C     MAXANA   -- MAX NUMBER OF ANALOG CHANNELS
C     MAXBAS   -- NUMBER OF BUFFERS (LENGTH OF IBUFBAS)
C     MAXSDI   -- NUMBER OF SAMPLED CHANNELS POSSIBLE
C     MXCHAN   -- MAXIMUM NUMBER OF PHYSICAL CHANNELS (100)
C     NFULL    -- NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM
C     SDINAM   -- 8 CHAR CHANNEL NAME (DOUBLE PREC)
C
C
C
C-----  Use flight number from COMPFLT file
      READ (CFLT(IFLGHT),'(A2,I2)') FLTTYP,NFLT
C
C
C     INITIALIZE NEW HEADER
C
C-----USE GBYTES TO UNPACK THE 16-BIT INFORMATION STORED 4 PER WORD
C     IN NBUF TO NHEAD (1 PER WORD). NWDS WAS SET BY THE MAIN PROGRAM.
      CALL GBYTES(NBUF,NHEAD,0 ,16,0,NWDS)
C-----DETERMINE NUMBER OF CHANNELS AND INITILIZE SOME CONSTANTS
      IF(NHEAD(273).EQ.63) THEN
        ISDISZ=252
        ILCTSZ=100
        NBTGB=8
      ELSE IF(NHEAD(313).EQ.120) THEN
        ISDISZ=360
          ILCTSZ=120
          NBTGB=6
      ELSE
        WRITE (6,1000)
 1000   FORMAT(2X,'AN ERROR OCCURRED DETECTING THE SDI BLOCK SIZE!')
        WRITE (6,'(2X"Flight "A", tape #"I2", volume "A)') CFLT(IFLGHT),
     &      NUMVOL, VOLUME(NUMVOL,IFLGHT)
C-----FLAG MAIN PROGRAM THAT A FAILURE OCCURRED.
        NWDS = 0
        RETURN
      END IF
C
C-----GET FLAG WORD = -1
  30  CONTINUE
C     LOC = 1
C     ISIGN = 100000B .AND.NHEAD(1)
C     IF(ISIGN.NE.0) NHEAD(1)=NHEAD(1)-200000B
C     NFLAG = NHEAD(LOC)
C
C-----GET TAPE NUMBER
C     LOC = 2
C     NTAPE = NHEAD(LOC) +9000
C
C-----GET FLIGHT NUMBER (done above)
C     LOC = 3
C     NFLT  = NHEAD(LOC)
C
C     TIME OF CREATION  OF THE TAPE
C     WORD 4     XXX-YEAR00000000     -- YEAR
C                00000000XX-MONTH     -- MONTH
C     WORD 5     XXXX-DAY00000000     -- DAY
C                00000000XXX-HOUR     -- HOUR
C     WORD 6     X-MINUTE00000000     -- MINUTE
C                00000000X-SECOND     -- SECOND
C
C     4 CHAR AIRCRAFT NAME
C     WORD 7     XX-CHAR100000000     -- CHAR 1
C                00000000XX-CHAR2     -- CHAR 2
C     WORD 8     XX-CHAR300000000     -- CHAR 3
C                00000000XX-CHAR4     -- CHAR 4
C
C     4 CHAR PROJECT NUMBER
C     WORD 9     XX-CHAR100000000     -- CHAR 1
C                00000000XX-CHAR2     -- CHAR 2
C     WORD 10    XX-CHAR300000000     -- CHAR 3
C                00000000XX-CHAR4     -- CHAR 4
C
   16 LOC = 4
      NBSKIP = 16*(LOC-1)
C
C-----EXTRACT DATE AND TIME FROM 16-BIT NBUF WORDS INTO 8-BITS PER WORD
C      NBIT8 AND THEN PRINT THEM
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,14)
      NYEAR = NBIT8(1)
      NMON  = NBIT8(2)
      NDAY  = NBIT8(3)
      NHR   = NBIT8(4)
      NMIN  = NBIT8(5)
      NSEC  = NBIT8(6)
C
C-----EXTRACT AIRCRAFT NAME  AND PRINT IT
      CALL SBYTES(NAIR,NBIT8(7),0,8,0,4)
C
C-----EXTRACT PROJECT NAME AND PRINT IT
      CALL SBYTES(NPRO,NBIT8(11),0,8,0,4)
C-----GET LENGTH OF FILE HEADER
      LENHD = NHEAD(11)
C
C-----GET NUMBER LOGICAL RECS PER PHYSICAL REC
      NLREC = NHEAD(12)
C
C-----GET LOGICAL RECORD LENGTH
      LENLOG= NHEAD(13)
C
C-----EXTRACT 4 CHARACTER DATA BLOCK NAMES AND PAD WITH SPACES
      LOC = 21
      NBSKIP = 16*(LOC-1)
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,32)
      IN=0
      DO 60 I=1,8
         DO 40 N=1,4
           IN=IN+1
  40     NCHAR(N)=NBIT8(IN)
C
C-----FILL REST OF NCHAR WITH SPACES
         DO 50 M=5,8
  50     NCHAR(M)=40B
         CALL SBYTES(NAMBLK(I),NCHAR,0,8,0,8)
  60  CONTINUE
C
C-----LENGTH OF EACH BLOCK
C-----FIRST WORD NUMBER(LOCATION/ADDRESS) OF EACH BLOCK
C-----LAST WORD NUMBER(LOCATION/ADDRESS) OF EACH BLOCK
      LOCS = 37
      LOCF = 45
      LOCL = 53
      DO 70 I=1,8
         ISIZE(I) = NHEAD(LOCS+I-1)
         IFIRST(I) = NHEAD(LOCF+I-1)
         ILAST(I) = NHEAD (LOCL+I-1)
  70  CONTINUE
C
C-----MAXIMUM NUMBER OF PHYSICAL CHANNELS
      MXCHAN = NHEAD (61)
C-----LOCATE(1,N) ADS OCTAL ADDRESS
C-----      (2,N) OFFSET INTO TAPE BUFFER OF FIRST SAMPLE
      LOC = 62
      K= 0
      KGO=ILCTSZ*2
      DO 80 I=1,KGO,2
         K= K+1
         LOCATE(1,K)= NHEAD(LOC+I-1)
         LOCATE(2,K) = NHEAD(LOC+I)
  80  CONTINUE
 1090 FORMAT(/,' LOCATE(1,K)  ',/,(15I8))
 1100 FORMAT(/,' LOCATE(2,K)  ',/,(15I8))
C
C-----GET MAXIMUM NUMBER OF BUFFERS
      LOC=LOC+KGO
      MAXBAS = NHEAD (LOC)
C
C-----GET OFFSET INTO TAPE BUFFER OF EACH LOGICAL RECORD
      LOC=LOC+1
      DO 90 I=1,10
         IBUF(I) = NHEAD (LOC+I-1)
  90  CONTINUE
C-----GET NUMBER OF SAMPLED CHANNELS POSSIBLE
      LOC=LOC+10
      MAXSDI = NHEAD (LOC)
C-----GET SAMPLE RATE FOR EACH CHANNEL
      LOC=LOC+1
      DO 100 I=1,MAXSDI
         ISDI(I) = NHEAD(LOC+I-1)
  100 CONTINUE
C
C-----GET 6 CHARACTER CHANNEL NAME
      LOC=LOC+MAXSDI
      NBSKIP = 16*(LOC-1)
      ITTS=ISDISZ*2
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,ITTS)
      IN=0
      DO 130 I=1,MAXSDI
         DO 110 N=1,NBTGB
            IN=IN+1
  110    NCHAR(N)=NBIT8(IN)
C
C-----NOW PAD REST IF NECESSARY WITH SPACES
         DO 120 M=NBTGB+1,8
  120    NCHAR(M)=40B
         CALL SBYTES(NAMSDI(I),NCHAR,0,8,0,8)
  130 CONTINUE
C
C-----THE MAXIMUM NUMBER OF ANALOG CHANNELS = 120
      LOC=LOC+ISDISZ
      MAXANA = NHEAD(LOC)
C-----GET THE ZEROTH ORDER CALIBRATION COEFFICIENT
      LOC=LOC+1
      ITTS=MAXSDI*2
      CALL SLSI(NHEAD(LOC),C1,ITTS)
C
C-----GET THE FIRST ORDER CALIBRATION COEFFICENT
      LOC=LOC+ITTS
      CALL SLSI(NHEAD(LOC),C2,ITTS)
C
C-----GET THE SECOND ORDER CALIBRATION COEFFICIENT
      LOC=LOC+ITTS
      CALL SLSI(NHEAD(LOC),C3,ITTS)
C
C-----READ THE NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM
      LOC=LOC+ITTS
      NFULL = NHEAD(LOC)
  135 FORMAT(I6)
C
C-----GET A 2 CHARACTER SPECIFICATION OF EITHER ANALOG(AN) OR DIGITAL(DI)
      LOC=LOC+1
      NBSKIP = 16*(LOC-1)
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,ITTS)
      IN=0
      DO 160 I=1,MAXSDI
         DO 140 N=1,2
            IN=IN+1
  140    NCHAR(N)=NBIT8(IN)
C
C-----NOW FILL REST OF NCHAR WITH SPACES
         DO 150 M=3,8
  150    NCHAR(M)=40B
         CALL SBYTES(ITYPE(I),NCHAR,0,8,0,8)
  160 CONTINUE
C
C-----GET FOUR CHAR PMS-1D PROBE NAME
      LOC=LOC+MAXSDI
      NBSKIP = 16*(LOC-1)
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,16)
      IN=0
      DO 190 I=1,4
         DO 170 N=1,4
            IN=IN+1
  170    NCHAR(N)=NBIT8(IN)
C
C-----AND PAD THE REST OF NCHAR WITH SPACES
         DO 180 M=5,8
  180    NCHAR(M)=40B
         CALL SBYTES(NAMPMS(I),NCHAR,0,8,0,8)
  190 CONTINUE
C
C-----GET BLOCK SIZE FOR INDIVIDUAL PROBES
      KPROB=0
      LOC=LOC+8
C
C-----LOOP OVER CURRENT MAXIMUM POSSIBLE PMS1 PROBES
      DO 200 I=1,4
         ISZPMS(I) = NHEAD(LOC+I-1)
         IF(ISZPMS(I) .GT. 0) KPROB=KPROB+1
  200 CONTINUE
C
C-----SAMPLE RATE FOR EACH INDIVIDUAL PROBE
      LOC=LOC+4
      DO 210 I=1,KPROB
  210 IRTPMS(I) = NHEAD(LOC+I-1)
C
C-----OFFSET INTO PMS BLOCK OF FIRST SAMPLE OF EACH INDIVIDUAL PROBE
      LOC=LOC+4
      DO 220 I=1,KPROB
  220 LOCPMS(I) = NHEAD(LOC+I-1)
C
C-----FIX UP PROBE NAMES FOR GENPRO
      DO 92, IJ = 1, KPROB
         IF (NAMPMS(IJ) .EQ. 8H260X    ) NAMPMS(IJ) = 8HX260
         IF (NAMPMS(IJ) .EQ. 8H200Y    ) NAMPMS(IJ) = 8HY200
         IF (NAMPMS(IJ) .EQ. 8H200X    ) NAMPMS(IJ) = 8HX200
   92 CONTINUE
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE ADSUSD(NFREQ,NCHANL,NCH,NUMSDI)
C
C     SUBROUTINE ADSUD WILL GENERATE A FILE THAT CAN BE INSERTED
C      INTO A GENNPRO-II INPUT UD TO READ IN THIS TAPE.
C
C
      COMMON/TAPTIM/SEGGO(3),SEGEND(3),PRTROW(10),SAVTIM,FORWRD
      LOGICAL FORWRD
      INTEGER SEGGO,SEGEND
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),
     1              LOCATE(2,120),NAMSDI(120),DUMBK1(120),DUMBK2(120),
     2              DUMBK3(120),NFULL,ITYPE(120),LENLOG,NLREC,
     2              KDUM2(2000),
     3              NHEAD(20),KDUM4,KDUM5,KDUM6(10),MAXSDI,KDUM7,
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)
C-----                  DME(1),LORN(91)
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)
C
      DIMENSION NAMHDR(11),NAMINS(11),NPLUS(11),NFREQ(1),NCHANL(1),
     1NMPMS2(4),SCALE(120),NAMHSK(50),NMCPIT(5),COM(6)
C
      CHARACTER COM
C
      DATA NMLORN/8HLRNC    /
      DATA NAMDME/8HDME     /
      DATA NAMHDR/8HIDWD    ,8HHR      ,8HMIN     ,8HSEC     ,
     1            8HSTEF    ,8HFTER    ,8HINST    ,8HRSWD    ,
     2            8HBECT    ,8HTECT    ,8HSYST    /
      DATA NAMINS/8HTSEC    ,8HTHNDS   ,8HALAT    ,8HALON    ,
     1            8HTHI     ,8HALPHA   ,8HXVI1    ,8HXVI2    ,
     2            8HYVI1    ,8HYVI2    ,8HGSI     /
      DATA NPLUS/0,16,34,930,162,130,66,194,98,226,706  /
      DATA NMPMS2/8HAUX1    ,8HAUX2    ,8HAUX3    ,8HAUX4    /
      DATA NAMHSK/8HV10     ,8HV10R    ,8HTADS    ,8HTV10    ,
     1     8HFLOADS  ,8HFZV     ,8HFZVR    ,8HVDREF   ,8HXIDICE  ,
     2     8HXI400U  ,8HXI60U   ,8HXI28U   ,8HXI400D  ,8HXI60D   ,
     3     8HXI28D   ,8HXILGEN  ,8HXIRGEN  ,8HXIEXT   ,8HVP15D   ,
     4     8HV28     ,8HVP15A   ,8HVM15A   ,8HTCBADS  ,8HFCBADS  ,
     5     8HSP1A    ,8HSP2A    ,8HSP3A    ,8HSP4A    ,8HSP5A    ,
     6     8HSP6A    ,8HCCKPIT  ,8HHCPY1   ,8HEV1     ,8HHPCY2   ,
     7     8HEV2     ,8HHCPY3   ,8HEV3     ,8HHCPY4   ,8HEV4     ,
     8     8HHCPY5   ,8HTA2D    ,8HTS2D    ,8HTLSI    ,8HSP1D    ,
     9     8HSP2D    ,8HSP3D    ,8HSP4D    ,8HSP5D    ,8HSP6D    ,
     A     8HSP7D    /
      DATA NMCPIT/8HCMFMC   ,8HDISW    ,8HSQSW    ,8HCAMESW  ,
     1            8HCKEVP1  /
      INTEGER FSTBIT,BITS,BTSKIP,RATES,DUMMY,CONKEY
      INTEGER TNAME(396)
 1000 FORMAT(/,' ORDVAR = FSTBIT,BITS,SKIP, SAMPLE,CONKEY,TERM,   FACTOR
     1 ')
 1010 FORMAT(' ORDGEN = LOGBIT,  DATLOG,  DATSIZ,  NAMKEY,  BITKEY')
 1020 FORMAT(' LETGEN = ',I6,',',3X,I4,',',3X,I6,',    IDWD ,  34433',/)
 1070 FORMAT(' VECVAR = ',6(1X,A8,','))
 1075 FORMAT(11X,'LORN    , CSTAT   ,')
 1080 FORMAT((10X,6(1X,A8,',')))
 1085 FORMAT((10X,6(1X,A8,A1)))
 1090 FORMAT(' LETVAR = ',I6,',',I4,',',I5,',',I4,',',I3,',   0.0,',
     1F13.7,', %FOR,',2X,A8)
C
C     VARIABLE DESCRIPTIONS USED IN ADSUD
C
C     BITS(215)   --NUMBER OF BITS FOR EACH VARIABLE
C     BTSKIP(215) --NUMBER OF BITS TO SKIP FOR EACH VARIABLE
C     CONKEY(215)  CONVERSION KEY FOR INPUT OPERATION UD
C           = 1  NO CONVERSION REQUIRED
C           = 2  CONVERT THE BIT STRING WITH THE MOST SIGNIFICANT BIT
C                AS A SIGN BIT ( 0 = +, 1 = -)
C           = 6  INVERT THE 1'S COMPLEMENT BIT STRING WITH .NOT..
C           = 7  INVERT THE 1'S COMPLEMENT BIT STRING WITH .NOT., THEN
C                CONVERT IT AS CONKEY=2.
C     DUMBK  --(1-3) UNUSED VARIABLES USED TO HOLD POSITION IN COMMON BLK
C     FACTOR(215) -- SCALING FACTOR FOR ALL VARIABLES
C     FSTBIT(215) -- FIRST BIT LOCATION FOR ALL VARIABLES
C     IFIRST(8)   -- FIRST WORD NUMBER
C     II,I,ITER,IJ,NO,NCH,IL -- INDEX'S FOR ARRAYS
C     INDEX       -- BEGINNING BIT LOCATION FOR INS & PMS BLOCK
C     ISDI(120)   -- SAMPLE RATE FOR EACH CHANNEL
C     ITYPE(120)  -- 2 CHAR SPEC OF ANALOG(AN) OR DIGITAL(DI)
C     KDUM  --(0-7) UNUSED VARIABLES USED TO HOLD POSITION IN COMMON BLK
C     LENLOG      -- LOGICAL RECORD LENGTH
C     LOCATE(2,120) -- (1,N) PHYSICAL CHANNEL NUMBER
C                      (2,N) RELATIVE ADDRESS OF FIRST SAMPLE
C     LOCIN        -- BEGINNING LOCATION IN BITS OF A BLOCK (I.E.SDI,INS)
C     MAXSDI       -- MAXIMUM NUMBER OF SAMPLED CHANNELS POSSIBLE
C     NAMBLK(8)    -- 4 CHARACTER DATA BLOCK NAMES
C     NAMES(215)   -- NAMES FOR ALL VARIABLES
C     NAMHDR(11)   -- 8H CHARACTER HEADER NAME
C     NAMINS(11)   -- 8H CHARACTER INS NAME
C     NAMSDI(120)  -- 8 CHAR CHANNEL NAME(DOUBLE PREC-4 4 CHAR NAME EACH)
C     NMPMS2(4)    -- 8H CHARACTER PMS2D NAME
C     NCHANL(20)   -- NUMBER OF CHANNELS AT A SAMPLING RATE
C     NFREQ(20)    -- SAMPLING RATE
C     NLREC        -- LOGICAL RECORDS PER PHYSICAL RECORD
C     NPLUS(11)    -- ADJUSTED STARTING BIT LOCATION FOR INS BLOCK
C     RATES(215)   -- SAMPLE RATES FOR ALL VARIABLES
C     SCALE(120)   -- SCALE FACTORS FOR SDI BLOCK
C
C------THE FOLLOWING SETS GAP INFO
      CONKEY(1) = 1
      FSTBIT(1) = 1
      BITS(1) = 1
      BTSKIP(1) = 0
      RATES(1) = 1
      FACTOR(1) = 1.0
      NAMES(1) = 8HGAP
C
C-----THE FOLLOWING PRINTS OUT ALL PERTINANT HEADER INFORMATION.
      DO 10 II=2,12
         CONKEY(II)=1
         FSTBIT(II)=((II-2)*16)+1
         BITS(II)=16
         BTSKIP(II)=0
         RATES(II)=1
         FACTOR(II)=1.0
10       NAMES(II)=NAMHDR(II-1)
      II=II-1
C
C-----SET SDI SCALING
      DO 20 I=1,MAXSDI
C
C-----CHECK IF DATA IS DIGITAL OR ANALOG, THEN SET SCALE ACCORDINGLY.
         IF(ITYPE(I).EQ.8HAN      ) SCALE(I)=0.001223                   ANALOG
  20     IF(ITYPE(I).EQ.2HDI      ) SCALE(I)=0.0219726                  DIGITAL
C-----THE FOLLOWING CALCULATES OUT ALL PERTINANT INS UD DATA.
      DO 260 ITER=2,8
         IF(NAMBLK(ITER).NE.8HINS     ) GOTO 50
         LOCIN=IFIRST(ITER)
         DO 40 IJ = 1,11
            II=II+1
            FACTOR(II)=1.0
            BITS(II)=16
            BTSKIP(II)=0
            RATES(II)=1
            FSTBIT(II)=LOCIN*16-15+NPLUS(IJ)
            IF(IJ .GT. 4) THEN
               BTSKIP(II)=206
               RATES(II)=5
            ENDIF
            CONKEY(II)=1
            IF(IJ .GT. 2) THEN
               CONKEY(II)=2
               BITS(II)=18
C
C-----CONVERSION FACTOR FOR XVI1,XVI2,YVI1,YVI2
               FACTOR(II)=26.2567192
               IF( IJ.LE.6) FACTOR(II)=728.1777861
               IF(IJ .EQ. 11) THEN
                  BTSKIP(II)=0
                  RATES(II)=1
                  FACTOR(II)=1.9425019
               ENDIF
            ENDIF
            NAMES(II)=NAMINS(IJ)
  40     CONTINUE
         GO TO 260
C
C-----THIS SECTION CALCULATES SDI INFORMATION
  50     IF(NAMBLK(ITER).NE.8HSDI     ) GOTO 90
         DO 80 I=1,NUMSDI
            II=II+1
            NAMES(II)=NAMSDI(I)
            RATES(II)=ISDI(I)
            IF(ISDI(I).GE.1)THEN
               CONKEY(II)=2
               BITS(II)=16
               FACTOR(II)=1./SCALE(I)
               FSTBIT(II)=LOCATE(2,I)*16-15
            IF(ISDI(I).GT.1) THEN
               DO 60 NO = 1,NCH
                IF(NFREQ(NO).EQ.ISDI(I))BTSKIP(II)=NCHANL(NO)*16-16
  60           CONTINUE
            ELSE
               BTSKIP(II)=0
            END IF
            IF((NAMSDI(I).EQ.8HPITCH   ).OR.(NAMSDI(I).EQ.8HROLL    ).OR
     1      .(NAMSDI(I).EQ.8HPHDG    )) THEN
               FACTOR(II)=FACTOR(II)*8.
               CONKEY(II)=7
            END IF
            IF((NAMSDI(I).EQ.8HCROLL   ).OR.(NAMSDI(I).EQ.8HCHGME   ).OR
     1      .(NAMSDI(I).EQ.8HHGME    )) CONKEY(II)=7
            IF(NAMSDI(I).EQ.8HHGME    ) FACTOR(II)=FACTOR(II)*25.
            IF(NAMSDI(I) .EQ.8HPSFD    ) THEN
               FACTOR(II)=1./0.033864
               CONKEY(II)=7
C              CONKEY(II)=6
            END IF
            IF(NAMSDI(I) .EQ.8HVZI     ) FACTOR(II)=82.020997
         ENDIF
  80     CONTINUE
         GO TO 260
C
C----------------------HOUSEKEEPING BLOCK------------------------
C
  90     IF(NAMBLK(ITER).NE.8HHSKP    ) GOTO 120
         INDEX=IFIRST(ITER)
         FSTBIT(II+1)=INDEX*16-15
         DO 110 IL=1,50
            II=II+1
            BTSKIP(II)=0
            RATES(II)=1
            NAMES(II)=NAMHSK(IL)
C-----HOUSEKEEPING BLOCK HAS THE SAME ANALOG CONVERSION FACTOR AS SDI
            FACTOR(II)=1.0
            CONKEY(II)=2
            BITS(II)=16
            IF(IL.LE.30) FACTOR(II)=1./0.001223
            IF(IL.GE.3 .AND. IL.LE.5) FACTOR(II)=FACTOR(II)/5.
            IF(IL.EQ.23 .OR. IL.EQ.24) FACTOR(II)=FACTOR(II)/5.
            IF(IL.EQ.19 .OR. IL.EQ.21 .OR. IL.EQ.22) FACTOR(II)=
     1      FACTOR(II)/3
            IF(IL.EQ.20) FACTOR(II)=FACTOR(II)/6.2
C-----EV1  CORRECTION
C           IF(IL.EQ.33) FSTBIT(II)=FSTBIT(II)+8
C           IF(IL.EQ.33) BITS(II)=8
C           IF(IL.EQ.34) FSTBIT(II)=FSTBIT(II)-8
C
C** HOUSEKEEPING BLOCK   ---------- WORD 31 CCKPIT
C        /   /   /   /   /   /   /   /   /   /   /   /   /   /   /   /
C BIT-- 15  14  13  12  11  10  09  08  07  06  05  04  03  02  01  00
C ORDER  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
C                    *   *   *               *  **********************
C                         COCKPIT EVENT (BITS 0-5)
C                         CAMERA SWITCH (BITS 6)
C                         SQUAT SWITCH  (BIT 10)
C                         DEICE SWITCH  (BIT 11)
C                         CAMERA INTERVAL (BIT 12)
C
            IF( IL.EQ. 31) THEN
               CONKEY(II)=1
               FACTOR(II)=1.0
C-----TO PRINT THE BIT LOCATION FOR THE WHOLE WORD
C-----TO PRINT OUT BIT 12, 11, 10, 6, AND BITS 0-5
               DO 100 I=1,5
                 II=II+1
                 BITS(II)=1
                 IF(I.EQ.5) BITS(II)=6
                 IF(I.EQ.1) FSTBIT(II)=FSTBIT(II-1) +3
                 IF(I.EQ.2.OR.I.EQ.3.OR.I.EQ.5) FSTBIT(II)=FSTBIT(II-1)+
     1           1
                 IF(I.EQ.4) FSTBIT(II)=FSTBIT(II-1)+4
                 BTSKIP(II)=0
                 RATES(II)=1
                 CONKEY(II)=1
                 FACTOR(II)=1.0
                 NAMES(II)=NMCPIT(I)
 100           CONTINUE
               FSTBIT(II+1)=FSTBIT(II)+6
               GOTO 110
            END IF
            IF(IL.NE.31) THEN
            END IF
            FSTBIT(II+1)=FSTBIT(II)+16
 110     CONTINUE
         GO TO 260
C
C-----PMS BLOCK       (PMS1)
C     PICKING UP THE WHOLE BLOCK AS ONE PARAMETER NAMED PMSBLOCK
C
 120     IF(NAMBLK(ITER).NE.8HPMS1    ) GOTO 140
         INDEX=IFIRST(ITER)
         DO 130, IP=1,KPROB
            IF (IP .GT. 1) INDEX = INDEX + ISZPMS(IP-1)
            II=II+1
            CONKEY(II)=2
            BITS(II)=16
            FSTBIT(II)=INDEX*16-15
            FACTOR(II)=1.0
            BTSKIP(II)=0
            RATES(II)=ISZPMS(IP)
            NAMES(II)=NAMPMS(IP)
 130     CONTINUE
         GO TO 260
C
C-----PMS BLOCK       (PMS2D AUXILARY WORDS)
C
 140     IF(NAMBLK(ITER).NE.8HPMS2    ) GOTO 160
         INDEX=IFIRST(ITER)
         FSTBIT(II+1)=INDEX*16-15
         DO 150 IL=1,4
            II=II+1
            CONKEY(II)=2
            BITS(II)=16
            FACTOR(II)=1.0
            BTSKIP(II)=0
            RATES(II)=1
            NAMES(II)=NMPMS2(IL)
            FSTBIT(II+1)=FSTBIT(II)+16
 150     CONTINUE
C
C-----DME BLOCK
C
 160     IF(NAMBLK(ITER).NE.8HDME     ) GOTO 180
         INDEX=IFIRST(ITER)
         FSTBIT(II+1)=INDEX*16-15
         II=II+1
         CONKEY(II)=0
         BITS(II)=16
         FACTOR(II)=1.0
         BTSKIP(II)=0
         RATES(II)=3
         NAMES(II)=NAMDME
C
C-----LORAN-C BLOCK
C     1) CSTAT  12+XL      4    1      0
C     2) CSEC   16+XL     16    1      1
C     3) CFSEC  32+XL     16    1      1
C     4) LORN   48+XL      8   64      0
C----- WHERE XL IS THE FIRST BIT LOCATION OF LORAN-C BLOCK
C
 180     IF(NAMBLK(ITER).NE.NMLORN    ) GOTO 260
         INDEX=IFIRST(ITER)
         FSTBIT(II+1)=INDEX*16-15
         II=II+1
         CONKEY(II)=0
         BITS(II)=16
         FACTOR(II)=1.0
         BTSKIP(II)=0
C        RATES(II)= 91
         NAMES(II)=NAMBLK(ITER)
C
C---- 1) CSTAT  12+XL      4    1      0
         II=II+1
         FSTBIT(II)=(FSTBIT(II-1)+12)
         CONKEY(II)=0
         BITS(II)=4
         FACTOR(II)=1.0
         BTSKIP(II)=0
         RATES(II)= 1
         NAMES(II)= 8HCSTAT
C---- 2) CSEC   16+XL     16    1      1
         II=II+1
         FSTBIT(II)=(FSTBIT(II-1)+4 )
         CONKEY(II)=1
         BITS(II)= 16
         FACTOR(II)=1.0
         BTSKIP(II)=0
         RATES(II)= 1
         NAMES(II)= 8HCSEC
C---- 3) CFSEC  32+XL     16    1      1
         II=II+1
         FSTBIT(II)=(FSTBIT(II-1)+16)
         CONKEY(II)=1
         BITS(II)= 16
         FACTOR(II)=1.0
         BTSKIP(II)=0
         RATES(II)= 1
         NAMES(II)= 8HCFSEC
C---- 4) LORN   48+XL      8   64      0
         II=II+1
         FSTBIT(II)=(FSTBIT(II-1)+16)
         CONKEY(II)=0
         BITS(II)= 8
         FACTOR(II)=1.0
         BTSKIP(II)=0
         RATES(II)= 64
         NAMES(II)= 8HLORN
 260  CONTINUE
C
       IJJ = 0
       DO 101, JJI = 1, II
         IF (NAMES(JJI) .NE. 8HLRNC    ) THEN
            IJJ = IJJ + 1
            TNAME(IJJ) = NAMES(JJI)
         ENDIF
  101  CONTINUE
C
      DO 11,KJ = 1,6
          COM(KJ) = ','
   11 CONTINUE
      JK = INT(FLOAT(IJJ)/6.0)
      JK = JK * 6
      IF (ABS(IJJ-JK) .LE. .00001) JK = IJJ - 6
      COM(IJJ-JK) = ' '
      LGBT=LENLOG*16
      IDTSZ=LGBT*NLREC
      KR=II
      RETURN
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE SLSI(NPAC,A,NUM)
C
C     SUBROUTINE LSI CONVERTS 32 BIT FLOATING POINT NUMBERS ENCODED
C     BY THE HP GROUND STATION INTO WORDS SUITABLE FOR THE CRAY
C----------------- ** AS OF AUGUST 26,1985 (CC) ** --------------------
C
C**     H P - 1 0 0 0    R E A L   F O R M A T  ( 32 BITS )
C
C----------
C BIT N0.    15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
C WORD 1      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
C
C   WHERE  BIT    15  IS SIGN OF FRACTION (0-POSITIVE;1-NEGATIVE)
C          BITS 0-14 ARE FRACTION BITS
C NOTE: 1'S COMPLIMENT FRACTION FOR NEGATIVE SIGN OF FRACTION
C----------
C BIT N0.    15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
C WORD 2      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
C
C   WHERE  BIT   00   IS SIGN OF EXPONENT (0-POSITIVE;1-NEGATIVE)
C          BITS 1-7  ARE EXPONENT BITS
C          BITS 8-15 ARE FRACTION BITS
C NOTE: 2'S COMPLIMENT EXPONENT FOR NEGATIVE SIGN OF EXPONENT
C----------------------------------------------------------------------
C
      DIMENSION NPAC(1),A(1)
      I=0
      DO 100 N=1,NUM,2
         I= I +1
C** PICK SIGN OF FRACTION BIT
         ISIGN = 100000B .AND.NPAC(N)
         IWD1 = NPAC(N)
         IWD2 = NPAC(N+1)
C** SAVE ORIGINAL WORD 2 FOR DECODING EXPONENT WORD
         IWD2S=IWD2
         IF(ISIGN.NE.0) THEN
C** NEGATIVE SIGN FRACTION -- 1'S COMPLIMENT
            IWD1 = .NOT. IWD1
            IWD2 = .NOT. IWD2
         END IF
         NFRAC1 = IWD1 .AND. 77777B
         NFRAC2 = NFRAC1*256
         NFRAC3 = IWD2 .AND.177400B
         NFRAC3 = NFRAC3/256
         NFRAC = NFRAC2 .OR. NFRAC3
C** PICK UP SIGN OF EXPONENT BIT FROM THE SAVED WORD 2
         NXSIGN = IWD2S .AND. 1B
         IF(NXSIGN.EQ.0) THEN
C** POSITIVE SIGN OF EXPONENT
            NEXP = IWD2S .AND. 376B
            NEXP = NEXP/2
         END IF
         IF(NXSIGN.NE.0 ) THEN
C** NEGATIVE SIGN OF EXPONENT -- 2'S COMPLIMENT
            IWD2 = .NOT. IWD2S
            NEXP = IWD2 .AND. 376B
            NEXP =(NEXP/2) +1
            NEXP = -NEXP
         END IF
         FRAC = NFRAC* 1.192092896E-7
         A(I) = 2.**NEXP *(FRAC)
         IF(ISIGN.NE.0) A(I)=-A(I)
  100 CONTINUE
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE RWPRT(IUNIT)
C
C     SUBROUTINE RWDPRT GENERATES A PRINTOUT OF THE RAW DATA TAPE
C      ACCORDING TO THE SPECIFICATIONS SET BY THE USER AT THE BOTTOM
C      OF THIS DECK.
C
C
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT
      COMMON/FLTX/FLTTYP
      CHARACTER*2  FLTTYP
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)
C-----                  DME(1),LORN(91)
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),
     1              BEGREC(60),FIRST
C    1              BEGREC(60)
      COMMON/TAPTIM/SEGGO(3),SEGEND(3),PRTROW(10),SAVTIM,FORWRD
      LOGICAL FIRST
      LOGICAL FORWRD
      INTEGER SEGGO,SEGEND
C
C  Common block for acquiring a new tape volume
      COMMON /GTVOL/ NUMTAP(500), NUMVOL,IFLGHT, IERR, IEXERR, LERR, LEOT
      INTEGER        NUMTAP,     NUMVOL,IFLGHT, IERR, IEXERR
      LOGICAL                                                 LERR, LEOT
      COMMON /GTVOL/ IFMT, STRING, VOLUME(500,20), CFLT(500)
      CHARACTER*80   IFMT, STRING
      CHARACTER*6                  VOLUME
      CHARACTER*4                                 CFLT
C  COMMON /GTVOL/ Variables 
C     NUMTAP -- Number of tape volumes available (per flight number)
C     NUMVOL -- Tape volume number to process
C     IFLGHT -- Flight number to process
C     IERR   -- Volume open error return code
C     IEXERR -- Unix command Error return code
C     LERR   -- Error occurred closing IUNIT, if .TRUE.
C     LEOT   -- No more tapes to process, if .TRUE.
C     IFMT   -- Format for system call to assign the unit number
C     STRING -- UNIX command sent to UNICOS
C     VOLUME -- Array of tape labels (flight number, number within flight)
C     CFLT   -- Array of flight titles
C
C
      DIMENSION NAMDMP(200),IDATA(250),ITIME(5)
      INTEGER DUMMY,NAMDMP,SPREC,STREC,TIMEST,TIMESP,FSTBIT,BTSKIP,
     1        BITS,RATES,TIME,CONKEY,BEGREC
      REAL MINUTE
      DATA NMLORN/8HLRNC    /
C
C-----BECAUSE THE LORAN-C DATA ARE ENCODED AS 8 BIT ASCII WHILE THE
C     REST OF THE DATA ARE IN PACKED INTEGER, IT CANNOT BE STORED
C     IN THE MINUTE ARRAY AS A FLOATING POINT NUMBER. INSTEAD, A
C     SEPARATE INTEGER ARRAY IS SET UP AND EQUIVALENCED TO THE
C     MINUTE ARRAY SO THAT THE INFORMATION IS STORED IN ITS
C     ORIGINAL FORM.
      INTEGER IMINUTE(60,5000)
      EQUIVALENCE (MINUTE, IMINUTE )
C
      LOGICAL INULL
C
 1000 FORMAT(' EOF SENSED ON DATA FILE REC. NO. ',I8)
 1010 FORMAT(' EOT SENSED ON DATA FILE REC. NO. ',I8)
 1020 FORMAT(' PARITY ERROR SENSED ON DATA FILE, REC. NO.',I8)
 1030 FORMAT(' NO MATCH WAS FOUND FOR RAWDUMP INPUT FILE VARIABLES')
 1035 FORMAT(A2,'0',I1,2X,3(I2),1X,3(I2))
 1036 FORMAT(A2,I2,2X,3(I2),1X,3(I2))
 1040 FORMAT(1H1)
 1050 FORMAT(10A8)
 1070 FORMAT(8I5)
 1080 FORMAT(8(1X,A8))
 1090 FORMAT(1X,' CONKEY=',I2,' IS NOT DEFINED IN CODE!')
C
C     VARIABLE DESCRIPTIONS USED IN SUBROUTINE TAPE DUMP PRINT OUT
C
C     IDATA       -- ARRAY WHICH HOLD GBYTES NBUF ARRAY
C     IHRATE      -- LARGEST RATE IN CURRENT PRINTING SEQUENCE
C     IPAGE       -- PAGE OF FORMATTED DUMP DONE
C     IPFLAG      -- PRINT FLAG TO PRINT HEADER ON TOP OF EACH PAGE
C     ITRNPG      -- COUNTER; PRINT 60 LINES PER PAGE
C     KR          -- NUMBER OF PARAMETERS
C     LOCPOS(200) -- HOLDS POSITION OF PARAMETER IN MINUTE
C     LOGBEG -- LOGICAL DATA RECORD TO BE PROCESSED IN PHYSICAL DATA REC
C     MINUTE(60,10000) -- HOLDS ONE MINUTE OF DATA
C     NDREC       -- NUMBER OF RECORD CURRENTLY BEING PROCESSED
C     NLREC       -- NUMBER OF LOGICAL RECORDS PER PHYSICAL RECORD
C     NUMSEC      -- INDEX USED IN MINUTE TO INDICATE SECONDS
C     TIMESP      -- STOP TIME IN SECONDS
C     TIMEST      -- START TIME IN SECONDS
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
C----- IHOWTO=0: DUMP BEFORE SCALING  1: AFTER SCALING
      IHOWTO=1
C----- IDMPBY=0: DUMP BY RECORD #   IDMPBY=1: DUMP BY TIME
      IDMPBY=0
C-----START AND STOP RECORD
      STREC=1
      SPREC=99999
C-----PRINT OUT BY COLUMNS (0) OR HORIZONTALLY(1)
      KPRT=0
C-----NUMBER, AND VARIABLES TO BE DUMPED
      INUM=3
      NAMDMP(1)='HR'
      NAMDMP(2)='MIN'
      NAMDMP(3)='SEC'
      TIMEST=0
      TIMESP=0
C-----INITIALIZE SOME VARIABLES
      IPOS=0
      IPAGE=0
      IPFLAG=1
      NDREC=0
C
  10  NUMSEC=0
C
  20  NDREC=NDREC+1
C
C-----READ IN A PHYSICAL RECORD OF DATA FROM RAW TAPE
      CALL RDTAPE(IUNIT,1,2,NBUF,2000)
      CALL IOWAIT(IUNIT,NSTATE,LEN)
      NSTATE=NSTATE+1
      IF (NSTATE.EQ.2) THEN
C----- Informational print
C        WRITE (6,30050)
C30050 FORMAT (3X"EOF encountered --> 210")
        WRITE (6,1000) NDREC
        WRITE (6,'(2X"Flight "A", tape #"I2", volume "A)') CFLT(IFLGHT),
     &      NUMVOL, VOLUME(NUMVOL,IFLGHT)
      END IF
      GO TO (60,210,50,40)NSTATE
C            OK EOF PE EOT
  40  CONTINUE
C----- Informational print
C      WRITE (6,10050)
C10050 FORMAT (3X"RWPRT:  First output--EOT")
      IF (NFLT.LT.10) THEN
       WRITE (4,1035) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C       WRITE (6,1035) FLTTYP,NFLT,SEGGO,SEGEND
      ELSE
       WRITE (4,1036) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C       WRITE (6,1036) FLTTYP,NFLT,SEGGO,SEGEND
      ENDIF
      RETURN
C----- I don't know what to do with a parity error, so I'll just quit
  50  WRITE (6,1020) NDREC
      WRITE (6,'(2X"Flight "A", tape #"I2", volume "A)') CFLT(IFLGHT),
     &      NUMVOL, VOLUME(NUMVOL,IFLGHT)
      RETURN
C
C-----GOOD READ
C-----FILL UP MINUTE ARRAY WITH ONE MINUTE OF DATA
  60  CONTINUE
      IF(NDREC.GT.SPREC) THEN
      IF(NUMSEC.GE.1) GOTO 210
C----- Informational print
C       IF(NUMSEC.GE.1) THEN
C          WRITE (6,30100)
C30100     FORMAT (3X" NDREC.GT.SPREC and NUMSEC.GE.1 --> 210")
C          GOTO 210
C       END IF
       RETURN
      ENDIF
      IF(NDREC.LT.STREC) GOTO 10
      LOGBEG=1
C-----LOOP OVER NUMBER OF LOGICAL RECORDS WITHIN EACH PHYSICAL RECORD
  70  DO 200 KL=LOGBEG,NLREC
         NUMSEC=NUMSEC+1
         BEGREC(NUMSEC)=NDREC
         JJ=0
         IOFFS=(KL-1)*LENLOG*16
         CALL GBYTES(NBUF,ITIME,IOFFS,BITS(2),0,4)
         TIME(NUMSEC,1)=ITIME(2)
         TIME(NUMSEC,2)=ITIME(3)
         TIME(NUMSEC,3)=ITIME(4)
C
C-----FIND STARTING TIME IF NECESSARY
         IF(ITIME(1).NE.103201B) THEN
             NUMSEC=NUMSEC-1
             GOTO 20
         ENDIF
         ITIME(5)=ITIME(2)*3600+ITIME(3)*60+ITIME(4)
C-----START RAW DUMP HERE
  80     IF(IPOS.GT.0) GOTO 110
C
C-----FIND THE LOCATION OF PARAMETERS TO BE DUMPED
         DO 100 J=1,KR
            DO 90 I=1,INUM
               IF(NAMES(J).NE.NAMDMP(I)) GOTO 90
               IPOS=IPOS+1
               LOCPOS(IPOS)=J
C
C-----FIND STARTING LOCATIONS FOR PARAMETERS IN THE MINUTE ARRAY
               IF(IPOS.EQ.1) THEN
                   LOCMIN(IPOS)=1
               ELSE
                   LOCMIN(IPOS)=LOCMIN(IPOS-1)+RATES(LOCPOS(IPOS-1))
               ENDIF
  90        CONTINUE
 100     CONTINUE
C
C-----CHECK FOR NULL MATCH ON INPUT FILE
         IF(IPOS.EQ.0)  THEN
            PRINT 1040
            PRINT 1030
            RETURN
         ENDIF
C
C-----NOW FILL UP THE MINUTE ARRAY
 110     DO 190 I=1,IPOS
            IK=LOCPOS(I)
            IOFFS=((KL-1)*LENLOG*16)+FSTBIT(IK)-1
            CALL GBYTES(NBUF,IDATA,IOFFS,BITS(IK),BTSKIP(IK),RATES(IK))
            DO 190 J=1,RATES(IK)
               JJ=JJ+1
C
C-----MAKE CONVERSIONS OF THE DATA AS SPECIFIED BY CONKEY
C     SEE EXPLANATION OF CONKEY CONVERSIONS IN ADSUD
C
               GO TO(155,180,130,120,120,120,150,150) (CONKEY(IK) + 1)
C
 155           IF (NAMES(IK) .EQ. 'DME     ') THEN
C              CALL SKEY8 (IDATA(1),IDATA(2),IDATA(3),MINUTE(NUMSEC,JJ),
C    $         MINUTE(NUMSEC,JJ+1),MINUTE(NUMSEC,JJ+2))
                  JJ=JJ+2
                  GOTO 190
               ELSE
                  GOTO 180
               ENDIF
C
 120           RETURN
C
C-----CONKEY=2
 130           MASK=2**(BITS(IK))-1
               IDATA(J)=IDATA(J).AND.MASK
               MAX1=2**(BITS(IK)-1)-1
               IF(IDATA(J).GT.MAX1) IDATA(J)=IDATA(J)-2**(BITS(IK))
               GOTO 180
C
C-----CONKEY=6 & 7
 150           IDATA(J)=.NOT.IDATA(J)
               IF(CONKEY(IK).EQ.6) GOTO 180
               MASK=2**(BITS(IK))-1
               IDATA(J)=IDATA(J).AND.MASK
               MAX1=2**(BITS(IK)-1)-1
               IF(IDATA(J).GT.MAX1) IDATA(J)=IDATA(J)-2**(BITS(IK))
 180           MINUTE(NUMSEC,JJ)=FLOAT(IDATA(J))/FACTOR(IK)
C              IF(IHOWTO.EQ.0) MINUTE(NUMSEC,JJ)=IDATA(J)
               IF(NAMES(LOCPOS(I)).EQ.8HNMLORN  ) IMINUTE(NUMSEC,JJ)  =
     1         IDATA(J)
               IF(NAMES(LOCPOS(I)).EQ.'LORN    ') IMINUTE(NUMSEC,JJ)  =
     1         IDATA(J)
 190     CONTINUE
         IF(NUMSEC.EQ.60) THEN
            LOGBEG=KL+1
            IF(LOGBEG.GT.NLREC) LOGBEG=1
C----- Informational print
C            WRITE (6,30200)
C30200       FORMAT (3X,"NUMSEC.EQ.60 --> 210")
            GOTO 210
         ENDIF
 200  CONTINUE
      GOTO 20
C
C-----NOW PRINT OUT THE DATA
 210  CONTINUE
      CALL KPRNT(NUMSEC,ITRNPG,IPFLAG,IPOS)
      IF(NDREC.GT.SPREC) RETURN
C     IF(NSTATE.EQ.2) GOTO 300
C-----  Changes resulting from UNICOS implementation
      IF(NSTATE.EQ.2) THEN
        IF (NUMVOL.LT.NUMTAP(IFLGHT)) THEN
          NUMVOL = NUMVOL + 1
C-----  Informational print
C       WRITE (6,10200) NUMVOL, CFLT(IFLGHT), VOLUME(NUMVOL,IFLGHT)
C10200  FORMAT (3X"RWPRT:  Assigning tape # "I2" for flight "A", tape "A)
          CALL GETVOL (IUNIT)
          READ (CFLT(IFLGHT),'(A2,I2)') FLTTYP,NFLT
C-----  Check for error acquiring tape volume
          IF (LEOT) GO TO 300
          IF (LERR) THEN
            IF (IERR.NE.0) THEN
              WRITE (6,2200) IERR, VOLUME(NUMVOL,IFLGHT)
 2200         FORMAT (1X' ERROR 'I3' CLOSING TAPE VOLUME 'A)
            ELSE IF (IEXERR.NE.0) THEN
              WRITE (6,2300) IERR, VOLUME(NUMVOL,IFLGHT), IUNIT
 2300         FORMAT (1X' ERROR 'I3' ASSIGNING TAPE 'A' TO UNIT'I2)
            END IF
          END IF
        ELSE
          GO TO 300
        END IF
      END IF
      NUMSEC=0
      IF(LOGBEG.GT.1) GOTO 70
      GOTO 10
C     RETURN
 300  CONTINUE
C----- Informational print
C      WRITE (6,10051)
C10051 FORMAT (3X"RWPRT:  Second output")
      IF(NFLT.LT.10) THEN
       WRITE (4,1035) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C       WRITE (6,1035) FLTTYP,NFLT,SEGGO,SEGEND
      ELSE
       WRITE (4,1036) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C       WRITE (6,1036) FLTTYP,NFLT,SEGGO,SEGEND
      ENDIF
      FIRST = .TRUE.
      RETURN
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE KPRNT(NUMSEC,ITRNPG,IPFLAG,IPOS)
C
C     SUBROUTINE KKPRINT PRINTS OUT THE RAW TAPE DATA IN COLUMN FORMAT
C
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),
     2              ITYPE(120),LENLOG,NLREC,NBUF(2000),
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT
      COMMON/FLTX/FLTTYP
      CHARACTER*2  FLTTYP
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)
C-----                  DME(1),LORN(91)
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),
     1              BEGREC(60),FIRST
C    1              BEGREC(60)
      DIMENSION LOCBYRT(200),ENDSEG(3)
      INTEGER DUMMY,FSTBIT,BTSKIP,BITS,RATES,TIME,CONKEY,BEGREC
      COMMON/TAPTIM/SEGGO(3),SEGEND(3),PRTROW(10),SAVTIM,FORWRD
      LOGICAL FIRST
      LOGICAL FORWRD
      INTEGER SEGGO,SEGEND
      REAL MINUTE,SAVTIM,CURTIM
      DATA NMLORN/8HLRNC    /FIRST/.TRUE./
 1000 FORMAT(1H1)
 1010 FORMAT(23X,A4,'-',I3,10X,I2,'/',I2,'/',I4,10X,'BEGINNING RECORD ='
     1,I5,31X,'PAGE',2X,I5)
 1020 FORMAT('  HR MI SEC       ',10(3X,A8))
 1030 FORMAT(2X,I2,1X,I2,1X,F6.3,2X,10(1X,F10.3))
 1035 FORMAT(A2,'0',I1,2X,3(I2),1X,3(I2))
 1036 FORMAT(A2,I2,2X,3(I2),1X,3(I2))
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
      DO 34, KJI=1 , IPOS
         LOCBYRT(KJI) = LOCPOS(KJI)
   34 CONTINUE
C
      IF (IPOS .GE. 2) THEN
       DO 36, KIJ = 1,IPOS
         DO 35, KJI =2,IPOS
            IF (RATES(LOCBYRT(KJI)) .LT. RATES(LOCBYRT(KJI-1))) THEN
               LTEMP = LOCBYRT(KJI)
               LOCBYRT(KJI) = LOCBYRT(KJI-1)
               LOCBYRT(KJI-1) = LTEMP
            ENDIF
   35    CONTINUE
   36  CONTINUE
      ENDIF
C
      DO 50 I=1,IPOS,10
         II=I+9
         IF(II.GT.IPOS) II=IPOS
C
C-----FIND THE LARGEST RATE IN THIS PRINTING SEQUENCE
         IHRATE=RATES(LOCBYRT(II))
C
C-----LOOP OVER 60 SECONDS OR ENDING TIME SEGMENT
            DO 40 J=1,NUMSEC
C
C-----PRINT PAGE HEADER
               DO 30 JJ=1,IHRATE
                  SECD=TIME(J,3)+FLOAT(JJ-1)/FLOAT(IHRATE)
C
C-----SET UP ROW OF DATA TO BE PRINTED
                  KK=0
                  DO 20 K=I,II
                  KK=KK+1
C
C-----SET PRTROW TO OVERFLOW PRINT BUFFER IF PARAM IS NONEXISTANT
C-----DURING THIS TIME INCREMENT.
                  PRTROW(KK)=99999999.9999
                  TIMECK=TIME(J,3)+FLOAT(JJ-1)/FLOAT(RATES(LOCBYRT(K)))
                  LOCROW=LOCMIN(K)+INT(RATES(LOCBYRT(K))*(SECD-FLOAT
     1            (TIME(J,3))))
                  IF(TIMECK.EQ.SECD) PRTROW(KK)=MINUTE(J,LOCROW)
  20           CONTINUE
               CURTIM=PRTROW(1)*3600.+PRTROW(2)*60.+PRTROW(3)
               IF (FIRST) THEN
                DO 125 L=1,3
  125            SEGGO(L)=IFIX(PRTROW(L))
                FIRST=.FALSE.
                FORWRD=.FALSE.
                GOTO 127
               ENDIF
               IF(CURTIM.NE.SAVTIM+1.0) THEN
C----- Informational print
C                  WRITE (6,20300)
C20300             FORMAT (3X"Subr. KPRNT Third output")
                  IF(NFLT.LT.10) THEN
                   WRITE (4,1035) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C                   WRITE (6,1035) FLTTYP,NFLT,SEGGO,SEGEND
                  ELSE
                   WRITE (4,1036) FLTTYP,NFLT,SEGGO,SEGEND
C----- Informational print
C                   WRITE (6,1036) FLTTYP,NFLT,SEGGO,SEGEND
                  ENDIF
                  DO 126 L=1,3
  126              SEGGO(L)=IFIX(PRTROW(L))
               ELSE
                FORWRD=.TRUE.
               ENDIF
  127          SAVTIM=CURTIM
               DO 128 L=1,3
                SEGEND(L)=IFIX(PRTROW(L))
  128          CONTINUE
               ITRNPG=ITRNPG+1
  30        CONTINUE
  40     CONTINUE
         IPFLAG=1
  50  CONTINUE
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE GETVOL (IUNIT)
C
C   This routine, added because of the conversion to UNICOS, opens a
C   new raw data tape volume.  In the COS-JCL version, file marks were
C   stacked so anytime an EOF was detected, the code would just go and
C   read another record from the file that was concatenated to it.  If
C   an EOT occurred, the program would halt.
C
C  Arguments:
C   Input:   IUNIT   -- Unit number for reading data volumes
C
      INTEGER IUNIT
C
C
C  Common block for acquiring a new tape volume
      COMMON /GTVOL/ NUMTAP(500), NUMVOL,IFLGHT, IERR, IEXERR, LERR, LEOT
      INTEGER        NUMTAP,     NUMVOL,IFLGHT, IERR, IEXERR
      LOGICAL                                                 LERR, LEOT
      COMMON /GTVOL/ IFMT, STRING, VOLUME(500,20), CFLT(500)
      CHARACTER*80   IFMT, STRING
      CHARACTER*6                  VOLUME
      CHARACTER*4                                 CFLT
C  COMMON /GTVOL/ Variables 
C     NUMTAP -- Number of tape volumes available (per flight number)
C     NUMVOL -- Tape volume number to process
C     IFLGHT -- Flight number to process
C     IERR   -- Volume open error return code
C     IEXERR -- Unix command Error return code
C     LERR   -- Error occurred closing IUNIT, if .TRUE.
C     LEOT   -- No more tapes to process, if .TRUE.
C     IFMT   -- Format for system call to assign the unit number
C     STRING -- UNIX command sent to UNICOS
C     VOLUME -- Array of tape labels (flight number, number within flight)
C     CFLT   -- Array of flight titles
C
C
C   Local variables:
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
C  Test for available tape
      LEOT = NUMVOL.GT.NUMTAP(IFLGHT)
      LERR = LEOT
      IF (LEOT) RETURN
C  Close old volume and open new one.
      CLOSE (UNIT=IUNIT,IOSTAT=IERR,ERR=10)
      IFMT = '(" assign -a "A" fort."I1)'
      IF (IUNIT.GT.9) IFMT(26:26) = '2'
      WRITE (STRING,IFMT) VOLUME(NUMVOL,IFLGHT), IUNIT
      WRITE (6,'(A)') STRING
      IEXERR = IEXEC(STRING)
      LERR = IEXERR.NE.0
C  Done.
      RETURN
C
C  Error processing
   10 CONTINUE
      LERR = .TRUE.
      RETURN
      END
