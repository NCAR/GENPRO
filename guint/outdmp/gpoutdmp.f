      PROGRAM OUTDMP
C
C   GPOUTDMP -- user interface to set up UNICOS script modfications and
C      run the GPTDMPCY program on Cray (for getting an output tape dump)
C
C   Usage:  GPOUTDMP proj# workpath header.flag
C
C     Input:  unit 61 -- SUMMARY file -- to get list of all flights
C             unit 63 -- COMPFLT file -- to get output volume number
C  ?not used? unit 65 -- GPOUTDMP JOB -- prototype deck to be modified
C             unit 66 -- OUTPUT file -- list of output vecvar variables
C     Output: unit 60 -- OUTDMP job script to be submitted to the Cray
C             unit 62 -- DATA file, to be read by the dump program
C
C
C   It is assumed that the output tape number is identical to the input
C   input tape number for given flight, except that the V changes to G.
C   However, if the flight contained a time gap, the output volume number
C   is adjusted to be the first input tape number for the collection of
C   tapes in that flight, again with the V changed to G.
C
C   It is assumed that the Summary file has been sifted through the
C   GPEDTSUM and GPFLTSEG exec files to produce a finalized COMPFLT file.
C
      integer YEAR
      character*1 arg3,icol
      CHARACTER*3 PROJNO,AIRCFT,turbrt
      CHARACTER*4 ACCNT
      CHARACTER*5 FLTNO
      CHARACTER*6 TAPENO
      PARAMETER (MAXOUT=400)
      CHARACTER*8 VARS(99),OUTNAM(MAXOUT)
      CHARACTER*8 USER
      CHARACTER*70 RATES
      character*80 arg2,fulpth,miscp
      CHARACTER*100 RECORD
      DIMENSION OUTPTR(MAXOUT)
      INTEGER OUTPTR,BEGIV(3),ENDIV(3),oct,ptime
      LOGICAL FOUND,HEADER
      DATA IHR1/0/IMIN1/0/ISEC1/0/IHR2/0/IMIN2/0/ISEC2/0/
      DATA IREC1/0/IREC2/0/

C get project number
      call GETARG(1,projno)
C get working path
      call getarg(2,arg2)
C get header flag
      call getarg(3,arg3)
      header = arg3 .eq. 'T'
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
C
C------------------------------------------------  open 61 (summary file)
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
C save size for later
      larg2 = lindex
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = projno
      open (unit=61,file=fulpth,access='sequential',
     $          form='formatted')
C------------------------------------------------  open 60 (job file)
      do 18, i = 1, 80
         fulpth(i:i) = ' '
 18   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/outdmp.'
      fulpth(lindex+9:lindex+11) = PROJNO
      open (unit=60,file=fulpth,access='sequential',
     $          form='formatted')
C-----------------------------------------------  open 62 (program data file)
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/tdump.'
      fulpth(lindex+8:lindex+10) = PROJNO
      open (unit=62,file=fulpth,access='sequential',
     $          form='formatted')
C-----------------------------------------------  open 63 (compflt file)
         do 14, i = 1, 80
            fulpth(i:i) = ' '
 14      continue
         fulpth(1:lindex) = arg2(1:lindex)
         fulpth(lindex+1:lindex+9) = '/compflt.'
         fulpth(lindex+10:lindex+12) = projno
         open (unit=63,file=fulpth,access='sequential',
     $          form='formatted')
C-----------------------------------------------  open 66 (output file)
         do 19, i = 1, 80
            fulpth(i:i) = ' '
 19      continue
         fulpth(1:lindex) = arg2(1:lindex)
         fulpth(lindex+1:lindex+8) = '/output.'
         fulpth(lindex+9:lindex+11) = projno
         open (unit=66,file=fulpth,access='sequential',form=
     $   'unformatted')
C-----------------------------------------------  open 65 (gpoutdmp.job file)
C  So far as I can tell, this unit is never accessed   RLR  900905
      call getenv ('outdmp',miscp)
C     fulpth = ' '
      lindex = index(miscp,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
C---------save size of miscp for later
      lmiscp = lindex
C     fulpth(1:lindex) = miscp(1:lindex)
C     fulpth(lindex+1:lindex+13) = '/gpoutdmp.job'
C     open (unit=65,file=fulpth,access='sequential',
C    $          form='formatted')
C
C derive aircraft number
      IF(PROJNO(1:1).NE.'2') THEN
       AIRCFT='30'//PROJNO(1:1)
      ELSE
       AIRCFT='312'
      ENDIF
C
      REWIND(61)
      READ(61,'(a8,3x,a3,3x,a4)',END=10) user,turbrt,accnt
      call caps(turbrt)
C get project year from unit 61 (record #2)
  10  call gtyear(year,61)
C offer list of flight numbers
      FOUND = .FALSE.
      CALL DISPLA(FOUND,TAPENO,FLTNO,BEGIV,ENDIV)
      IF (.NOT.(FOUND)) RETURN
C get output volume number from COMPFLT file, given the flight number
      CALL GTVOL(FLTNO,TAPENO)
C now modify default values in $IN according to user specs
C     CALL system("clear")
      WRITE (62,'("  Input data for tape dump program")')
      WRITE (62,'("   unicos/guint version ")')
      WRITE (62,'(" UNIT NUMBER   (I5)")')
      WRITE (62,'("     9")')
      WRITE (62,'(" WAS THE TAPE WRITTEN WITH GENPRO1 (0) OR GENPRO2 (1
     &)")')
      WRITE (62,'("     1")')
      WRITE (62,'(" ARE YOU USING GENPROII BINARY LIBRARY VER 11-12 (0) 
     &OR LIB01 (1)")')
      WRITE (62,'(" ANY OUTPUT TAPES GENERATED BEFORE DEC 1983 USES LIB 
     &11-12")')
      WRITE (62,'("     1")')
      WRITE(6,'(/'' Print Octal, Integer(packed), Decimal(unpacked)'',
     $''(1) or not (0)''/)')
      READ(5,'(I1)',END=500) oct
      WRITE(62,'('' PRINT OCTAL,INTEGER,DECIMAL (1) or not (0)?'')')
      WRITE(62,'(2X,I1)') oct
      WRITE(6,'(/'' TPTIME (0) or TPTIME(1) TPTIME is usual'',
     $'' or not (0)''/)')
      READ(5,'(I1)',END=500) ptime
      WRITE(62,'('' TPTIME (0) OR PTIME(1)?'')')
      WRITE(62,'(2X,I1)') ptime
      WRITE(6,'('' Range of times: '',3(I2,1X),'' to '',3(I2,1X))')
     $BEGIV,ENDIV
      WRITE(6,'(/'' Indicate Start HR MIN SEC (hh mm ss): '')')
      READ(5,'(BN,3(I2,1X))',END=500) IHR1,IMIN1,ISEC1
      if (ihr1 .eq. 0) goto 500
      WRITE(6,'(/'' Indicate Stop HR MIN SEC (hh mm ss): '')')
      READ(5,'(BN,3(I2,1X))',END=500) IHR2,IMIN2,ISEC2
      if (ihr2 .eq. 0) goto 500
      WRITE(62,'('' START AND STOP TIME OF FORMATTED DUMP (3F5.0)'')')
      WRITE(62,'(BN,1x,3I5,/,1x,3I5)') IHR1,IMIN1,ISEC1,IHR2,IMIN2,ISEC2
      WRITE(6,'(/'' Select Dump by (0) Rate or (1) Variable Name''/
     $ )')
      READ(5,'(I1)',END=500) IHOW
      WRITE(62,'('' DUMP VARS BY RATE (0) OR VARIABLE NAME (1)?'')')
      WRITE(62,'(2X,I1)') IHOW
C     CALL system("clear")
      NUM=0
      RATES='0000'
      IF (IHOW.EQ.1) THEN
       IF (HEADER) THEN
C get list of variables in output header
        CALL RDHDR(OUTNAM,MAXOUT,NMOUT)
C   Get Variables to dump
        CALL SELECT(OUTNAM,NMOUT,OUTPTR,NUM)
       ELSE
        WRITE(6,'('' Enter Variables for dump ... <r> when done: '')')
 1100   READ(5,'(A8)',END=1101) OUTNAM(NUM+1)
	call caps(outnam(num+1))
        if (outnam(num+1) .eq. "") goto 500
        NUM=NUM+1
        OUTPTR(NUM)=NUM
        GOTO 1100
       ENDIF
C don't continue if no vars selected
       IF (NUM.EQ.0) GOTO 500
      ELSE
       WRITE(6,'('' Select Rate(s) wanted by entering 1 below rate(s):''
     $,////////////////////,
     $''      1    5    10   15   20   21   25   50   62   70   150  '',
     $''210  250  '')')
       READ(5,'(A70)',END=1201) RATES
C don't continue if no rates selected
 1201  IF (RATES.EQ.'0000') THEN
C       PAUSE ' No rates selected ...'
        GOTO 500
       ENDIF
      ENDIF
 1101 continue
      WRITE(62,'(
     $'' RATES TO BE PRINTED [Y=1, N=0]  (13I5)   (POSSIBLE RATES)'',/,
     $''    1    5    10   15   20   21   25   50   62   70   150  '',
     $''210  250  '')')
      WRITE(62,'(1x,A70)') RATES
C
C     CALL system("clear")
      WRITE(6,'(/'' Select Print by (0) Column or (1) Horizontally''/
     $ )')
      READ(5,'(A1)',END=500) ICOL
      if (icol .eq. "") goto 500
      WRITE(62,'('' PRINTED BY COLUMN (0) OR HORIZONTALLY (1)?'')')
      WRITE(62,'(2X,A1)') ICOL
      WRITE(62,'('' HOW MANY VARIABLES DO YOU WANT TO DUMP?'')')
      WRITE(62,'(BN,1x,I2)') NUM
      WRITE(62,'('' LIST OF VARIABLES FOR OUTPUT DUMP: '')')
      WRITE(62,'(1x,8(1X,A8))') (OUTNAM(OUTPTR(J)),J=1,NUM)
C write out command line -- if we've gotten this far, it means user input
C is completed and the DATA file will exist as flag for the calling program
C
C build the include file for the job script
C
C write out account number change statement
      WRITE (60,'(" newacct 41113",A3/)') AIRCFT
C write out command to start job accounting
      WRITE (60,'(" ja                    # start job accounting"/)')
      WRITE (60,'(" set SOURCE = "A)') miscp(1:lmiscp)
      WRITE (60,'(" set DATA = "A)') arg2(1:larg2)
      WRITE (60,'(" set DEST = "A)') arg2(1:larg2)
      WRITE (60,'(" set PROGM = gptdmpcy")')
C set up statment to acquire the requested data set from the Mass Store
C Example:  set MSSFILE = /RAF/1989/761/LRT/RF05A
      WRITE (60,'(" set MSSFILE = /RAF/19"I2"/"A3"/"A3"/"A)')
     & YEAR, PROJNO, TURBRT, FLTNO
      WRITE (60,'(" set USER = "A)') USER
      WRITE (60,'(" set HOST = spock.atd.ucar.edu")')
      WRITE (60,'(" set PRGDATA = tdump."A)') projno
      WRITE (60,'(" set DATOUT = tdump."A3"out")') projno
      WRITE (60,'(" set LOGFILE = tdump."A3"log")') projno

  500 continue
C     CALL system("clear")
      RETURN
      END
C                                                                   DISPLA
      SUBROUTINE DISPLA(FOUND,TAPENO,FLTNO,BEGIV,ENDIV)
C
C  display choices from SUMMARY file and get user choice; return FOUND
C false if choice not found, true if found
C
      CHARACTER *5 FLTNO,FLTNUM
      CHARACTER *6 TAPENO
      CHARACTER*80 STRING
      INTEGER BEGIV(3),ENDIV(3)
      LOGICAL FOUND
C
C     CALL system("clear")
      REWIND(63)
C skip 1st record of project information
      READ(63,'(A80)') STRING
C display 4 across screen, get user response
      J=0
      STRING=' '
 1215 READ(63,'(16X,A5)',END=1116)  FLTNUM
      call caps (FLTNUM)
      IF (FLTNUM(1:4).EQ.'TAPE') GOTO 1215
      WRITE(STRING(J*20+1:J*20+5),'(A5)') FLTNUM
      J=J+1
      IF (J.EQ.4) THEN
       J=0
       WRITE(6,'('' '',A80)') STRING
       STRING=' '
      ENDIF
      GOTO 1215
 1116 IF (J.LT.4) WRITE(6,'('' '',A80)') STRING
      WRITE(6,'(//,'' Select Flight -- <r> to exit: '')')
  401 READ(5,'(A5)',END=402) FLTNO
      call caps(fltno)
      if (fltno .eq. "") goto 402
C  search SUMMARY file for FLTNO
      REWIND(63)
C skip 1st record of project information
      READ(63,'(A5)') FLTNUM
    5 READ(63,'(16X,A5)',END=999) FLTNUM
      call caps (FLTNUM)
      IF(FLTNUM.NE.FLTNO) GOTO 5
      FOUND=.TRUE.
      BACKSPACE(63)
      READ(63,'(A6,10X,A5,2X,3(I2),1X,3(I2))') TAPENO,FLTNO,BEGIV,ENDIV
      call caps (FLTNO)
      RETURN
 999  WRITE(6,'('' Flight # not found; retry or <cr> to exit'')')
      GOTO 401
  402 continue
      RETURN
      END
C                                                                   SELECT
      SUBROUTINE SELECT(IARRAY,IDBSIZ,IMODE,ISIZE)
C
C  - supply list of all possible variables for selection
C  - allow user to specify which ones are desired
C
C   VARIABLES ---
C
C   IARRAY: incoming array of variable names
C   IDBSIZ: total possible maximum # of names that can be selected
C   DBASE:  workspace array getting the database
C   IMODE:  array of pointers to selected variables in IARRAY
C   ISIZE:  # of selected vars in IARRAY
C   RECORD: single record read/written, various utility usage
C   CH:     user response
C   IFMT:   variable format used to parse a range input
C   FLAG:   flag on array element to de-select given variable name
C   CHOSEN: flag on array element to select given variable name
C   IROWS:  number of rows to display to screen
C   IGO, IEND: begin and end characters on each array element for flags
C   ISTRT:  array element number with which to start next screen display
C   ICOUNT: total number of variables selected at any given time
C
C   ASSUMPTIONS ---
C
C    1: The incoming array IARRAY is a Character*8 array of no more than
C       IDBSIZ words.
C
      INTEGER IDSIZ
      CHARACTER *16 DBASE(200),RECORD
      CHARACTER * 9 CH , IFMT
      CHARACTER * 8 IARRAY(1)
      CHARACTER * 7 FLAG, CHOSEN
      INTEGER  IMODE(1),ISIZE
      DATA IROWS/18/IGO/10/IEND/12/FLAG/'   '/CHOSEN/'XXX'/
C
C  read in database and set flags as function of IMODE array
C
      DO 444 J=1,IDBSIZ
       DBASE(J)(1:8)=IARRAY(J)
       DBASE(J)(9:16)='       '
  444 CONTINUE
C  ISIZE is # of valid words in IMODE, the array of pointers, on entry
      DO 445 J=1,ISIZE
       DBASE(IMODE(J))(IGO:IEND)=CHOSEN
  445 CONTINUE
C
C  start with 1st element
C
      ISTRT=1
C
C  top of processing loop -- display IROWS elements starting with the
C  ISTRT'th element to screen
C
   10 CONTINUE
C     CALL system("clear")
C  get an INTEGER *4 argument for MIN0
      IDSIZ=IDBSIZ
      DO 50 J=ISTRT,MIN0(IDSIZ,ISTRT + IROWS - 1)
       WRITE(6,110) J,DBASE(J)
  110  FORMAT(' ',I3,': ',A16)
   50 CONTINUE
      WRITE(6,'(/'' Select Variables for dump ... H for help ...'')')
C
C  clear input buffer and read next user input
C
   55 CH='         '
      READ(5,120,END=200) CH
      call caps(ch)
      if (ch .eq. "") goto 200
  120 FORMAT(A9,BN)
C
C  select action
C
      IF (CH.EQ.'C') THEN
C  count number of currently selected variables
C
       ICOUNT=0
       DO 255 K=1,IDBSIZ
        IF (DBASE(K)(IGO:IEND).EQ.CHOSEN) ICOUNT=ICOUNT + 1
  255  CONTINUE
       WRITE(6,2555) ICOUNT
 2555  FORMAT(' TOTAL SELECTED: ',I5)
       GOTO 55
      ELSEIF (CH.EQ.'S') THEN
C
C  Relate DBASE to IARRAY, IMODE and ISIZE
C
       ISIZE=0
  225  DO 150 K=1,IDBSIZ
        IF (DBASE(K)(IGO:IEND).EQ.CHOSEN) THEN
         ISIZE=ISIZE+1
         IMODE(ISIZE)=K
        ENDIF
  150  CONTINUE
C      CALL system("clear")
       RETURN
      ELSEIF (CH.EQ.'Q') THEN
C
C  Quit without saving anything
C
C      CALL system("clear")
       RETURN
      ELSEIF (CH.EQ.'F') THEN
C
C  go forward one screen's worth, back to top of loop
C
       ISTRT=ISTRT + IROWS
       IF (ISTRT.GT.IDBSIZ) ISTRT=1
       GOTO 10
      ELSEIF (CH.EQ.'B') THEN
C
C   go backward one screen's worth, back to top of loop
C
       ISTRT=ISTRT - IROWS
       IF (ISTRT.LT.1) ISTRT=IDBSIZ - IROWS + 1
       GOTO 10
      ELSEIF (CH.EQ.'H') THEN
C
C   Display Help screen
C
C      CALL system("clear")
       WRITE(6, 969)
  969  FORMAT ('                             COMMANDS'//
     $T10,'   B        go backward one screen                      '/
     $T10,'   C        display number of vars selected             '/
     $T10,'   F        go forward one screen                       '/
     $T10,'   H        help screen                                 '/
     $T10,'   Q        quit without saving current list            '/
     $T10,'   S        quit and save current list           '/
     $T10,'  <r>       forward 1 screen'/
     $T10,'   n        for n''th name, select if unselected, vica versa'
     $,/,T10,'             XXX after name means that name is selected'/
     $T10,' n1:n2      for n1''th through n2''th names, select each '/
     $T10,'             if unselected, vica versas...e.g., 32:35     '/
     $T10,'             ... XXX after a name means it is selected   '/
     $T10,/'   **** enter <r> to return to display  ****             '
     $)
 7070  READ(5,120,END=7071) CH
      call caps(ch)
      if (ch .eq. "") goto 7071
       GOTO 7070
 7071  REWIND (5)
       GOTO 10
      ELSEIF (INDEX(CH,':').GT.0) THEN
C
C  a range has been specified...parse the string to extract beginning
C  and ending indices in array to be flagged
C
       IPOS=INDEX(CH,':')
       WRITE (IFMT,250) IPOS - 1, INDEX(CH,' ') - IPOS - 1
  250  FORMAT('(I',I1,',X,I',I1,')')
       READ(CH,IFMT) ICH1,ICH2
C
C   toggle current flag for each element in range specified, back to top
C   ... if start of range is greater than end, nothing will happen
C
       DO 251 K=ICH1,ICH2
        IF (DBASE(K)(IGO:IEND).EQ.FLAG) THEN
         DBASE(K)(IGO:IEND)=CHOSEN
        ELSE
         DBASE(K)(IGO:IEND)=FLAG
        ENDIF
  251  CONTINUE
       GOTO 10
      ELSE
C
C   else it is assumed that a single integer is input to specify that
C   element for flagging...if an alphanumeric character or invalid number
C   is input, simply go back to top of loop
C
       READ (CH,160,IOSTAT=IERR,ERR=55) ICH
       IF (ICH.GT.IDBSIZ.OR.ICH.LT.1) GOTO 10
  160  FORMAT (BN,I3,6X)
C
C   toggle flag for that element and go back to top of loop
C
       IF (DBASE(ICH)(IGO:IEND).NE.FLAG) THEN
        DBASE(ICH)(IGO:IEND) = FLAG
       ELSE
        DBASE(ICH)(IGO:IEND) = CHOSEN
       ENDIF
       GOTO 10
      ENDIF
  200 CONTINUE
C
C   Come here on a carriage return input (empty string ==> end of file
C   on the terminal input device)...go forward one screen and get back
C   to top of loop
C
      continue
      ISTRT=ISTRT + IROWS
      IF (ISTRT.GT.IDBSIZ) ISTRT=1
      GOTO 10
      END
C                                                                   RDHDR
      SUBROUTINE RDHDR(OUT,MAX,NUM)
C
C  read in the OUTPUT file for this project (list of output vecvar vars)
C
      CHARACTER *8 OUT(MAX)
      REWIND (66)
      READ(66) NUM, (OUT(J),J=1,NUM)
      RETURN
      END
C                                                                   GTVOL
      SUBROUTINE GTVOL(FLTNO,TAPENO)
      CHARACTER *5 FLTNO,IFLGHT
      CHARACTER *6 TAPENO
      CHARACTER *12 FLAG
      character *80 iproj
C
C  search COMPFLT file for the given flight number and return the
C  corresponding output volume number
      REWIND(63)
      READ(63,'(A80)',END=100) IPROJ
    5 READ(63,'(16x,a5,17x,a12,1x,a6)',END=100)iflght,flag,tapeno
      call caps (iflght)
      IF(IFLGHT.NE.FLTNO) GOTO 5
C     TAPENO=T1
C     IF(FLAG.EQ.'OUTPUT TAPE:') TAPENO=T2
      RETURN
  100 WRITE(6,'('' Flight number '',A5,'' not found in Compflt file'',/,
     $'' ... Hit <r> to continue'')') FLTNO
      READ(5,'(A)',END=5000) I
5000  continue
      STOP
      END
