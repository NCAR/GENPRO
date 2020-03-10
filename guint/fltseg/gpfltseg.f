C     FLTSEG -- modify COMPFLT file to reflect flight gaps
C             called fltseg.out projno workingpath
C
C  INPUT FILES -- Unit 55: COMPFLT <project number> <workdisk>
C                 Unit 58: SEGMNTS <project> <workdisk>
C  OUTPUT FILE -- Unit 57: MODCOMP <project number> <workdisk>
C                 Unit 56: MODSEGS.projno
C
C		If tracing becomes neccessary (for debugging) "uncomment"
C			the lines that write to unit 66, also uncomment the
C			lines that open unit 66.
C
C assuming 10 maximum flight segments per flight
      PARAMETER(MAXSEG=20)
      CHARACTER *6  TAPNO,IDATEF,OUTPUT
      INTEGER SEGGO,SEGEND,BEGIV,ENDIV,SAVGO,SAVEND
      CHARACTER *5  FLT
      CHARACTER *4 FLTSEG,SAVSEG
      CHARACTER *35 DESCRP
      CHARACTER *80 PROJ
      CHARACTER *1 SUFFIX(MAXSEG),TAPE1
      CHARACTER *2 X
      integer       lindex
      character *3  arg1
      CHARACTER *80 fulpth ,arg2
      DATA SUFFIX/'a','b','c','d','e','f','g','h','i','j',
     $            'k','l','m','n','o','p','q','r','s','t' /X/'  '/
      DATA OUTPUT/'GXXXXX'/ TAPE1/'0'/
C
C  VARIABLES:
C
C  MAXSEG  -- maximum # of segments
C  TAPNO   -- current tape V number
C  IDATEF  -- date of flight
C  SEGGO, SEGEND -- begin/end of current segment  (SEGMNTS)
C  SAVGO,SAVEND  -- Save begin/end of current segment
C  BEGIV,ENDIV  --  begin/end of times covered on current tape (COMPFLT)
C  FLT    --  current flight number
C  FLTSEG --  current flight segment number (FLT w/o letter suffix)
C  SAVSEG --  saved last FLTSEG to compare with next one
C  OUTPUT --  output tape G number: for now, same as first input tape #
C  DESCRP --  description of flight
C  PROJ   --  first record of Summary file, project info
C  SUFFIX --  list of suffix letters
C  NUMSEG --  current segment number, index into suffix alphabet
C  TAPE1  --  suffix of first tape containing start of segment
C  NUMTAP --  number of tapes needed to wholly contain the segment
C
C
      SAVSEG='X'
C --- assumed that both input files are sorted by increasing flight #'s
C get first record of project information
      call GETARG(1,ARG1)
      call getarg(2,arg2)
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
      lindex = index(arg2,' ')
C-------------set lindex to the last data character in arg2.
C--------Note that since lindex is not reset to blanks, each subsequent open
C 		must use a file name of equal or greater length than the
C               previous open.
      do 23,i=1,80
         fulpth(i:i) = ' '
 23   continue
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/compflt.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=55,file=fulpth,access='sequential',form='formatted')
      do 28,i=1,80
         fulpth(i:i) = ' '
 28   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/segmnts.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=58,file=fulpth,access='sequential',form='formatted')
      do 29,i=1,80
         fulpth(i:i) = ' '
 29   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/modcomp.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=57,file=fulpth,access='sequential',form='formatted')
      do 26,i=1,80
         fulpth(i:i) = ' '
 26   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/modsegs.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=56,file=fulpth,access='sequential',form='formatted')
C     do 27,i=1,80
C        fulpth(i:i) = ' '
C27   continue
C     fulpth(1:lindex) = arg2(1:lindex)
C     fulpth(lindex+1:lindex+10) = '/fsgtrace.'
C     fulpth(lindex+11:lindex+13) = arg1
C     open (unit=66,file=fulpth,access='sequential',form='formatted')
      READ(55,'(A80)') PROJ
      WRITE(57,'('' '',A80)') PROJ
C     adjust SEGMNTS to  provide for midnight crossover and minimum length
C     of any time segment.
      call adjseg(maxseg)
C get next flight number and interval
      READ(56,'(bz,A4,2X,I6,1X,I6)',END=2000) FLTSEG,SEGGO,SEGEND
      IF(SAVSEG.NE.FLTSEG) THEN
       NUMSEG=1
       SAVSEG=FLTSEG
       NUMTAP=1
      ENDIF
C     WRITE(66,'('' search Compflt for '',A4,'' from Segmnt'')') FLTSEG
C search COMPFLT file for next entry with this flight number
      rewind(55)
      read(55,'(a80)') proj
    5 READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',end=1500)
     $TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C     WRITE(66,'(''  Next entry from Compflt: '',A5)') FLT(1:4)
      IF (FLT(1:4).NE.FLTSEG) THEN
C      WRITE(66,'(''   no match'')')
       WRITE(57,'('' '',A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)')
     $ TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
       GOTO 5
      ELSE
C this is a match
C is start of segment within tape's interval?
C      WRITE(66,'(''   match'')')
C  10  IF (SEGGO.GE.BEGIV.AND.SEGGO.LE.ENDIV) THEN
   10  IF (SEGGO.LE.ENDIV) THEN
        IF (SEGEND.LT.BEGIV) THEN
C error condition -- stop
         WRITE(6,'('' ERROR: Segment not covered in SUMMARY file'')')
	 write(6,'(/'' You must edit the SUMMARY file to include all'',
     $''  segments indicated''/'' in the SEGMENTS file.'')')
      	 write(6,'(/'' Contact a GUINT expert''//'' Hit <r> to '',
     $'' continue ...'')')
         read(5,'(a)')ibob
         STOP
        ENDIF
   15   IF (BEGIV.GT.ENDIV) THEN
C error condition -- stop
         WRITE(6,'('' ERROR: Begin end time > end time '')')
      	 write(6,'(/'' Contact a GUINT expert''//'' Hit <r> to '',
     $  '' continue ...'')')
         read(5,'(a)')ibob
         STOP
        ENDIF
C write out the tape info -- this one is part of segment
   20   WRITE(57,'('' '',A6,2X,A6,2X,''TAPE'',A1,2X,I6,1X,I6,2X,A35)')
     $  TAPNO,IDATEF,FLT(5:5),BEGIV,ENDIV,DESCRP
C       WRITE(66,'('' Write to modcomp: TAPE'',A1)') FLT(5:5)
C determine first tape needed for input, if not already done
   24   IF (NUMTAP.EQ.1.AND.FLT(5:5).NE.' ') TAPE1=FLT(5:5)
C determine output tape volume number, if not already done
        IF (NUMSEG.EQ.1.AND.NUMTAP.EQ.1) OUTPUT='G'//TAPNO(2:6)
        IF (SEGEND.LE.ENDIV) THEN
C this segment is complete
         WRITE(57,'('' SEG'',A1,I2,2X,A6,2X,A4,A1,2X,I6,1X,I6,2X,
     $   ''OUTPUT TAPE: '',A6,'' 1ST INPUT '',A1)')
     $   SUFFIX(NUMSEG),NUMTAP,IDATEF,FLT(1:4),SUFFIX(NUMSEG),
     $   SEGGO,SEGEND,OUTPUT,TAPE1
C        WRITE(66,'(''    segment complete'')')
C save begin/end segment times in case adjustment needed
         SAVGO=SEGGO
         SAVEND=SEGEND
C get next segment
         READ(56,'(bz,A4,2X,I6,1X,I6)',END=2000) FLTSEG,SEGGO,SEGEND
C        WRITE(66,'('' next segment: '',A4)') FLTSEG
         IF(SAVSEG.NE.FLTSEG) THEN
C skip remaining entries for completed flight (COMP entry invalid)
   25     READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=1500)
     $    TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C         WRITE(66,'('' skip until COMP: '',A4)') TAPNO(1:4)
          IF(.NOT.(TAPNO(1:4).EQ.'COMP'.AND.FLT(1:4).EQ.SAVSEG)) GOTO 25
C if last flight had only one segment, rewrite it as a complete flight
          IF (SUFFIX(NUMSEG).EQ.'a') THEN
           BACKSPACE (57)
           WRITE(57,'('' '',A6,2X,A6,2X,A4,3X,I6,1X,I6,2X,
     $     ''OUTPUT TAPE: '',A6)')
     $     TAPNO,IDATEF,FLT(1:4),SAVGO,SAVEND,
     $     OUTPUT
          ENDIF
C new segment; initialize and look for next entry in COMPFLT file
          NUMTAP=1
          NUMSEG=1
          SAVSEG=FLTSEG
          GOTO 5
         ELSE
C add one more segment to list for this flight
          NUMSEG=NUMSEG + 1
          NUMTAP=1
         ENDIF
        ELSE
C segment ends after current tape  -- get next tape entry
         READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=1500)
     $   TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C        WRITE(66,'('' segment ends after this tape; next: '',A5)')FLT
         IF (TAPNO(1:4).NE.'COMP') THEN
C there is at least one more tape to cover this segment
          NUMTAP=NUMTAP + 1
         ELSE
C segment end time > last tape end time for this flight
C adjust: first, retrieve last tape number for this flight
C         WRITE(66,'('' backspace twice...'')')
          BACKSPACE(55)
          BACKSPACE(55)
C get correct TAPNO for output tape number
          READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=1500)
     $    TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C indicate that this flight in completed
          ENDIV=SEGEND
          GOTO 24
         ENDIF
         GOTO 20
        ENDIF
       ELSE
C start of segment not in this tape's interval; get next tape
        READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=1500)
     $  TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C       WRITE(66,'('' seg starts in next tape: '',A5)') FLT
       ENDIF
       GOTO 10
      ENDIF
 1500 IF(SEGGO.LT.BEGIV) GOTO 15
      IF(SEGGO.GT.ENDIV) GOTO 5
      WRITE(6,'('' ERROR: flt entry not found'')')
      write(6,'(/''  Contact a GUINT expert''//'' Hit <r> to '',
     $'' continue ...'')')
      read(5,'(a)') ibob
      STOP
C end of segments reached -- write out rest of COMPFLT file
C ...if last flight had only one segment, rewrite it as a complete flight
 2000 IF (SUFFIX(NUMSEG).EQ.'a') THEN
       BACKSPACE (57)
       WRITE(57,'('' COMP'',I2,2X,A6,2X,A4,3X,I6,1X,I6,2X,''OUTPUT '',
     $ ''TAPE: '',A6)')
     $ NUMTAP,IDATEF,FLT(1:4),SAVGO,SAVEND,
     $ OUTPUT
      ENDIF
C skip remaining entries for completed flight (COMP entry invalid)
  225 READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=1500)
     $TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C     WRITE(66,'('' skip remaining entries: '',A5)') FLT
      IF(.NOT.(TAPNO(1:4).EQ.'COMP'.AND.FLT(1:4).EQ.SAVSEG)) GOTO 225
 2001 READ (55,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)',END=2500)
     $TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
C     WRITE(66,'('' write to modcomp: '',A5)') FLT
      WRITE(57,'('' '',A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)')
     $TAPNO,IDATEF,FLT,BEGIV,ENDIV,DESCRP
      GOTO 2001
2500  CONTINUE
      END
      SUBROUTINE ADJSEG(MAXSEG)
C
C    ADJSEG -- Adjust SEGMNTS file to account for crossing over midnite
C    during flight, and to delete any segments of length less than
C    the minimum indicated
C
C  INPUT  FILE -- Unit 58: SEGMNTS <project> <workdisk>
C  OUTPUT FILE -- Unit 56: MODSEGS <project> <workdisk>
C
C    VARIABLES:
C
C    FLTNO        --  flight number just read from SEGMNTS file
C    LASTFL       --  last flight number read
C    STRING       --  internal file, converts HHMMSS to HH MM SS
C    START, STOPT --  HHMMSS start and stop
C    MINSEG       --  minimum length of any segment, in seconds
C    TOTSEG       --  total segments for any flight being processed
C    MAXSEG       --  maximum segments for any flight
C    LENG         --  length of current segment, in seconds
C    SHORT        --  shortest segment so far for current flight
C    GO, STP      --  HH MM SS start and stop
C    MIDNIT       --  true if current flight has crossed over midnight
C    OVER         --  true if TOTSEG exceeds MAXSEG
C
C
      CHARACTER *4 FLTNO,LASTFL
      CHARACTER *12 STRING
      INTEGER START,STOPT,MINSEG,TOTSEG,MAXSEG,LENG,SHORT,GO(3),STP(3)
      LOGICAL MIDNIT,OVER
      MINSEG=1
    1 CALL system("clear")
      WRITE(6,'(//,'' Processing Time Segments ... '')')
      REWIND(58)
      REWIND(56)
      TOTSEG=0
      LASTFL='NONE'
    5 READ(58,'(BZ,A4,2X,I6,1X,I6)',END=1000) FLTNO,START,STOPT
      IF (FLTNO.NE.LASTFL) THEN
C next flight -- report statistics if last flight was real; initialize
       IF (LASTFL.NE.'NONE') THEN
        CALL REPORT(LASTFL,TOTSEG,MAXSEG,SHORT,MINSEG,OVER)
C if MAXSEG exceeded, redo
        IF(OVER) GOTO 1
       ENDIF
       OVER=.FALSE.
       MIDNIT=.FALSE.
       LASTFL=FLTNO
       SHORT=86400
       TOTSEG=0
      ELSE
C same flight -- if any previous tape in this flight crossed midnight,
C adjust times accordingly
       IF (MIDNIT) THEN
        START=START+240000
        STOPT=STOPT+240000
       ENDIF
      ENDIF
C GPGTSEGS program splits up output when midnight is crossed
      IF (STOPT.EQ.235959)THEN
C verify the probable: that 235959 wasn't actually the end of flight
       READ(58,'(BZ,A4,2X,I6,1X,I6)',END=1000) FLTNO,IDUMMY,STOPT
       IF(FLTNO.EQ.LASTFL) THEN
C combine crossover entry's start with next entry's stop time
        MIDNIT=.TRUE.
        STOPT=STOPT+240000
       ELSE
C 235959 actually was end of this flight's tapes; reset for top of loop
        BACKSPACE(58)
       ENDIF
      ENDIF
C facilitate computation of LENG
      WRITE(STRING,'(I6,I6)')START,STOPT
      READ(STRING,'(I2,I2,I2,I2,I2,I2)')
     $GO(1),GO(2),GO(3),STP(1),STP(2),STP(3)
      LENG=STP(1)*3600+STP(2)*60+STP(3)-(GO(1)*3600+GO(2)*60+GO(3))
C skip this segment if it is less than minimum
      IF (LENG.LT.MINSEG) GOTO 5
C else write it to output file and save shortest segment, total segments
      WRITE(56,'(BZ,1x,A4,2X,I6,1X,I6)') FLTNO,START,STOPT
      IF (LENG.LT.SHORT) SHORT=LENG
      TOTSEG=TOTSEG + 1
C top of loop
      GOTO 5
 1000 CONTINUE
C EOF; report statistics on last flight
      CALL REPORT(LASTFL,TOTSEG,MAXSEG,SHORT,MINSEG,OVER)
C if MAXSEG exceeded, redo
      IF(OVER) GOTO 1
C give user a chance to stipulate a longer minimum
      WRITE(6,'('' If these segment lengths are OK, enter <r>; else, '',
     $''enter new minimum '',/,'' segment length in minutes: '')')
      READ(5,'(BN,I2)',END=1001) IMIN
      if (imin .eq. 0) goto 1001
      WRITE(6,'(/''   Enter Seconds: '')')
      READ(5,'(BN,I2)',END=99) ISEC
      GOTO 55
   99 continue
      ISEC=0
   55 MINSEG=IMIN*60 + ISEC
      GOTO 1
 1001 continue
      REWIND(56)
      RETURN
      END
      SUBROUTINE REPORT(LASTFL,TOTSEG,MAXSEG,SHORT,MINSEG,OVER)
      INTEGER TOTSEG,MAXSEG,SHORT,MINSEG
      LOGICAL OVER
      CHARACTER *4 LASTFL
       WRITE(6,'('' Flight '',A4,'' has '',I4,'' segment(s)'',
     $  '', the shortest of which was '',/,I4,'' minutes and '',I2,
     $  '' seconds.'',)') LASTFL,TOTSEG,SHORT/60,MOD(SHORT,60)
       IF(TOTSEG.GT.MAXSEG) THEN
        OVER=.TRUE.
        WRITE(6,'('' MAXIMUM # OF SEGMENTS IS '',I3,
     $  /'' Choose new minimum for this flight, longer than shortest'',
     $  '' segment stated above'',)') MAXSEG
   45   WRITE(6,'(''   Enter Minutes: '')')
        READ(5,'(BN,I2)',END=98) IMIN
        GOTO 50
   98   continue
        IMIN=0
   50   WRITE(6,'(''   Enter Seconds: '')')
        READ(5,'(BN,I2)',END=99) ISEC
        GOTO 55
   99   continue
        ISEC=0
   55   MINSEG=IMIN*60 + ISEC
        RETURN
       ENDIF
      RETURN
      END
