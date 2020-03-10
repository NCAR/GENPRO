      PROGRAM TP2MS
C  GPTP2MS FORTRAN --  create MIGS script to stage in flight tapes
C
C   Input --   unit 34: SUMMARY file for project
C              on command line: project number
C   Output --  unit 35: JOB &  ACCOUNT statements for JCL
C              unit 36: command lines for TPSTAGE Proc
C              unit 37: TAPELIST EXEC input file
C
C  Revised to include project number on each msimport request
C    by Ron Ruth  August 1991
C  Revised to use MIGS instead of the Cray by Ron Ruth  February 1993
C
      INTEGER YEAR
      CHARACTER*3 PROJNO,AIRCFT
      CHARACTER*2 inp, ExaByte, HExaByte, MagTape
      CHARACTER*4 ACCNT
      CHARACTER*5 FLTNO(100)
      CHARACTER*6 TAPENO(100)
      CHARACTER*8 USER
      CHARACTER*80 STRING
      CHARACTER*5 FLTNUMB
C
      DATA ExaByte  / "EX" /
      DATA HExaByte / "QD" /
      DATA MagTape  / "MT" /
C
C
c Get project number from command line
      call getarg (1,projno)
C
C Open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
c  Open input/output files
      open (unit=34,file="summary."//projno,form='FORMATTED')
C  Derive aircraft number
      IF(PROJNO(1:1).NE.'2') THEN
        AIRCFT='30'//PROJNO(1:1)
      ELSE
        AIRCFT='312'
      ENDIF
C  Get user name and account #
      REWIND(34)
      READ(34,'(A8,9X,A4)') USER,ACCNT
C  Get project year
      CALL GTYEAR(YEAR,34)
C  Offer individual tapes or all of 'em
      open (unit=12, file="xom",form='formatted')
      write(6,'(///'' Are the tapes on:'',/,'' 1: 1/2 magnetic tape'',
     +/,'' 2: ExaByte'',/'' 3: High-density ExaByte'',/
     +''    Choose 1,  2, 3 or <cr> to exit'')')
      read(5,'(i1)') kk
      if ((kk .lt. 1) .or. (kk .gt. 3)) goto 500
      if (kk .eq. 1) inp = MagTape
      if (kk .eq. 2) inp = ExaByte
      if (kk .eq. 3) inp = HExaByte
      WRITE(12,*) inp
      WRITE(6,'(//'' To stage in selected flights, choose option 1;'')')
      WRITE(6,'('' to stage in all, choose option 2: '')')
      WRITE(6,'(//,''  (1) Specific tapes  (2) All tapes'')')
      READ(5,'(I1)') N
      if (n.eq.0) goto 500
      NUM=0
      IF (N.EQ.1) THEN
        CALL DISPLA(TAPENO,FLTNO,NUM)
      ELSE
c   Read in summary file and skip the first record
        READ(34,'(A80)') STRING(1:80)
        write(38,10200) 
c   Entry point for more data
   50   CONTINUE
        I = NUM + 1
        READ(34,'(A6,10X,A5)',END=100) TAPENO(I), FLTNO(I)
        NUM = NUM + 1
        GO TO 50
  100   CONTINUE
      END IF
      if (num.eq.0) goto 500
C **************************************************
C  Create a MIGS job script
      open (unit=37,file="tapelist",form='formatted')
      open (unit=38,file="tp2ms."//projno,form='FORMATTED')
      write(38,10200) 
10200 FORMAT ("#! /bin/csh -f")
      DO 45 K=1,NUM
        FLTNUMB = FLTNO(K)
        call caps (FLTNO(K))
        WRITE (38,10201) inp
10201   FORMAT ("nrnet msimport % r df=BI bksz=50000 rcfm=U dvty="A2
     +          " \\")
        IF (inp.eq.MagTape) WRITE (38,10202)
10202   FORMAT (" den=1600 rtry=yes eotp=END ermx=99 \\")
        WRITE (38,10203)
10203   FORMAT (" name=RAF mvn=CTRAFDMG w=RAFDMG rtpd=4095 disp=KEEP cla
     +s=QQ \\")
c   If FLTNUMB value only has 4 characters, must change the format
c     so there are no blanks
        IF (FLTNUMB(5:5) .EQ. " ") THEN
          WRITE(38,10204)AIRCFT,TAPENO(K),YEAR,PROJNO,FLTNO(K),TAPENO(K)
10204     FORMAT (" proj=41113"A3" skpr=0 nrec=6500 vlsr=",A6,
     +            " flnm=/RAF/19",I2,"/",A3,"/",A4,"/",A6)
        ELSE
          WRITE(38,10205)AIRCFT,TAPENO(K),YEAR,PROJNO,FLTNO(K),TAPENO(K)
10205     FORMAT (" proj=41113"A3" skpr=0 nrec=6500 vlsr=",A6,
     +            " flnm=/RAF/19",I2,"/",A3,"/",A5,"/",A6)
        END IF
        WRITE(38,*) " "
C   Add tape to email list.
        WRITE(37,'(1x,A6)') TAPENO(K)
   45 CONTINUE
      write(38,*) "# done."
      write(38,*) "exit"
  500 CONTINUE
      CALL EXIT (0)
      END
C **************************************************
      SUBROUTINE DISPLA(TAPENO,FLTNO,NUM)
C
C  Display choices from SUMMARY file and get user choice(s).
C
      CHARACTER *5 FLTNO(*),FLTNUM
      CHARACTER *6 TAPENO(*)
      CHARACTER*90 STRING
C
      write(6,'(////)')
      REWIND(34)
C  Skip 1st record of project information
      READ(34,'(A80)') STRING(1:80)
C  Display 4 across screen; get user response
      J=0
      STRING=' '
 1215 CONTINUE
      READ(34,'(16X,A5)',END=1116)  FLTNUM
      WRITE(STRING(J*20+1:J*20+5),'(A5)') FLTNUM
      J=J+1
      IF (J.EQ.4) THEN
        J=0
        WRITE(6,'('' '',A80)') STRING
        STRING=' '
      ENDIF
      GOTO 1215
 1116 CONTINUE
      IF (J.LT.4) WRITE(6,'('' '',A80)') STRING
      WRITE(6,'(//,'' Select Flight(s) -- <r> to exit: '')')
      WRITE(6,'('' Select one at a time and hit '')')
      WRITE(6,'('' return <r> after each entry. '')')
  401 CONTINUE
      READ(5,'(A)') FLTNO(NUM+1)
c   Removed l from caps below
c      call caps (FLTNO)
      call lcaps(fltno(num+1))
      if (fltno(num+1).eq."") return
C  Search SUMMARY file for FLTNO
      REWIND(34)
C  Skip 1st record of project information
      READ(34,'(A5)') FLTNUM
    5 CONTINUE
      READ(34,'(16X,A5)',END=999) FLTNUM
      IF(FLTNUM.NE.FLTNO(NUM+1)) GOTO 5
      NUM=NUM+1
      BACKSPACE(34)
      READ(34,'(A6,10X,A5)') TAPENO(NUM),FLTNO(NUM)
      GOTO 401
 999  CONTINUE
      WRITE(6,'('' Flight # not found; retry or <cr> to exit'')')
      GOTO 401
      END
