
      SUBROUTINE FILE (IOPTN,IREC,IERR,LUE) 
C    &          , DAP File Read/Write Subr.    MLG/RLR/CAW/MDD
C*      Copyright University Corporation for Atmospheric Research, 1995   *
C>
C Subroutine Arguments: 
C  IOPTN (input)  => Task to perform
C     1 => Read header (open header file, read both records, close the file)
C     2 => Read data (open data file, if necessary; leave it open, shared)
C     3 => Write header (open header file, write both records, close file)
C     4 => Write data (open data file, if necessary; leave open, exclusive) 
C     5 => Close the data file, writing out the record first, if necessary) 
C     6 => Creat and close the header file
C     7 => Creat the data file
C 
C  IREC  (input)  => Record number for data file read/write (2-word integer)
C 
C  IERR  (output) => Error return 
C     0 => No error 
C     1 => Illegal IOPTN
C     2 => Illegal record number
C     3 => Unexpected header EOF
C    <0 => FMP error number 
C 
C  LUE   (input)  => Logical unit for reporting any error (0=none). 
C 
C 
C   Assumption:  During program execution, the data file is intended
C     to be either read or written.  The data file is opened for shared 
C     access by the first read request and opened for exclusive access
C     by the first write request.  It remains open in its shared/exclusive
C     mode for faster response to subsequent read/write requests. 
C 
C 
C***  Revised by RLR to include a global "file open" flag which allows
C      proper execution of programs needing MLLDR.  <850221.1224>  ** 
C 
C***  Revised by RLR to add more error returns. <850605.1430> **
C 
C***  Revised by RLR to make INTAS a stmt. fcn. <850613.1530> **
C 
C***  Revised by MLG to do blocked disk I/O     <860212.1030> **
C 
C***  Revised by RLR to fix minor bugs          <860624.1650> **
C 
C***  Revised by MDD to use F77 OPEN in create  <860904.0900> **
C 
C     IMPLICIT INTEGER*2 (I-N),REAL*4(A-H,O-Z)
      INTEGER*4 IOPTN,IREC,IERR,LUE
C 
      INCLUDE 'file.com'
      INCLUDE 'slus.prm'
C 
C****** LOCAL VARIABLES ******
      INTEGER MONTH(12) 
      character*80 hfile,dfile
      integer lhf
C 
      DATA MONTH / 31,28,31,30,31,30,31,31,30,31,30,31 /
C 
C  Length of header record #1.
      DATA LHDR1 / 1336 / 
C  Length of header record #2.
      DATA LHDR2 /  NVARSZ / 
      save hfile,dfile,lhdr1,lhdr2,lhf
C 
C 
C       * * * * * * * * * * * * * * * * 
C      *                               *
C     *  Arithmetic statement function  * 
C      *                               *
C       * * * * * * * * * * * * * * * * 
C 
C   Convert 0=<Integer<100 to ASCII             RLR/CAW <850613.1530> **
C     INTAS(IC) = 30060B+MIN0(IABS(IC),90)/10*400B+MOD(IABS(IC),10) 
C     INTAS(IC) = '30060'O+MIN0(IC,90)/10*'400'O+MOD(IC,10)
c     INTAS(IC) = o'30060'+MIN0(IC,90)/10 * o'400' + MOD(IC,10)
      INTAS(IC) = 12336+MIN0(IC,90)/10 * 256 + MOD(IC,10)
C 
C 
C       * * * * * * * * * * * * * * * 
C      *                             *
C     *  Executable code starts here  * 
C      *                             *
C       * * * * * * * * * * * * * * * 
C 
C****** PERFORM REQUESTED TASK, IF POSSIBLE ******
c     WRITE(LUE,961)IOPTN,IREC,IERR,LUE 
c     WRITE(6,961)IOPTN,IREC,IERR,LUE 
  961 FORMAT('FILE:IOPTN,IREC(1),IERR,LUE=',I3,I7,2I5) 
      IERR = 0
      GO TO (1001,100,200,300,400,500,600,700,1002) IOPTN+1 
C           Bogus  RH  RD  WH  WD END CrH CrD Bogus 
C 
C 
C****** COME HERE TO READ THE HEADER FILE ******
  100 CONTINUE
C 
C****** CLOSE THE DATA FILE, IF OPEN
      IF (IOPNFI) THEN
        CLOSE(34,IOSTAT=IERR) 
        IOPNFI=.FALSE.
        IF (IERR.NE.0) GO TO 1003 
      ENDIF
C****** OPEN THE HEADER FILE FOR EXCLUSIVE ACCESS ******
C     WRITE(LUE,855) NMHEAD 
C 855 FORMAT(' OPEN HDR FILE 'A6,' FOR READ') 
c     OPEN(33,FILE=DAPDTA//NMHEAD,STATUS='OLD',IOSTAT=IERR,
      call cname (hfile, 'XANDATA', NMHEAD,LHF)
      OPEN(33,FILE=hfile(1:LHF),STATUS='OLD',IOSTAT=IERR,
     &     FORM='UNFORMATTED')
      IF(IERR.NE.0.AND.LUE.GT.0) then
         if(ierr.eq.118) then
            write(lue,7110) NMHEAD
 7110       format(' Cannot find file ', A6)
            return
         else
            WRITE(LUE,7111)IERR
 7111       FORMAT(' Error opening header file for read =', I9) 
            return
         endif
      endif
      REWIND(33)
C
C****** READ HEADER RECORD #1 ******
      READ(33,ERR=1004,IOSTAT=IERR,END=999) NMUSER,IDATEF, 
     1 ITIMEF,NOTES,IDATED,IPROJ,IARCFT,IFLGHT,NMRECS,NWORDS,IDELTT,
     2 NTMSEG,NVAR,NVOLT,NAMES,UNITS,QCX,PSX,TTX,AVANE,BVANE,DPX 
C 
C****** READ HEADER RECORD #2 ******
      READ(33,ERR=1005,IOSTAT=IERR,END=999) ITMSEG
C 
C****** REWIND TO UPDATE HEADER ACCESS TIME ******
      REWIND (UNIT=33,ERR=1006,IOSTAT=IERR) 
      CLOSE (33,IOSTAT=IERR)
      return
C 
C****** COME HERE TO READ A DATA FILE RECORD ****** 
  200 CONTINUE
C 
C****** QUIT IF REQUESTED RECORD IS OUT OF BOUNDS ******
      IF (IREC.GT.NMRECS .OR. IREC.LT.1) GO TO 1007 
C
C****** OPEN DATA FILE OPEN FOR READING, IF NECESSARY ******
      IF (.NOT.IOPNFI) THEN 
c     OPEN(44,FILE=DAPDTA//NMDATA,STATUS='OLD',IOSTAT=IERR,
         call cname (dfile, 'XANDATA', NMDATA,LDF)
         OPEN(34,FILE=dfile(1:LDF),STATUS='OLD',IOSTAT=IERR,
     1        ACCESS='DIRECT',RECL=(NWORDS*4),FORM='UNFORMATTED')
         REWIND(34)
C     WRITE(LUE,2667)IERR 
C2667 FORMAT(' FILE: OPEN IERR=', I6)
         IF (IERR.NE.0) GO TO 1008 
         IOPNFI = .TRUE. 
      END IF
C 
C****** READ THE DESIRED RECORD ******
      IL = NWORDS 
      READ(34,REC=IREC,IOSTAT=IERR)IHR,IMIN,ISEC,IMSEC,
     &                             (VALUES(NMBR),NMBR=1,NVAR)
C     WRITE(LUE,4674)IERR 
C4674 FORMAT(' FILE:READ ERR=', I6) 
      RETURN
C 
C 
C****** COME HERE TO WRITE THE HEADER FILE ****** 
  300 CONTINUE
C 
C****** OPEN THE HEADER FILE FOR WRITING ****** 
C     WRITE(LUE,97) 
C 97  FORMAT(' OPEN HEADER TO WRITE') 
c     OPEN(33,FILE=DAPDTA//NMHEAD,ERR=1009,IOSTAT=IERR,
c....... use already defined hfile:
c     call cname (hfile, 'XANDATA', NMHEAD,LHF)
      write(0,'(" open h file for writing, name=",(a))') hfile(1:LHF)
      OPEN(33,FILE=hfile(1:LHF),ERR=1010,IOSTAT=IERR,
     1 STATUS='OLD',FORM='UNFORMATTED')
      write(0,'(" successfully opened file ",(a))') hfile(1:LHF)
      REWIND(33)
C 
C****** PLACE THE CURRENT DATE AND TIME INTO THE HEADER ******
c....... no longer used; instead write "A" to identify format as
c..      FORMAT-code A, 11-character variable names
      IDATEF(1)='A '
 320  CONTINUE
C 
C****** WRITE HEADER RECORD #1 ****** 
C     WRITE(LUE,92) 
C 92  FORMAT(' WRITE HDR REC 1')
      WRITE(33,IOSTAT=IERR) NMUSER,IDATEF,ITIMEF, 
     1 NOTES,IDATED,IPROJ,IARCFT,IFLGHT,NMRECS,NWORDS,IDELTT,NTMSEG,
     2 NVAR,NVOLT,NAMES,UNITS,QCX,PSX,TTX,AVANE,BVANE,DPX 
       IF(IERR.NE.0.AND.LUE.GT.0)WRITE(LUE,7234)IERR 
 7234 FORMAT(' WRITE HEADER REC 1 ERR=', I4)
C     WRITE(LUE,7254) NMUSER,IPROJ,IARCFT,IFLGHT,NWORDS 
C7254 FORMAT('NMUSER,IPROJ,IARCFT,IFLGHT,NWORDS', A16,I6,A6,I5,I6)
C 
C****** WRITE HEADER RECORD #2 ****** 
C     WRITE(LUE,93) 
C 93  FORMAT('WRITE HDR REC 2') 
      WRITE(33,ERR=1011,IOSTAT=IERR) ITMSEG 
C 
C****** CLOSE THE HEADER FILE ******
C     WRITE(LUE,94) 
C 94  FORMAT(' CLOSE HEADR FILE AFTER WRITE') 
      CLOSE (33,IOSTAT=IERR)
      RETURN
C 
C 
C****** COME HERE TO WRITE A DATA FILE RECORD ******
  400 CONTINUE
C
C****** THE DATA FILE SHOULD BE OPENED FOR EXCLUSIVE UPDATE ******
      IF (.NOT.IOPNFI) THEN 
c        OPEN(44,FILE=DAPDTA//NMDATA,IOSTAT=IERR,ERR=1012,
         call cname (dfile, 'XANDATA', NMDATA,LDF)
         OPEN(34,FILE=dfile(1:LDF),IOSTAT=IERR,ERR=1013,
     1     STATUS='OLD',ACCESS='DIRECT',RECL=(NWORDS*4),
     2     FORM='UNFORMATTED')
        IF (IERR.NE.0) GO TO 1014 
        write(0,'(" successfully opened data file ",(a))') dfile(1:LHF)
	REWIND(34)
        IOPNFI = .TRUE. 
      END IF
C 
C****** WRITE THE DESIRED RECORD ****** 
 410  CONTINUE
      WRITE(34,REC=IREC,IOSTAT=IERR)IHR,IMIN,ISEC,IMSEC,
     1 (VALUES(I),I=1,NVAR)
      IF(IERR.NE.0)GOTO 1015
      RETURN
C 
C 
C  COME HERE TO CLOSE THE FILE
C  
 500  CLOSE(34,IOSTAT=IERR) 
      IOPNFI=.FALSE.
      RETURN
CN
C 
C*****  CREATE A HEADER FILE ******** 
c600   OPEN(33,FILE=DAPDTA//NMHEAD,STATUS='NEW',IOSTAT=IERR,
 600  call cname (hfile, 'XANDATA', NMHEAD,LHF)
	write(6,'(" cname return hfile=",(a), ", lhf=",i5)')
     $   hfile, lhf
        write(6,'(" NMHEAD=", (a))') NMHEAD
       OPEN(33,FILE=hfile(1:LHF),STATUS='NEW',IOSTAT=IERR,
     1  ERR=1016,FORM='UNFORMATTED')
	write(6,'(" open file ",(a)," without error")') hfile(1:LHF)
      write(0,'(" NMHEAD now=",(a))') NMHEAD
       CLOSE(33,IOSTAT=IERR)
       RETURN 
C 
C 
C  ***  CREATE A DATA FILE ***********
 700    ICMND=3 
c     OPEN(44,FILE=DAPDTA//NMDATA,STATUS='NEW',IOSTAT=IERR,
        call cname (dfile, 'XANDATA', NMDATA,LDF)
 42     format(a)
        write(0, 42) 
     $   'specified output file '//dfile(1:LDF)//' in rdata'
        write(0,'(" name length=",i5, ", nwords=",i5)') ldf,NWORDS
        OPEN(34,FILE=dfile(1:LDF),STATUS='NEW',IOSTAT=IERR,
     1       RECL=(NWORDS*4),ERR=1017,ACCESS='DIRECT',
     2       FORM='UNFORMATTED')
 83     format ( 'created data file ',a, ', ierr=',i5,', nwords=',i6)
        write(6, 83) dfile(1:LDF),ierr,nwords
        RETURN
C 
C 
C****** COME HERE ON UNEXPECTED HEADER EOF ****** 
  999 CONTINUE
C 
C****** COME HERE ON ANY OTHER ERROR ****** 
1001  continue
      ibrnch=1
      goto 1000
1002  continue
      ibrnch=2
      goto 1000
1003  continue
      ibrnch=3
      goto 1000
1004  continue
      ibrnch=4
      goto 1000
1005  continue
      ibrnch=5
      goto 1000
1006  continue
      ibrnch=6
      goto 1000
1007  continue
      ibrnch=7
      goto 1000
1008  continue
      ibrnch=8
      goto 1000
1009  continue
      ibrnch=9
      goto 1000
1010  continue
      ibrnch=10
      goto 1000
1011  continue
      ibrnch=11
      goto 1000
1012  continue
      ibrnch=12
      goto 1000
1013  continue
      ibrnch=13
      goto 1000
1014  continue
      ibrnch=14
      goto 1000
1015  continue
      ibrnch=15
      goto 1000
1016  continue
      ibrnch=16
      goto 1000
1017  continue
      ibrnch=17
      goto 1000
1018  continue
      ibrnch=18
 1000 CONTINUE
 84   format (' ibrnch=',i3,', irec=', i5, ', ierr=', i5,
     $   ', ihr, imin, isec',3i5)
      write(6, 84) ibrnch,irec,IERR,ihr,imin,isec
      if (ibrnch.eq.16) ierr=117
      IF (LUE.GT.0) THEN
        WRITE (LUE,11000) IERR 
11000   FORMAT (' Error in subroutine FILE',2X,I4)
	write (6,11000) ierr
      END IF
      IF (IOPNFI) THEN
        CLOSE(34,IOSTAT=IERR)
        IOPNFI = .FALSE.
C       WRITE(LUE,3667)IERR 
C3667   FORMAT(' FILE: TERM IERR=',  I6)
      END IF
      RETURN
      END 
