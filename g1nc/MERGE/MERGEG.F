      PROGRAM TGMERG
C
C    * * * * * * * * * * * * * * * * * * * * * * * * *
C    *  Copyright 1993, University Corporation for   *
C    *  Atmospheric Research, All Rights Reserved.   *
C    * * * * * * * * * * * * * * * * * * * * * * * * *
C
C
C  Merge two or more GENPRO output data sets into one.
C  Output file name and full MSS bitfile names are specified via stdin.
C  Assume that only one data file exists on a bitfile.
C
C
      PARAMETER (LEN=10000)
      DIMENSION BUFF(LEN)
      CHARACTER*6 NAMEDFO
      CHARACTER*50 ISTRING,OSTRING
      CHARACTER*80 MSTRING,NAMEIN(30)
      LOGICAL HEADRD
C
      DATA IUNIT / 8 /
      DATA KUNIT / 9 /
      DATA IMODE / 1 /
      DATA ITYPE / 1 /
      DATA NDFI  / 0 /
      DATA I / 0 /
      DATA HEADRD / .FALSE. /
C
C ****  READ IN GENPRO DATA FILE INFORMATION
C NAMEIN    THE NAMES OF DATA FILES TO BE READ IN AND MERGED
C NAMEDFO   THE NAME OF DATA FILE TO BE WRITTEN
C ISTRING   IN THE FORM OF "assign -a input fort.8"
C OSTRING   IN THE FORM OF "assign -a OUTPUT fort.9"
C
      READ (5,10) NAMEDFO
   10 FORMAT (A6)
      WRITE (OSTRING,1203) NAMEDFO
 1203 FORMAT ('   assign -a ',a6,' fort.9')
      WRITE (6,1203) NAMEDFO
      IER = ISHELL(OSTRING)
      IF (IER .NE. 0) PRINT 9, IER
    9 FORMAT (' IER OF OSTRING = ', i4)
      IER = ISHELL('assign -V')
      IF (IER .NE. 0) PRINT 8,IER
    8 FORMAT (' IER OF assign -V = ', i4)
C
C  Get new input volume
  100 CONTINUE
      I = I + 1
      READ (5, 16) NAMEIN(I)
   16 FORMAT (A)
      IF (NAMEIN(I)(1:6).EQ.'ENDTAP') THEN
        ENDFILE (KUNIT)
        PRINT 101, NDFI,NAMEDFO,
     &            (NAMEIN(J),J=1,NDFI)
  101   FORMAT (I4,' Data files have been merged into file name ',A,/
     &  ' The input MSS bitfile names are: '/, (4X,A))
        STOP
      END IF
      NDFI = NDFI + 1
C
      IF (NDFI.GT.9) THEN
        WRITE (6,10300) NDFI
10300   FORMAT ("   Merge cannot handle"I6" or more input files.")
        STOP 00001
      END IF
      WRITE (MSTRING,1204) NDFI, NAMEIN(I)
 1204 FORMAT ('  msread input'I1' 'A)
C
C  Encode new volume to ISTRING
      WRITE (ISTRING,1202) NDFI
 1202 FORMAT ('  assign -a input'I1' fort.8')
C
C  Get the bitfile from MSS with msread
      IER = ISHELL(MSTRING)
      IF (IER .NE. 0) PRINT 29, IER
   29 FORMAT (' IER OF MSTRING = ', i4)
C
C  Assign the new volume to the unit
      IER = ISHELL(ISTRING)
      IF (IER .NE. 0) PRINT 19, IER
   19 FORMAT (' IER OF ISTRING = ', i4)
      IER = ISHELL('assign -V')
      IF (IER .NE. 0) PRINT 17, IER
   17 FORMAT (' IER OF assign -V = ', i4)
C      NREC=0
C
C  Skip header file (by reading each record) of each succeeding input bitfile
      IF (HEADRD) THEN
  333   CONTINUE
        BUFFER IN (IUNIT,IMODE)(BUFF(1),BUFF(LEN))
        LENREC = LENGTH(IUNIT)
        NSTATE = UNIT(IUNIT) + 1.0
C        CALL RDTAPE (IUNIT,IMODE,ITYPE,BUFF,LEN)
C        CALL IOWAIT (IUNIT,NSTATE,LENREC)
C  CHECK FOR EOF (NSTATE=1) OR ERROR (EOD?) (NSTATE=2)
C   If EOF ENCOUNTERED -- Read the data file which follows
        IF (NSTATE.EQ.1) THEN
          GO TO 1
C   If EOF/EOD ENCOUNTERED --  Close the unit and go on to the next data set
        ELSE IF (NSTATE.GT.1) THEN
          CLOSE(UNIT=IUNIT,IOSTAT=IOS,ERR=829)
C         WRITE (6,1201) IOS,IUNIT,NAMEIN(I)
          GO TO 100
        END IF
        GO TO 333
      END IF
C
C  Entry point for each record to be read
    1 CONTINUE
      BUFFER IN (IUNIT,IMODE)(BUFF(1),BUFF(LEN))
      LENREC = LENGTH(IUNIT)
      NSTATE = UNIT(IUNIT) + 1.0
C      CALL RDTAPE (IUNIT,IMODE,ITYPE,BUFF,LEN)
C      CALL IOWAIT (IUNIT,NSTATE,LENREC)
C     IF (NREC.LE.5) PRINT 3599, NREC,LEN,LENREC,NSTATE
C3599 FORMAT (' BUFFER IN:NREC,LEN, LENREC, NSTATE  --- ',4I8)
C
C
C  In this application, the first file will have the header.  For merging
C   more than one file, one wants to transfer the header and data from the
C   first data set but only the data from succeeding data sets.  There is
C   code above to skip the first file of subsequent data sets, so all I need
C   do here is copy succeeding data files as contiguous records.  If I detect
C   an EOF, it must be from the first data set, so I should copy the EOF to
C   output and continue on to read the data file coming next.
C
C  CHECK FOR EOF (NSTATE=1) OR ERROR (NSTATE=2) (EOD?)
C   If EOF ENCOUNTERED ON FIRST DATA SET --  Write EOF and continue reading
      IF (NSTATE.EQ.1 .AND. .NOT.HEADRD) THEN
        ENDFILE (KUNIT)
        HEADRD = .TRUE.
        GO TO 1
C   If EOF/EOD ENCOUNTERED --  Close the unit and go on to the next data set
      ELSE IF (NSTATE.GT.0) THEN
        CLOSE(UNIT=IUNIT,IOSTAT=IOS,ERR=829)
C       WRITE (6,1201) IOS,IUNIT,NAMEIN(I)
C1201   FORMAT (' VOLUME CLOSE,IOS= ',I4,' UNIT= ',I4,' NAME= ',A)
        GO TO 100
      ELSE 
C  Write the data out to unit 9
C        NREC = NREC + 1
C        CALL WRTAPE (KUNIT,IMODE,ITYPE,BUFF,LENREC)       
C        CALL IOWAIT (KUNIT,KSTATE,NSUS)     
        BUFFER OUT (KUNIT,IMODE)(BUFF(1),BUFF(LENREC))
        KSTATE = UNIT(KUNIT) + 1.0
C
C       IF (NREC.LE.5) PRINT 3588, NSUS,LENREC,KSTATE
C3588   FORMAT (' BUFFER OUT: NSUS,LENREC,KSTATE --- ',3I8)
C
C     TEST FOR I/O ERROR ENCOUNTERED DURING PREVIOUS WRITE
        IF (KSTATE.GT.0) THEN 
          PRINT 99, KSTATE
   99     FORMAT (' BUFFER OUT:  KSTATE ='I3)
        ELSE
          GO TO 1
        END IF
      END IF
  200 CONTINUE
      STOP 1
C
C 929 PRINT 928, KUNIT
C 928 FORMAT (' NWVL:  ERROR IN OPENING UNIT ',I5)
C     STOP
  829 PRINT 830, IOS, IUNIT
  830 FORMAT (' NWVL:  ERROR 'I5' IN CLOSING IUNIT ',I5)
      STOP 1
      END
