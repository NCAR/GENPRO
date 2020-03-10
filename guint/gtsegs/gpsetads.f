C   GPSETADS -- user interface to set up JCL and run the GPADSRAW
C   program on Cray
C
C     Input parameters:  <project number> <workpath>
C
C     Input file:  unit 81 -- SUMMARY file
C     Output file: unit 80 -- insert file, to be combined with reference
C                              script to produce the job script file
      PARAMETER (MAXREC=600,MAXTAP=100)
      CHARACTER *80 SUMMRY(MAXREC)
      CHARACTER *8 USER
      CHARACTER *4 ACCNT
      CHARACTER *4 FLTNO,TRBRT
      CHARACTER *3 PROJNO,AIRCFT,paren, bksl, cparen
      CHARACTER *2 temp
      character*80 fulpth,arg2,miscp,lfmt1,lfmt2,lfmt3,lfmt4
      INTEGER YEAR
      DATA NUMSUM/0/
      DATA lfmt1 / "(' set MSSTAPE = (' 8(XA)')')" /
C                                      xx --> 20:21
      DATA lfmt2 / "(' set MSSFLT = ('10(XA)')')" /
C                                     xx --> 19:20
      DATA lfmt3 / "(16X 8(XA)'" /
      DATA lfmt4 / "(15X10(XA)'" /
      DATA paren  / " )'" /
C  Following backslash, for some reason, needs to be escaped.
      DATA bksl   / " \\'" /
      DATA cparen / " ) " /
C
C
C    * * * * * * * * * * * * * * *
C   *                             *
C  *  Executable code starts here  *
C   *                             *
C    * * * * * * * * * * * * * * *
C
C get project number, derive aircraft number
      call GETARG(1,projno)
C get working path
      call getarg(2,arg2)
C--------------------------------------------------------------- open 81
      fulpth = ' '
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
C--------- save string length for later
      larg2 = lindex
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = projno
      open (unit=81,file=fulpth,access='sequential',iostat=ierr,
     &  err=1000,form='formatted')
C--------------------------------------------------------------- open 80
      fulpth = ' '
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/gtsegs.'
      fulpth(lindex+9:lindex+11) = projno
      fulpth(lindex+12:lindex+14) = 'i'
      open (unit=80,file=fulpth,access='sequential',iostat=ierr,
     &  err=1020,form='formatted')
C
C get project number (derive aircraft number)
      IF(PROJNO(1:1).NE.'2') THEN
       AIRCFT='30'//PROJNO(1:1)
      ELSE
       AIRCFT='312'
      ENDIF
C get user name and turbulence rate and user accnt number
      READ(81,'(A8,3X,A3,3x,A4)') USER,TRBRT,ACCNT
C get project year (and rewind unit 81)
      CALL GTYEAR(YEAR,81)
C skip first record of summary file
      READ(81,'(A2)') temp
C read remaining records of summary file
    5 READ(81,'(A80)',END=100) SUMMRY(NUMSUM+1)
      call caps (SUMMRY(NUMSUM+1))
      NUMSUM=NUMSUM+1
      IF (NUMSUM+1.GE.MAXREC)
     &   CALL ERRMSG('gpsetads: Exceeded Maxrec in read')
      GO TO 5
C
  100 CONTINUE
C
C write out account number change statement
      WRITE (80,'(" newacct 41113",A3/)') AIRCFT
C write out command to start job accounting
      WRITE (80,'(" ja                    # start job accounting"/)')
      write(80,*) 'cat << "EOF" >! .ntwkparms'
      write(80,'(" SCI=",A4)')
     $ ACCNT
      write(80,'(" PROJ=41113",A3)')
     $ aircft
      write(80,*) '"EOF"'
C get source directory from environment variable
      call getenv ('gtsegs',miscp)
      lmiscp = index(miscp,' ')
C---------adjust lindex to the end of the string
      lmiscp = lmiscp -1
      WRITE (80,'(" set SOURCE = "A)') miscp(1:lmiscp)
      WRITE (80,'(" set DATA = "A)') arg2(1:larg2)
      WRITE (80,'(" set DEST = "A)') arg2(1:larg2)
      WRITE (80,'(" set PROGM = gpgtsegs")')
      WRITE (80,'(" set USER = "A)') USER
      WRITE (80,'(" set HOST = spock.atd.ucar.edu")')
      WRITE (80,'(" set DATIN1 = compflt."A3)') projno
      WRITE (80,'(" set DATOUT1 = segmnts."A3)') projno
      WRITE (80,'(" set LOGFILE = gtsegs."A3"log")') projno
      WRITE (80,'(" set MSSPATH = /RAF/19"I2"/"A3)') YEAR, PROJNO
C      WRITE (80,'(" @ numtapes = "I3)') NUMSUM 
C
C set tape numbers
C  assign default format string
        fulpth = lfmt1
C  I can get 8 volumes on one line
      IF (NUMSUM.GT.8) THEN
C       fulpth(1:26) = lfmt1(1:26)
        fulpth(27:32) = bksl // cparen
        WRITE (80,fulpth) (SUMMRY(I)(1:6),I=1,8)
        fulpth = lfmt3
        DO 200 I=9,NUMSUM,8
          IF (I+7.GE.NUMSUM) THEN
            fulpth(12:17) = paren // cparen
            WRITE (temp,'(I2)') NUMSUM-I+1
            fulpth(5:6) = temp
            WRITE (80,fulpth) (SUMMRY(J)(1:6),J=I,NUMSUM)
          ELSE
            fulpth(12:17) = bksl // cparen
            WRITE (80,fulpth) (SUMMRY(J)(1:6),J=I,I+7)
          END IF
  200   CONTINUE
      ELSE
        WRITE (temp,'(I2)') NUMSUM-I+1
        fulpth(20:21) = temp
        WRITE (80,fulpth) (SUMMRY(I)(1:6),I=1,NUMSUM)
      END IF
C
C set flight numbers
C  assign default format string
      fulpth = lfmt2
C  I can get 10 flights on a line
      IF (NUMSUM.GT.10) THEN
C       fulpth(1:25) = lfmt2
        fulpth(26:31) = bksl // cparen
c ** need upper case here!!***
        WRITE (80,fulpth) (SUMMRY(I)(17:21),I=1,10)
        fulpth = lfmt4
        DO 300 I=11,NUMSUM,10
          IF (I+9.GE.NUMSUM) THEN
            fulpth(12:17) = paren // cparen
            WRITE (temp,'(I2)') NUMSUM-I+1
            fulpth(5:6) = temp
            WRITE (80,fulpth) (SUMMRY(J)(17:21),J=I,NUMSUM)
          ELSE
            fulpth(12:17) = bksl // cparen
            WRITE (80,fulpth) (SUMMRY(J)(17:21),J=I,I+9)
          END IF
  300   CONTINUE
      ELSE
        WRITE (temp,'(I2)') NUMSUM-I+1
        fulpth(19:20) = temp
        WRITE (80,fulpth) (SUMMRY(I)(17:21),I=1,NUMSUM)
      END IF
C Done.
      CLOSE (80)
      CLOSE (81)
      STOP
 1000 CONTINUE
      CALL ERRMSG ('gpsetads: Cannot open summary file')
 1020 CONTINUE
      CALL ERRMSG ('gpsetads: Cannot open script output file')
      END
      SUBROUTINE ERRMSG(STRING)
      CHARACTER *(*) STRING
      WRITE(6,'('' '',A)') STRING
      CLOSE (80)
      CLOSE (81)
      STOP 1
      END
