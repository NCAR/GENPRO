C            PROGRAM GPFLTSUM --
C   use to add, delete, and modify entries in the SUMMARY file for given
C   project. For proper control, invoke only through GPGUINT EXEC with
C   the L (log flight tapes) option under the S (setup) menu.
C
C  AUTHOR -- Gary Horton (x1646, x 1051)
C
C user choice from main menu CHOICE
      CHARACTER*1 CHOICE
      character *80 arg2,fulpth,miscp
C flag for doing project setup or not
      CHARACTER*1 EXISTS
C common blocks
      include "gpfltcom.h"
C  calling program stacks info: if SUMMARY file already exists, no need
C  to get project information
      call GETARG(1,prnum)
      call getarg(2,arg2)
      call getarg(3,exists)
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C--------------------------------------start to open 37
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = prnum
      open (unit=37,file=fulpth,access='sequential',form='formatted')
C-----------------------start to open 38
      do 18, i = 1, 80
         fulpth(i:i) = ' '
 18   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summprt.'
      fulpth(lindex+10:lindex+12) = prnum
      open (unit=38,file=fulpth,access='sequential',form='formatted')
C-------------start to open 39
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/summchgd.'
      fulpth(lindex+11:lindex+13) = prnum
      open (unit=39,file=fulpth,access='sequential',form='formatted')
      call getenv ('fltsum',miscp)
      do 17, i = 1, 80
         fulpth(i:i) = ' '
 17   continue
      GOTPRO=.FALSE.
      IF (EXISTS.EQ.'Y') GOTPRO=.TRUE.
c      CALL system ("clear")
      CALL INIT
C   initial project setup
      IF (.NOT.(GOTPRO)) THEN
c       CALL system ("clear")
       CALL PROSET(1)
      ENDIF
C     CALL TRACE(1,'NMREC BEFORE MAIN MENU',NMREC)
C  main menu
1     CALL system ("clear")
      WRITE(6,'(''                    FLIGHT LOG MENU '',//)')
      WRITE(6,'(''               f   Examine File'',/,
     $          ''               e   Edit File'',/,
     $          ''               a   Add new entries to File'',/,
     $          ''               z   Delete entries from File'',/,
     $          ''               c   Save & continue'',/,
     $          ''               s   Save & exit'',/,
     $          ''               d   Discard changes & continue'',/,
     $          ''               q   Discard changes & exit'',/,
     $          ''                                           '',/,
     $          ''                 -save and continue often-'',//)')
      READ(5,'(A)',END=500) CHOICE
      if (choice .eq. "") goto 500
      call lcaps(choice,1)
      IF (CHOICE.EQ.'f') THEN
C examine file
       EXAMIN=.TRUE.
       CALL LOOK(.FALSE.,.TRUE.,1,NMREC)
       EXAMIN=.FALSE.
      ELSEIF (CHOICE.EQ.'e') THEN
C edit file
       CALL EDIT
      ELSEIF (CHOICE.EQ.'a') THEN
C add entries to file: NMREC is incremented
       CALL FLTSET(NMREC+1,1,.TRUE.)
      ELSEIF (CHOICE.EQ.'z') THEN
C  delete entry
       CALL DELETE
      ELSEIF (CHOICE.EQ.'c') THEN
C  save and continue
       CALL SAVE
      ELSEIF (CHOICE.EQ.'s') THEN
C  save and exit
       CALL SAVE
       CALL RXIT
      ELSEIF (CHOICE.EQ.'d') THEN
C discard changes and continue
       CALL INIT
      ELSEIF (CHOICE.EQ.'q') THEN
C discard changes and exit
       CALL RXIT
      ENDIF
      GOTO 1
 500  continue
      WRITE(6,'(//,'' Select: s save changes'',/,
     $             ''         q discard changes'',/,
     $             ''        <r> saves changes also'')')
      READ(5,'(A)',END=501) CHOICE
      call caps(choice,1)
      IF (CHOICE.ne.'Q') CALL SAVE
  501 continue
      CALL RXIT
      END
      BLOCK DATA FLTSUM
      include "gpfltcom.h"
      DATA X/' '/GOTPRO/.FALSE./EXAMIN/.FALSE./
C     DATA USERS/'HORTON','CELIA','RAFDMG'/
C     DATA NMACCT/'6979','6451','5027'/
      END
      SUBROUTINE LOOK(EDIT,PRJ,START,LAST)
C if EDIT, use record numbers to label each record;
C if PRJ, then display record of project info;
C display flight records starting from START through LAST
      include "gpfltcom.h"
      LOGICAL EDIT, PRJ
      INTEGER START,LAST
C user response to viewing more entries
      CHARACTER*1 MORE
C  top of current page/ size of each page/ last entry on current page
      INTEGER TOP,PGSIZ,PGLAST
      TOP=START
      PGSIZ=8
   1  CALL system ("clear")
      PGLAST=TOP+PGSIZ-1
      IF (EDIT) THEN
       WRITE(6,'(''      User'',7X,''RAT Latitude  Longitude'',
     $ ''  Sensors: ''
     $ ,''QC1, QC2, TTx, PSx, DPx'')')
       WRITE(6,'('' '',75(''-''))')
       WRITE(6,'(''  1: '',A8,3X,a3,1x,2(A9),5X,2(A3,1X),3(A8,1X),/)')
     $ PRJREC(1:8),prjrec(12:14),PRJREC(23:31),
     $ PRJREC(33:41),PRJREC(43:45),PRJREC(47:49),PRJREC(51:58),PRJREC
     $ (59:66),PRJREC(67:74)
       WRITE(6,'(''    Tape #     Date  Flt #   Start   End   '',
     $ ''Comment'')')
       WRITE(6,'('' '',75(''-''))')
       DO 100 J=TOP,MIN0(LAST,PGLAST)
        WRITE(6,'('' '',I2,'': '',A75)') J+1,FLTREC(J)
 100   CONTINUE
      ELSE
       IF (PRJ) THEN
       WRITE(6,'(''      User'',7X,''RAT Latitude  Longitude'',
     $ ''  Sensors: ''
     $ ,''QC1, QC2, TTx, PSx, DPx'')')
        WRITE(6,'('' '',75(''-''))')
        WRITE(6,'('' '',A8,3X,a3,1x,2(A9),5X,2(A3,1X),3(A8,1X),/)')
     $ PRJREC(1:8),prjrec(12:14),PRJREC(23:31),
     $  PRJREC(33:41),PRJREC(43:45),PRJREC(47:49),PRJREC(51:58),PRJREC
     $  (59:66),PRJREC(67:74)
       ENDIF
C exclude flight info if project only desired
       IF (LAST.NE.0) THEN
        WRITE(6,'('' Tape #   Date   Flt #  Start    End   Comment'')')
        WRITE(6,'('' '',75(''-''))')
        DO 200 J=TOP,MIN0(LAST,PGLAST)
         WRITE(6,'('' '',A75)') FLTREC(J)
 200    CONTINUE
       ENDIF
      ENDIF
C  determine if there's more left that won't fit on current screen
      IF(LAST.GT.PGLAST) THEN
       WRITE(6,'(//,'' More entries -- View next page (y/n) ? '')')
       READ(5,'(A)',END=300) MORE
 300   continue
       IF (MORE.EQ.'y') THEN
        TOP=PGLAST
        GOTO 1
       ENDIF
      ELSE
C give user a read to wait with, if in Examine mode
       IF (EXAMIN) THEN
        WRITE(6,'(//,''  hit return to continue...'')')
        READ(5,'(A)',END=10) J
   10   continue
       ENDIF
      ENDIF
      RETURN
      END
      SUBROUTINE EDIT

      include "gpfltcom.h"
   5  continue
C display one page at a time of project plus flight records
      CALL LOOK(.TRUE.,.TRUE.,1,NMREC)
      WRITE(6,'(/,'' Select # to modify or <r> to exit: '')')
C      READ(5,'(Bz,I2)',END=100) NUMREC
      READ(5,'(I2)',END=100) NUMREC
      if (numrec .eq. 0) goto 100
      IF(NUMREC.LT.1.OR.NUMREC.GT.NMREC+1) THEN
       WRITE(6,'(//,'' Choice out of range. Hit <r>, try again'')')
       READ(5,'(A)',END=5) J
       GOTO 5
      ENDIF
      IF (NUMREC.EQ.1) THEN
C  Change project entry
   10  continue
c       CALL system ("clear")
C display project info only
       CALL LOOK(.FALSE.,.TRUE.,0,0)
       WRITE(6,'(/,'' Select field -- <r> = exit'',//,
     $ ''      (1)  Redo entire entry'',/,
     $ ''      (2)  Change User name'',/,
     $ ''      (3)  Change Latitude'',/,
     $ ''      (4)  Change Longitude'',/,
     $ ''      (5)  Change Primary QC sensor'',/,
     $ ''      (6)  Change Secondary QC sensor'')')
       WRITE(6,'(
     $ ''      (7)  Change Temperature sensor'',/
     $ ''      (8)  Change Static Pressure sensor'',/
     $ ''      (9)  Change Dewpoint sensor'',/
     $ )')
       READ(5,'(Bz,I2)',END=5) IOPT
       if (iopt .eq. 0) goto 5
       IF(IOPT.LT.1.OR.IOPT.GT.9) THEN
        WRITE(6,'(//,'' Choice out of range. Hit <r>, try again'')')
        READ(5,'(A)',END=10) J
        GOTO 10
       ENDIF
       CALL PROSET(IOPT)
       GOTO 10
      ENDIF
C  Change flight entry
C adjust user requested record number since LOOK displays with offset
      NUMREC=NUMREC-1
   15 continue
      CALL system ("clear")
C display just the NUMREC'th flight record
      CALL LOOK(.FALSE.,.FALSE.,NUMREC,NUMREC)
      WRITE(6,'(/,'' Select field -- <r> = exit'',//,
     $ ''      (1)  Redo entire entry'',/,
     $ ''      (2)  Change Tape number'',/,
     $ ''      (3)  Change Date entry'',/,
     $ ''      (4)  Change Flight number'',/,
     $ ''      (5)  Change Start Time'',/,
     $ ''      (6)  Change End Time'',/,
     $ ''      (7)  Change Comment'')')
C not implemented yet:
C    $ ''      (8)  Indicate gap in flight'',//)')
      READ(5,'(Bz,I2)',END=5) IOPT
       if (iopt .eq. 0) goto 5
      IF(IOPT.LT.1.OR.IOPT.GT.7) THEN
       WRITE(6,'(//,'' Choice out of range. Hit <r>, try again'')')
       READ(5,'(A)',END=15) J
       GOTO 15
      ENDIF
C save current values of requested record
      READ(FLTREC(NUMREC),'(A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')
     $ TAPNO,IDATEF,IFLGHT,BEGIV,ENDIV,DESCRP
C FLTSET need not increment NUMREC
C -- also send IOPT + 1 to index correct Computed Goto value
      CALL FLTSET(NUMREC,IOPT,.FALSE.)
      GOTO 15
 100  continue
      RETURN
      end
      SUBROUTINE INIT
C initialize array and get file into it
      include "gpfltcom.h"
      NMREC=0
      PRJREC=' '
      DO 100 J=1,MAXREC
       FLTREC(J)=' '
  100 CONTINUE
      CALL GETFIL(37)
* read in database of scientist names and numbers
      RETURN
      END
      SUBROUTINE RXIT
      include "gpfltcom.h"
      CLOSE(37)
      STOP
      END
      SUBROUTINE DELETE
C
C  remove a flight entry from array FLTREC
C
      include "gpfltcom.h"
   15 continue
      CALL LOOK(.TRUE.,.TRUE.,1,NMREC)
      WRITE(6,'(/,'' Select # to delete'')')
      READ(5,'(Bz,I2)',END=500) NUMREC
      if (numrec .eq. 0) goto 500
      IF(NUMREC.LT.2.OR.NUMREC.GT.NMREC+1) THEN
       WRITE(6,'(//,'' Choice out of range. Hit <r>, try again'')')
       READ(5,'(A)',END=15) J
       GOTO 15
      ENDIF
      DO 100 J=NUMREC-1,NMREC
       IF(J.LT.NMREC) THEN
        FLTREC(J)=FLTREC(J+1)
       ELSE
        FLTREC(J)=' '
       ENDIF
  100 CONTINUE
      NMREC=NMREC-1
      GOTO 15
  500 continue
      RETURN
      END

      SUBROUTINE FLTSET(NREC,IOPT,INCR)
C  if INCR, add new entry RECNUM + 1 to flight records; else, modify
C  flight record RECNUM according to field selected by IOPT option,
C  which may specify modify entire record (IOPT=1)
C
      include "gpfltcom.h"
      INTEGER RECNUM,NREC
      LOGICAL INCR,SOLO,FULL
      character*10 temp
      SOLO=.FALSE.
      RECNUM=NREC
      IF(IOPT.NE.1) SOLO=.TRUE.
    5 FULL=.FALSE.
      GOTO (10,11,20,30,40,50,60,70) IOPT
   10 IF (INCR) THEN
       CALL system ("clear")
      ENDIF
   11 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      IF (INCR) WRITE(6,'(/,'' enter <r> to exit at any point --'',//)')
      WRITE(6,'(/,'' 6-character Tape Volume Number -- ex. V55898: '')')
      READ(5,'(A6)',END=1403) FLTREC(RECNUM)(1:6)
      if (fltrec(recnum)(1:6) .eq. "") goto 1403
      temp = ""
      temp(1:6) = fltrec(recnum)(1:6)
      call caps(temp,8)
      fltrec(recnum)(1:6) = temp(1:6)
      IF (SOLO) RETURN
   20 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      WRITE(6,'(/,'' Flight Date (mmddyy) -- example 031688:'')')
      READ(5,'(A6)',END=402) FLTREC(RECNUM)(9:14)
      if (fltrec(recnum)(9:14) .eq. "") goto 402
      IF (SOLO) RETURN
   30 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      WRITE(6,'(/,'' Flight #: enter 2-character prefix + 2 digits'',
     $'' + optional letter -- <r> to exit: '',//,
     $''             PREFIX CHOICES --'',/,
     $''      RF   -- for a research flight '',/,
     $''      FF   -- for a ferry flight '',/,
     $''      TF   -- for a test flight'',//,
     $'' examples: TF04  RF12B   FF01 ''//)')
      READ(5,'(A5)',END=402) FLTREC(RECNUM)(17:21)
      if (fltrec(recnum)(17:21) .eq. "") goto 402
      temp = ""
      temp(1:5) = fltrec(recnum)(17:21)
      call lcaps(temp,8)
      fltrec(recnum)(17:21) = temp(1:5)
      IF (SOLO) RETURN
   40 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      WRITE(6,'(/,'' Flight Begin time (hhmmss) -- '',/,
     $          ''    example 094503: '')')
      READ(5,'(A6)',END=402) FLTREC(RECNUM)(24:29)
      if (fltrec(recnum)(24:29) .eq. "") goto 402
      IF (SOLO) RETURN
   50 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      WRITE(6,'(/,'' Flight End time (hhmmss) -- '',/,
     $          ''    example 105530: '')')
      READ(5,'(A6)',END=402) FLTREC(RECNUM)(31:36)
      if (fltrec(recnum)(31:36) .eq. "") goto 402
      IF (SOLO) RETURN
   60 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
      WRITE(6,'(/,'' Enter 35-character comment -- <r> = flight '',
     $''number is the comment: '')')
      READ(5,'(A35)',END=401) FLTREC(RECNUM)(39:73)
      if (fltrec(recnum)(39:73) .eq. "") goto 401
      GOTO 1401
 401  continue
C on <r> for comment prompt, set comment to flight number
      FLTREC(RECNUM)(39:73)=FLTREC(RECNUM)(17:21)
1401  IF (SOLO) RETURN
C  indicate gap in flight (not implemented yet)
   70 CALL LOOK(.FALSE.,.FALSE.,RECNUM,RECNUM)
C flag that complete entry is done
      FULL=.TRUE.
      WRITE(6,'(//,'' hit <r> to continue'')')
      READ(5,'(A)',END=402) J
C loop for more entries and increment count if an add entry request
 402  continue
      IF (INCR) THEN
       NMREC=RECNUM
       RECNUM=RECNUM+1
C if all entries accounted for, get another one; else, exit
       IF (FULL) GOTO 5
      ENDIF
 1403 continue
      RETURN
      END
      SUBROUTINE SAVE
      include "gpfltcom.h"
      REWIND(37)
C  write new SUMMARY back to file
      WRITE(37,'(1x,A75)') PRJREC
      DO 500 J=1,NMREC
C      WRITE(6,'('' WRITE TO FILE # : '',I4,/,A75)') J,FLTREC(J)
C      WRITE(37,'(1x,A75)') FLTREC(J)
       WRITE(37,'(A75)') FLTREC(J)
  500 CONTINUE
C  write out a formatted version for project file purposes
      WRITE(38,'(9X,'' RESEARCH FLIGHT TAPES '',/,
     $ 14X,''PROJECT '',A3,//,
     $ '' NUMBER  FLIGHT  FLIGHT DATE  START    END'',/,
     $ '' ------  -----    --------    ------  ------ '')') PRNUM
      DO 501 J=1,NMREC
       READ(FLTREC(J),'(A6,2X,A6,2X,A5,2X,A6,1X,A6)')
     $ TAPNO,IDATEF,IFLGHT,BEGIV,ENDIV
       WRITE(38,'(1x,A6,2X,A5,4X,A2,2(''/'',A2),4X,A6,2X,A6)')
     $ TAPNO,IFLGHT,IDATEF(1:2),IDATEF(3:4),IDATEF(5:6),BEGIV,ENDIV
  501 CONTINUE
C create the flag file to indicate post-program processing is needed
C   (CMS implementation)
      WRITE(39,'('' CHANGES WERE MADE TO THE SUMMARY FILE'')')
      RETURN
      END



      SUBROUTINE PROSET (IOPT)
C initial project setup information
      include "gpfltcom.h"
C temporary user name
      CHARACTER *8 TMPNAM
      character *4 tmpsci
      character*3  tmprat
C FOUND is true if good user name entered
      LOGICAL FOUND,SOLO
      SOLO=.FALSE.
      IF (IOPT.NE.1) SOLO=.TRUE.
      CALL LOOK(.FALSE.,.TRUE.,0,0)
      GOTO (10,10,20,30,40,50,60,70,80) IOPT
   10 IF (.NOT.(SOLO)) WRITE(6,200)
  200 FORMAT(' Initial project setup; exit at any time with <r>',//)
      WRITE(6,
     $'('' Name of user who is processing project: '')')
      write (6,'('' Note: enter literally, case is important, most'',
     $/,'' logins will be lower case.'')')
  201 READ(5,'(A8)',END=1402) TMPNAM
      if (tmpnam .eq. "") goto 1402
      WRITE(6,430)
  430 FORMAT(' Turbulence Rate of Project (HRT or LRT): ')
      READ(5,'(A3)',END=402) tmprat
      if (tmprat .eq. "") goto 1402
      IF (SOLO)  RETURN
      call caps(tmprat,3)
      prjrec(12:14) = tmprat
C get user's account number
      WRITE(6,
     $'('' Scientist number of user who is processing '')')
      write (6,'('' Project: Note: This is the 4 digit number used '',
     $/,'' for Cray accounting.'')')
      read(5,'(a4)') tmpsci
      if (tmpsci .eq. "") goto 1402
      PRJREC(1:8)=TMPNAM
      PRJREC(18:21)=tmpsci
      IF (SOLO) RETURN
   20 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,510)
  510 FORMAT(' Initial Latitude (floating point -- Example 40.315): ')
      READ(5,'(A9)',END=402) PRJREC(23:31)
      if (prjrec(23:31) .eq. "") goto 402
      IF (SOLO)  RETURN
   30 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,515)
  515 FORMAT(' Initial Longitude (floating point): ')
      READ(5,'(A9)',END=402) PRJREC(33:41)
      if (prjrec(33:41) .eq. "") goto 402
      IF (SOLO) RETURN
   40 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,525)
  525 FORMAT(' Primary QC reference sensor ... Example QCR: ')
      READ(5,'(A3)',END=402) PRJREC(43:45)
      if (prjrec(43:45) .eq. "") goto 402
      call caps(prjrec(43:45),3)
      IF (SOLO) RETURN
   50 CALL LOOK(.FALSE.,.TRUE.,0,0)
      GOTPRO=.TRUE.
      WRITE(6,526)
  526 FORMAT(' Secondary QC reference sensor, <r> if none: ')
      READ(5,'(A3)',END=527) PRJREC(47:49)
      if (prjrec(47:49) .eq. "") goto 527
      call caps(prjrec(47:49),3)
      GOTO 528
  527 continue
C on <r> for 2nd QC sensor, set it equal to primary QC sensor
      IF (.NOT.(SOLO)) PRJREC(47:49)=PRJREC(43:45)
  528 IF (SOLO) RETURN
   60 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,625)
  625 FORMAT(' Temperature reference sensor ... Example TTRF: ')
      READ(5,'(A8)',END=402) PRJREC(51:58)
      if (prjrec(51:58) .eq. "") goto 402
      call caps(prjrec(51:58),8)
      IF (SOLO) RETURN
   70 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,725)
  725 FORMAT(' Static Pressure reference sensor ... Example PSFDC: ')
      READ(5,'(A8)',END=402) PRJREC(59:66)
      if (prjrec(59:66) .eq. "") goto 402
      call caps(prjrec(59:66),8)
      IF (SOLO) RETURN
   80 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,825)
  825 FORMAT(' Dewpoint reference sensor ... Example DPTC: ')
      READ(5,'(A8)',END=402) PRJREC(67:74)
      if (prjrec(67:74) .eq. "") goto 402
      call caps(prjrec(67:74),8)
      IF (SOLO) RETURN
 1000 CALL LOOK(.FALSE.,.TRUE.,0,0)
      WRITE(6,'(//,'' Hit <r> to continue'')')
      READ(5,'(A)',END=402) J
  402 continue
      RETURN
 1402 continue
      IF (.NOT.(GOTPRO)) STOP
      END
      SUBROUTINE GETFIL(IUNIT)
C
C  read file attached to IUNIT number into array FLTREC
C
      include "gpfltcom.h"
      REWIND(IUNIT)
      READ(IUNIT,'(A)',END=200) PRJREC
  100 READ(IUNIT,'(A)',END=200) FLTREC(NMREC+1)
      if (fltrec(nmrec+1) .eq. "") goto 200
C     CALL TRACE(1,'READ ONE MORE FLTREC; NMREC: ',NMREC)
      NMREC=NMREC+1
      GOTO 100
 200  CONTINUE
C     CALL TRACE(1,'DONE WITH READS; NMREC: ',NMREC)
      DO 300 K=NMREC+1,MAXREC
  300  FLTREC(K)=' '
      RETURN
      END
      SUBROUTINE TRACE(ITYPE,NAME,IVAL)
      CHARACTER *(*) NAME
      IF (ITYPE.EQ.1) WRITE(6,'('' '',A50,'' = '',I6)') NAME,IVAL
      PAUSE
      RETURN
      END
