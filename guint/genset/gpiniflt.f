      SUBROUTINE INIFLT
C
C  do flight setups
C
      INTEGER ICODE,IVALUE
      LOGICAL FOUND
      INCLUDE "gpifile.h"
      INCLUDE "gpio.h"
C
C     CALL system("clear")
C
C open stdin to not use first character as print control.
      open (unit=LUTI,form='formatted')
C
      FOUND=.FALSE.
      CALL NEWFLT(FOUND)
C set booleans: flight setup done, but not saved
      FLTSET=.TRUE.
      FLTSAV=.FALSE.
C     CALL system("clear")
      RETURN
      END
C
      SUBROUTINE NEWFLT(FOUND)
      CHARACTER *5 FLTNUM
      CHARACTER *6 TAPNUM
      CHARACTER*90 STRING
      LOGICAL FOUND,GAPPED
      INCLUDE "gpio.h"
      INCLUDE "gpifile.h"
      LOGICAL ALL
      ALL=.FALSE.
      GAPPED=.FALSE.
C attempt to open SUMMARY file
      CALL OPNSUM
C  display choices from log entries file
C     CALL system("clear")
      REWIND(99)
C skip 1st record of project information
      READ(99,'(A80)') STRING(1:80)
C display 4 across screen, get user response
      J=0
      STRING=' '
 1215 READ(99,'(A6,10X,A5)',END=1116)  TAPNUM,FLTNUM
C skip tape volumes that are part of a segment created by a gap in time
      IF (FLTNUM(1:4).EQ.'TAPE') GOTO 1215
c     WRITE(STRING(J*20+1:J*20+5),'(A5)') FLTNUM
      J=J+1
      IF (J.EQ.4) THEN
       J=0
       WRITE(6,'('' '',A72)') STRING
       STRING=' '
      ENDIF
      GOTO 1215
 1116 IF (J.LT.4) WRITE(6,'('' '',A72)') STRING
      WRITE(LUTO,'(/'' Please type the word ALL here''/
     $              ''  or enter a flight number. '')')
      iflght = '     '
  401 READ(LUTI,'(A)',END=402) IFLGHT
C 401 READ(LUTI,'(A5)',END=402) IFLGHT
      if (iflght .eq. "") goto 402
      call lcaps(iflght,5)
      IF (IFLGHT.EQ.'all') THEN
       ALL=.TRUE.
C      CALL system("clear")
       WRITE(LUTO,'('' Setting up all flights .... '')')
      ENDIF
C  search SUMMARY file for IFLGHT
      REWIND(99)
C skip 1st record of project information
      READ(99,'(A5)') FLTNUM
    5 READ(99,'(A6,10X,A5)',END=999) TAPNO(1),FLTNUM
      GAPPED=.FALSE.
      IF (ALL) THEN
       IF (TAPNO(1)(1:4).NE.'COMP'.AND.
     $     TAPNO(1)(1:3).NE.'SEG') GOTO 5
      ELSE
       IF(FLTNUM.NE.IFLGHT) GOTO 5
      ENDIF
      FOUND=.TRUE.
      BACKSPACE(99)
      READ(99,'(A6,2X,3(I2),2X,A5,2X,3I2,1X,3(I2),2X,A40)')
     $ TAPNO(1),IDATEF,IFLGHT,ITIMEF,DESCRP
C set flight number and date in title
      CALL SETTIT
C set default snapshots from ITIMEF for input (.TRUE.) & output (.FALSE.)
      CALL GETSNP (ITIMEF,NSXIN,.TRUE.,ITMSEG,ITMFLG,INMSEG)
      CALL GETSNP (ITIMEF,NSXOUT,.FALSE.,OTMSEG,OTMFLG,ONMSEG)
      NUMVOL=1
C adjust TAPNO if needed to process multiple tapes for this flight
      IF (TAPNO(1)(1:4).EQ.'COMP'.OR.
     $    TAPNO(1)(1:3).EQ.'SEG') THEN
       READ(TAPNO(1)(5:6),'(BN,I2)') NUMVOL
C flag for add'l info if this is a gap segment
       IF (TAPNO(1)(1:3).EQ.'SEG') GAPPED=.TRUE.
C back up in file to first entry of that sequence
       DO 123 J=1,NUMVOL + 1
  123   BACKSPACE(99)
       DO 124 J=1,NUMVOL
        READ(99,'(A6)') TAPNO(J)
  124  CONTINUE
C get MS pathname info and suffix of first tape to acquire -- this also
C positions file properly if ALL is true: ready for next read at label 5
       READ(99,'(3X,A1,47X,A6,11X,A1)')
     $ GAP,OUTPUT,TAPE1
       IF(.NOT.(GAPPED)) THEN
C reset above if no gaps in flight
        GAP='0'
        TAPE1='0'
        OUTPUT='G'//TAPNO(1)(2:6)
       ENDIF
C      WRITE(6,19772) (TAPNO(J),J=1,NUMVOL)
19772  FORMAT('  Iniflt Tape Volume(s): ',/,
     $ (A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X))
      ELSE
C Tape number is not 'COMP' or 'SEG' ==> single tape to acquire
       GAP='0'
       TAPE1='0'
       OUTPUT='G'//TAPNO(1)(2:6)
      ENDIF
      IF (ALL) THEN
       CALL FLTGEN
       GOTO 5
      ENDIF
      CLOSE(99)
      RETURN
 999  IF (ALL) THEN
C      CALL PAUSER(' All flights now have default setup...hit <r>')
       CLOSE(99)
       RETURN
      ELSE
       WRITE(LUTO,'('' Flight # not found; retry or <cr> to exit'')')
      ENDIF
      GOTO 401
  402 continue
      FOUND=.FALSE.
      CLOSE(99)
      RETURN
      END
