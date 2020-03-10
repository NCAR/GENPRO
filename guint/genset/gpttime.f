      SUBROUTINE TTIME(DATE)
C
C  Given a CHARACTER*14 (minimum) string DATE, return the date and time
C  in 'mm/dd/yy hh:mm' format
C
C
      CHARACTER *23 DATTIM
      CHARACTER *3  MONTHS(12)
      CHARACTER *(*) DATE
      integer*4 systim,arry(9)
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     + 'OCT','NOV','DEC'/
C     CALL DATETM (DATTIM, TBEG,ETIME,ETCPU)
      call gmtime(systim,arry)
        WRITE(DATE,'(I2,''/'',i2,''/'',i2,1x,i2,'':'',i2)') arry(5)+1,
     +  arry(4),arry(6),arry(3),arry(2)
C     DO 100 J=1,12
C      IF (DATTIM(6:8).EQ.MONTHS(J)) THEN
C       WRITE(DATE,'(I2,''/'',A2,''/'',A2,A6)') J,DATTIM(10:11),
C    +  DATTIM(15:16),DATTIM(19:23)
C       GOTO 105
C     ENDIF
C 100 CONTINUE
C 105 CONTINUE
      RETURN
      END
