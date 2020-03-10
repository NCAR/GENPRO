      SUBROUTINE caps(ARRY,KNT)
C
C  This is a renamed version of the routine CAPTL
C   written by Ron Ruth for QLDAP           911030
C
      CHARACTER *(*) ARRY 
      CHARACTER*1 CHAR
      DO 1 I=1,KNT
      KAR = ICHAR(ARRY(I:I)) 
C       WRITE(0,99)I,KNT,ARRY(I:I),KAR,KAR
C  99   FORMAT(' INPUT: '2I5,2XA1,2XA1,O8)
        IF (KAR.GE.97 .AND. KAR.LE.122) THEN 
          KAR=KAR-32
          ARRY(I:I)=CHAR(KAR)
C         WRITE(0,98)I,KNT,ARRY(I:I),KAR,KAR
C  98     FORMAT('OUTPUT: '2I5,2XA1,2XA1,O8)
        ENDIF 
   1  CONTINUE
      RETURN
      END 
