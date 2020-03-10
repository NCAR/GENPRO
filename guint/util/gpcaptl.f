      SUBROUTINE CAPTL(STRING,LEN)                                              
      CHARACTER * (*) STRING                                                    
C     INTEGER*2 LEN                                                             
C                                                                               
C convert all LEN characters of STRING to upper case  ... based on EBCDI        
C                                                                               
      DO 5 J=1,LEN                                                              
       IF (ICHAR(STRING(J:J)).LE.169.AND.ICHAR(STRING(J:J)).GE.129) THEN        
        STRING(J:J)=CHAR(ICHAR(STRING(J:J)) + 64)                               
       ENDIF                                                                    
    5 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
