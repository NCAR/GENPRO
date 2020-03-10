      SUBROUTINE SERCH(KEY,ARRAY,LEN,INDX)                                      
C     INTEGER*2 INDX,LEN                                                        
      CHARACTER *(*) KEY                                                        
      CHARACTER *(*) ARRAY (1)                                                  
C                                                                               
C search for KEY in ARRAY of length LEN; return its position in the arra        
C in INDX                                                                       
C                                                                               
      INDX=0                                                                    
      DO 5 J=1,LEN                                                              
       IF (ARRAY(J).EQ.KEY) THEN                                                
        INDX=J                                                                  
        RETURN                                                                  
       ENDIF                                                                    
    5 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
