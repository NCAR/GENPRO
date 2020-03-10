      SUBROUTINE SERCH2 (KEY,ARRAY,IPTR,LEN,INDX)                               
      INTEGER INDX,LEN,IPTR(1)                                                  
      CHARACTER *(*) KEY                                                        
      CHARACTER *(*) ARRAY (1)                                                  
C     WRITE(6,'('' KEY: '',A8,'' ARRAY: '',/,9(A8,1X))') KEY,ARRAY              
C     WRITE(6,'('' IPTR: '',/,14(I4,1X))') IPTR                                 
C     WRITE(6,'('' LEN: '',I5,'' INDX: '',I5)') LEN,INDX                        
C     PAUSE                                                                     
C                                                                               
C search for KEY in ARRAY indirectly through pointer array IPTR; # of           
C words is LEN; return its position ,IPTR(J), in INDX                           
C                                                                               
      INDX=0                                                                    
      DO 5 J=1,LEN                                                              
       IF (ARRAY(IPTR(J)).EQ.KEY) THEN                                          
        INDX=IPTR(J)                                                            
        RETURN                                                                  
       ENDIF                                                                    
    5 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
