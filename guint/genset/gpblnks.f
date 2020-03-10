      SUBROUTINE BLNKS(VIN,VOUT,IARRAY,NUM)                                     
C                                                                               
C   if the first NUM characters of IARRAY are not blanks, assign VIN            
C   to VOUT; else do nothing                                                    
C                                                                               
      CHARACTER*(*) IARRAY                                                      
      K=0                                                                       
      DO 1 J=1,NUM                                                              
      IF(IARRAY(J:J).NE.' ')GO TO 5                                             
 1    CONTINUE                                                                  
      RETURN                                                                    
    5 VOUT=VIN                                                                  
      RETURN                                                                    
      END                                                                       
