                   SUBROUTINE RDCAL(HGCAL)                                      
                                                                                
C This subroutine reads the variables directly from the CALCOE file             
C into the CNAME array.                                                         
                                                                                
      include "gpcalc.h"
                                                                                
      INTEGER I, J,HGCAL                                                 
      CHARACTER*62 DUMMY1                                                       
                                                                                
      HGCAL = 0                                                                 
      READ (35,2085)  DUMMY1                                                    
 2085 FORMAT (A62)                                                              
                                                                                
      DO 10 J = 1,600                                                           
        CNAME(J) = '        '                                                   
   10 CONTINUE                                                                  
      CNUMV= 1                                                                  
   20   READ(35,2095,END=30)CNAME(cnumv)            
 2095   FORMAT(68X,A8)                                                          
        IF (CNAME(cnumv) .EQ. 'HGM     ') HGCAL = 1      
        CNUMV = CNUMV + 1                                                       
        goto 20
   30 CONTINUE                                                                  
                                                                                
      CNAME(CNUMV) = 'HGM     '                                                 
      RETURN                                                                    
      END                                                                       
