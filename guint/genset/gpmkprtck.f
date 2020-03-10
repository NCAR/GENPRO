
                   SUBROUTINE PRATCK                                            
C--------This Subroutine checks for PMS data coming in at high                  
C        rate, and modifies needed parameters if it is.                         
                                                                                
                                                                                
                                                                                
      include "gprad.h"
                                                                                
      INTEGER J, K, L                                              
                                                                                
 1111 FORMAT(' ORDGEN= MRATE',A1)                  
 1112 FORMAT(' LETGEN=   ',I4)                                       
                                                                                
      DO 100, J=1,TNUM                                                          
         IF ((TNAME(J) .EQ. 'FSSP    ').AND.(TRATE(J) .NE. 21))THEN             
            WRITE(97,1111)'F'                                                   
            K= INT(FLOAT(TRATE(J))/21.0)                                        
            WRITE(97,1112)K                                                     
         ENDIF                                                                  
         IF ((TNAME(J) .EQ. 'ASAS    ').AND.(TRATE(J) .NE. 21))THEN             
            WRITE(97,1111)'A'                                                   
            K= INT(FLOAT(TRATE(J))/21.0)                                        
            WRITE(97,1112)K                                                     
         ENDIF                                                                  
         IF ((TNAME(J) .EQ. 'X200    ').AND.(TRATE(J) .NE. 21))THEN             
            WRITE(97,1111)'X'                                                   
            K= INT(FLOAT(TRATE(J))/21.0)                                        
            WRITE(97,1112)K                                                     
         ENDIF                                                                  
         IF ((TNAME(J) .EQ. 'Y200    ').AND.(TRATE(J) .NE. 21))THEN             
            WRITE(97,1111)'Y'                                                   
            K= INT(FLOAT(TRATE(J))/21.0)                                        
            WRITE(97,1112)K                                                     
         ENDIF                                                                  
         IF ((TNAME(J) .EQ. 'X260    ').AND.(TRATE(J) .NE. 21))THEN             
            WRITE(97,1111)'6'                                                   
            K= INT(FLOAT(TRATE(J))/70.0)                                        
            WRITE(97,1112)K                                                     
         ENDIF                                                                  
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
