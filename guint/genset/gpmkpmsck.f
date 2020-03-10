                   SUBROUTINE PMSCHK                                            
C--------------This checks to see if PMS probes are being used, and             
C              sets needed values if they are.                                  
                                                                                
                                                                                
      include "gparc.h"
                                                                                
      CHARACTER*12 PROB(5)                                                      
      INTEGER J,K,L                                               
                                                                               
 1010 FORMAT(' /DEFFUN=1-NAMFUN,2-NSIZE ,3-LOGLIN,4-MGRY ,5-MINRY ,')           
 1011 FORMAT(' /       6-BOT,7-TOP ,8-LABX, 9-LABY')                            
 1012 FORMAT(' /')                                                              
 1013 FORMAT(' VECFUN=',5A12)                                                   
 1014 FORMAT(' ORDFUN=  NSIZE, LOGLIN, MGRY, MINRY, BOT, TOP')                  
 1015 FORMAT(' LETFUN= 1     , 2     , 0   ,  0   , 0. ,   0.0, %FOR,A',        
     &A4)                                                                       
 1016 FORMAT(' LETFUN= 1     , 2     , 0   ,  0   , 0. ,   0.0, %FOR,C',       
     &A4)                                                                       
 1017 FORMAT(' LABX  = ''  ',A4,' PROBE CELL SIZE IN MICROMETERS '' ,',       
     &'%FOR,A',A4)                                                              
 1018 FORMAT(' LABX  = ''  ',A4,' PROBE CELL SIZE IN MICROMETERS '' ,',      
     &'%FOR,C',A4)                                                              
 1019 FORMAT(' LABY  = '' ',A4,' CONCENTRATION (N/CC)            '',',        
     &'%FOR,C',A4)                                                              
 1020 FORMAT(' LABY  = '' ',A4,' CONCENTRATION (N/LTR)           '',',         
     &'%FOR,C',A4)                                                              
 1021 FORMAT(' LABY  = '' ',A4,' ACCUMULATION (COUNT)            '' ,',     
     &'%FOR,A',A4)                                      
                                                                                
      K=0                                                                       
      DO 110, J= 1, 5                                                           
         PROB(J) = '            '                                               
  110 CONTINUE                                                                  
                                                                                
      DO 100, J=1,NUMVAR                                                        
         IF (NAME(J)(1:5) .EQ. 'ASAS ') THEN                                    
            K=K+1                                                               
            PROB(K) = 'AASAS,CASAS,'                                            
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'FSSP') THEN                                     
            K=K+1                                                               
            PROB(K) = 'AFSSP,CFSSP,'                                            
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'X200') THEN                                     
            K=K+1                                                               
            PROB(K) = 'AX200,CX200,'                                            
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'Y200') THEN                                     
            K=K+1                                                               
            PROB(K) = 'AY200,CY200,'                                            
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'X260') THEN                                     
            K=K+1                                                               
            PROB(K) = 'AX260,CX260,'                                            
         ENDIF                                                                  
  100 CONTINUE                                                                  
      IF (K .EQ.  0) GOTO 500                                                   
      PROB(K)(12:12) = ' '                                                      
                                                                                
                                                                                
      WRITE(97,1010)                                                            
      WRITE(97,1011)                                                            
      WRITE(97,1012)                                                            
      WRITE(97,1013)(PROB(L),L=1,5)                                             
      WRITE(97,1012)                                                            
      WRITE(97,1014)                                                            
                                                                                
      DO 200, J= 1,K                                                            
         WRITE(97,1015)PROB(J)(2:5)                                             
         WRITE(97,1016)PROB(J)(2:5)                                             
  200 CONTINUE                                                                  
                                                                                
      WRITE(97,1012)                                                            
                                                                                
      DO 300,J=1,K                                                              
         WRITE(97,1017)PROB(J)(2:5),PROB(J)(2:5)                                
         WRITE(97,1018)PROB(J)(2:5),PROB(J)(2:5)                                
  300 CONTINUE                                                                  
                                                                                
      WRITE(97,1012)                                                            
                                                                                
      DO 400,J=1,K                                                              
         IF((PROB(J)(2:5) .EQ. 'ASAS') .OR. (PROB(J)(2:5) .EQ.                  
     &    'FSSP')) THEN                                                         
            WRITE(97,1019)PROB(J)(2:5),PROB(J)(2:5)                             
         ELSE                                                                   
            WRITE(97,1020)PROB(J)(2:5),PROB(J)(2:5)                             
         ENDIF                                                                  
         WRITE(97,1021)PROB(J)(2:5),PROB(J)(2:5)                                
  400 CONTINUE                                                                  
                                                                                
      WRITE(97,1012)                                                            
                                                                                
  500 RETURN                                                                    
      END                                                                       
