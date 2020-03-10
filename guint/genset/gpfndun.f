      SUBROUTINE FNDUN                                                          
C                                                                               
C                                                                               
C  FILL UNITS ARRAY WITH THE CORRECT VALUE CORRESPONDING TO THE VARIABLE        
C                                                                               
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gpio.h"                                                          
C                                                                               
C                                                                               
      CHARACTER*4 BLNK                                                          
      DATA BLNK/'    '/                                                         
C  search units dictionary for engineering vars' units                          
      DO 12 J=1,NMPOSS                                                          
       UNITS(J)=BLNK                                                            
       DO 11 K=1,NPVARS                                                         
        IF (VRPOSS(J).EQ.PRNAME(K)) THEN                                        
         UNITS(J)= 'cnts'                                                       
         IF(VRPOSS(J)(5:5).EQ.'C') THEN                                         
          IF (VRPOSS(J).EQ.'ASASC'.OR.VRPOSS(J).EQ.'FSSPC') THEN                
           UNITS(J)= 'n/cc'                                                     
          ELSE                                                                  
           UNITS(J)= 'n/l '                                                     
          ENDIF                                                                 
         ENDIF                                                                  
         GOTO 12                                                                
        ENDIF                                                                   
   11  CONTINUE                                                                 
       CALL SERCH(VRPOSS(J),VNAME,NUMBV,INDX)                                   
       IF(INDX.NE.0)THEN                                                        
        UNITS(J)=VUNIT(INDX)                                                    
       ENDIF                                                                    
 12   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
