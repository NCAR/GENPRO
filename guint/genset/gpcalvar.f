      SUBROUTINE CALVAR                                                         
C  output the current selection of calibration variables to the                 
C  DIRECT file                                                                  
C                                                                               
      INCLUDE "gpdictn.h"                                                       
C  write 'em in order as specified by user                                      
C                                                                               
C     WRITE(21) NMPOSS,(VRPOSS(J),J=1,NMPOSS)                                   
C  generate and write out list for calib vecvar                                 
      CALL CALVEC                                                               
C  write out list for print/stats/plot vecvars                                  
      WRITE(21) PSPOUT,(VRPOSS(PSPPTR(J)),J=1,PSPOUT)                           
C  write out list for comprehensive plot operation                              
      WRITE(21) PL2OUT,(VRPOSS(PL2PTR(J)),J=1,PL2OUT)                           
C                                                                               
      RETURN                                                                    
      END                                                                       
