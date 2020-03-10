      SUBROUTINE VERIF(DNAME,SNAM1)                                             
C                                                                               
C  if SNAM1 is present in SDINAM, add DNAME to the list of derived vars         
C                                                                               
      INCLUDE "gppdata.h"                                                       
      CHARACTER*(*) DNAME,SNAM1                                                 
      CALL SERCH(SNAM1,SDINAM,NFULL,INDX)                                       
      IF(INDX.EQ.0)GO TO 10                                                     
C  make sure no duplicates are added  (e.g., GUSTO can be added from            
C  from both AFIXR and ADIF)                                                    
      CALL SERCH(DNAME,SDINAM,NFULL,INDX)                                       
      IF (INDX.NE.0) GOTO 10                                                    
      NDERIV=NDERIV+1                                                           
      DERIV (NDERIV)=DNAME                                                      
      NMODE3=NMODE3+1                                                           
      MODE3(NMODE3)=NMODE3                                                      
  10  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
