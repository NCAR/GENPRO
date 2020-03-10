      SUBROUTINE RESPRO(IMODE)                                                  
C                                                                               
C   restore a project setup                                                     
C                                                                               
      INTEGER IMODE                                                             
C                                                                               
C  IMODE:  2 ==> restore SETSAV file, current format as per SR GPFILE           
C          3 ==> restore SETSAV file, old format as per option 0 in             
C                SR GPFILE; save it as current format; restore updated          
C                version                                                        
C                                                                               
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gpio.h"                                                          
      INCLUDE "gppdata.h"                                                       
    
      call inipro
C  start by restoring previous setup from disk                                  
      IF (IMODE.EQ.3) THEN                                                      
C restore old format first; if OK, save it as new format                        
       WRITE(LUTO,'(//'' Restore OLD format ...''//)')                          
       CALL FILE(0)                                                             
       IF (IERR.EQ.0) THEN                                                      
        WRITE(LUTO,'(//'' Save it as NEW format ...''//)')                      
C following statement is added in order to get BULL9 and NMBUL9 ready           
C for the 'new' SETSAV format (2/7/89). Once another change is made to          
C the SETSAV format OR there are no more pending projects with the 'old'        
C format (under option 0), the GETDIC call may be removed.  If left             
C alone, it is not really in the way. Do what you want. I don't care.           
        CALL GETDIC                                                             
        CALL FILE(3)                                                            
C now restore new format                                                        
        WRITE(LUTO,'(//'' Restore NEW format ...''//)')                         
        CALL FILE(1)                                                            
       ENDIF                                                                    
      ELSE                                                                      
       CALL FILE(1)                                                             
      ENDIF                                                                     
      IF(IERR.EQ.1) THEN                                                        
C  there are no previous setups; so, do an initial setup instead                
       WRITE(6,'(//,'' Proceeding with Initial Project Setup instead.'',        
     $ ''...'',//)')                                                            
       CALL INIPRO                                                              
       imode = 1
       RETURN                                                                   
      ELSEIF (IERR.EQ.2) THEN                                                   
       STOP                                                                     
      ENDIF                                                                     
C------the call to inipro means that changes to the various databases will show
C-------up in restores.  Changed 10/20/89 as per meeting
      call derive
      RETURN                                                                    
      END                                                                       
