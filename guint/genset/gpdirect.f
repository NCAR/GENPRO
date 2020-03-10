      SUBROUTINE DIRECT                                                         
C  Direct downstream processing                                                 
C  EXEC is true only if execution is to continue;                               
C  if so: if calibrations have been changed, revise the CALCOE file.            
C  Also output the calibration variables and any other needed info              
C     IMPLICIT INTEGER*2 (I-N)                                                  
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gppdata.h"                                                       
C     INCLUDE "gpusrvar.h"                                                      
      OPEN(21,FORM='UNFORMATTED')                                               
      REWIND (21)                                                               
      REWIND (44)                                                               
C     WRITE(44,'('' &TRACE OFF'')')                                             
C  EXEC is true if processing is to continue                                    
      WRITE(21) EXEC                                                            
      IF (EXEC) THEN                                                            
C revise CALCOE file if necessary                                               
       IF (CALCHG) CALL CHGCAL                                                  
C output calibration variables names as selected by user                        
       CALL CALVAR                                                              
C primary sensors and attack angle sensors -- extra blanks included since       
C a 5-character variable used to be written instead of QCREF, which is 3.       
       WRITE(21) QCREF(1),'   ',PSX,TTX,DPX,AVANE,BVANE                       
C output pertinent info for project setup                                       
       WRITE(21) NMACCT,NMUSER,NMPROJ,IPROJ,IFLGHT,PRTITL,IARCFT,IVTITL,        
     $ IDATEF,ITIMEF,ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                 
     $ NSXIN,NSXOUT,                                                            
     $ INILAT,INILON,QCREF,                                                     
     $ NUMVOL,TAPNO,OUTPUT,GAP,TAPE1,TURBRT,                                    
     $ OUTCYC,STACYC,PRTCYC,PLTCYC,PL2CYC,                                      
     $ PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS,                        
     $ TAU1,TAU2,TAU3,DOF,BDIA,WIRET,FNUSS,REXP                                 
C output user-defined variables' names, units, titles, & derivations            
C      WRITE(21) NMUSRV,USRVEC                                                  
C output keywords                                                               
C      WRITE(21) NUMKEY, KEYWRD                                                 
C  setup a line in the stack to be read by GPGUINT, determining whether         
C  to continue with GPMKDECK or not                                             
C      WRITE(44,'('' &STACK PROCEED'')')                                        
C     ELSE                                                                      
C      WRITE(44,'('' &STACK STOP'')')                                           
      ENDIF                                                                     
C     CLOSE (21)                                                                
      RETURN                                                                    
      END                                                                       
