      SUBROUTINE GETORD                                                         
C  Get the output order from the current lists in all output                    
C  arrays arranged as function of dictionary VNAME                              
C                                                                               
C                                                                               
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gppdata.h"                                                       
C  total found so far                                                           
      NMPOSS=0                                                                  
C  check each name in VNAME                                                     
      DO 900 JJ=1,NUMBV                                                         
       CALL SERCH2(VNAME(JJ),SDINAM,MODE1,NMODE1,INDX)                          
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH2 (VNAME(JJ),SDINAM,MODE2,NMODE2,INDX)                         
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH2 (VNAME(JJ),DERIV,MODE3,NMODE3,INDX)                          
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH2(VNAME(JJ),HSKNAM,HSKP,NHSKP,INDX)                            
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH2(VNAME(JJ),LORNAM,LORAN,NLORN,INDX)                           
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH (VNAME(JJ),PRNAME,NPVARS,INDX)                                
       IF (INDX.NE.0) GOTO 500                                                  
       CALL SERCH (VNAME(JJ),PMS1D,NPMS1D,INDX)                                 
       IF (INDX.EQ.0) GOTO 900                                                  
C  update pointer array                                                         
  500  NMPOSS=NMPOSS+1                                                          
       VRPOSS(NMPOSS)=VNAME(JJ)                                                 
  900  CONTINUE                                                                 
C cross-check the sampled vars for those without an index                       
C in GPVAR DBASE (Bulletin 9 )..output also these user-def'd vars               
       DO 300 J=1,NMODE2                                                        
        CALL SERCH(SDINAM(MODE2(J)),VNAME,NUMBV,INDX)                           
        IF (INDX.EQ.0) THEN                                                     
         NUMBV=NUMBV+1                                                          
         VNAME(NUMBV)=SDINAM(J)                                                 
         NMPOSS=NMPOSS+1                                                        
         VRPOSS(NMPOSS)=SDINAM(J)                                               
        ENDIF                                                                   
  300  CONTINUE                                                                 
C repeat for derived vars                                                       
       DO 400 J=1,NDERIV                                                        
        CALL SERCH(DERIV(J),VNAME,NUMBV,INDX)                                   
        IF (INDX.EQ.0) THEN                                                     
         NUMBV=NUMBV+1                                                          
         VNAME(NUMBV)=DERIV(J)                                                  
         NMPOSS=NMPOSS+1                                                        
         VRPOSS(NMPOSS)=DERIV(J)                                                
        ENDIF                                                                   
  400  CONTINUE                                                                 
       RETURN                                                                   
       END                                                                      
