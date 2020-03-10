      SUBROUTINE PARM                                                           
C  Determine Defaults for Attack and Sideslip sensors                           
C                                                                               
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gppdata.h"                                                       
      DIMENSION PAIRS(2,5),TEMP(12),DPTS(2)                                     
      CHARACTER*8 PAIRS,TEMP,DPTS                                               
      DATA PAIRS/'PTMSF ','PSFD  ','QCW   ','PSW   ','QCF   ','PSF   ',         
     1           'QCG   ','PSB   ','QCB   ','PSB   '/                           
      DATA TEMP/'TTW   ','TTWH  ','TTF   ','TTFH  ','TTB   ','TTBH  ',          
     1          'ATRF  ','ATW   ','ATF   ','ATFH  ','ATB   ','ATBH  '/          
      DATA DPTS/'DPT   ','DPB   '/                                              
C     DO 3 J=1,5                                                                
C     CALL SERCH(PAIRS(1,J),SDINAM,NFULL,INDX)                                  
C     IF(INDX.NE.0)THEN                                                         
C           CALL SERCH(PAIRS(2,J),SDINAM,NFULL,INDX)                            
C           IF(INDX.NE.0) GO TO 6                                               
C           ENDIF                                                               
C  3  CONTINUE                                                                  
C     GO TO 50                                                                  
C  6  QCX=PAIRS(1,J)                                                            
C     PSX=PAIRS(2,J)                                                            
C     NDERIV=NDERIV+1                                                           
C     DERIV (NDERIV)='XMACH '                                                   
C     NMODE3=NMODE3+1                                                           
C     MODE3(NMODE3)=NDERIV                                                      
C 50  DO 55 J=1,12                                                              
C     CALL SERCH(TEMP(J),SDINAM,NFULL,INDX)                                     
C     IF(INDX.NE.0)GO TO 56                                                     
C 55  CONTINUE                                                                  
C     GO TO 100                                                                 
C 56  TTX=TEMP(J)                                                               
C     NDERIV=NDERIV+1                                                           
C     DERIV (NDERIV)='TASX  '                                                   
C     NMODE3=NMODE3+1                                                           
C     MODE3(NMODE3)=NDERIV                                                      
C  FIND INPUT TO ATTACK AND SSLIP                                               
 100  CONTINUE                                                                  
      AVANE='ADIFR'                                                             
      DO 101 J=1,NDERIV                                                         
       CALL SERCH('AKRD',DERIV,NDERIV,INDX)                                     
       IF(INDX.NE.0) THEN                                                       
        AVANE='AKRD'                                                            
        GO TO 102                                                               
       ENDIF                                                                    
 101  CONTINUE                                                                  
 102  BVANE='BDIFR'                                                             
      DO 104 J=1,NDERIV                                                         
       CALL SERCH('SSRD',DERIV,NDERIV,INDX)                                     
       IF(INDX.NE.0) THEN                                                       
        BVANE='SSRD'                                                            
        GO TO 105                                                               
       ENDIF                                                                    
 104  CONTINUE                                                                  
C  FIND INPUT TO DPX                                                            
 105  CONTINUE                                                                  
C     DO 110 J=1,2                                                              
C     CALL SERCH(DPTS(J),SDINAM,NFULL,INDX)                                     
C     IF(INDX.NE.0) GO TO 111                                                   
C110  CONTINUE                                                                  
C     RETURN                                                                    
C 111 DPX=DPTS(J)                                                               
      RETURN                                                                    
      END                                                                       
