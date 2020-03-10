      SUBROUTINE CHGCAL                                                         
C                                                                               
C change the CALCOE file based on info in SDINAM, C1, C2, and C3                
C ...at most MAXREX records may be processed from CALCOE file                   
C ...assumes maximum length of records in CALCOE is 80 bytes                    
C and that the first record in CALCOE is an 'ORDVAR' directive                  
C                                                                               
C  for each var in CALCOE file, locate the corresponding variable               
C  in array SDINAM and overwrite the CALCOE coefficients with                   
C  those in arrays C1, C2, and C3                                               
C                                                                               
      INTEGER MAXREX                                                            
      PARAMETER (MAXREX=250)                                                    
      INCLUDE "gpio.h"                                                          
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      CHARACTER*8 CALNAM                                                        
      CHARACTER *80 STRING(MAXREX)                                              
      LOGICAL *1 FLAG(DIMSDI)                                                   
C-------------start to open 16
      do 19, i = 1, 80
         fulpth(i:i) = ' '
 19   continue
      lindex = index(arg2,' ') -1
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calcoe.'
      fulpth(lindex+9:lindex+11) = iproj
      open (unit=16,file=fulpth,access='sequential')
C     OPEN(16,FILE='CALCOE',FORM='FORMATTED')                                   
      REWIND(16)                                                                
C                                                                               
C  initialize array that marks the vars in SDINAM that get written to           
C  CALCOE file                                                                  
      DO 1 J=1,DIMSDI                                                           
    1  FLAG(J)=.FALSE.                                                          
C  get past 1st record in CALCOE                                                
      READ(16,'(A6)')                                                           
      J=1                                                                       
C  J remembers how many vars are referenced in CALCOE                           
    5 READ (16,'(A80)',END=500) STRING(J)                                       
      J=J+1                                                                     
C  too many records?                                                            
      IF (J.GT.MAXREX) THEN                                                     
       CALL PAUSER(' Max. # records exceeded on CALCOE file...hit <r>')         
       GOTO 900                                                                 
      ENDIF                                                                     
      GOTO 5                                                                    
C get to 2nd record to start rewrite                                            
  500 REWIND(16)                                                                
      READ(16,'(A6)')                                                           
C  for every record in CALCOE:                                                  
      DO 200 N=1,J-1                                                            
C find the CALCOE var in the SDINAM array                                       
         CALNAM=STRING(N)(69:77)                                                
         DO 195 K=1,NFULL                                                       
          IF (CALNAM.EQ.SDINAM(K)) THEN                                         
C mark it found                                                                 
           FLAG(K)=.TRUE.                                                       
C write character values to real to facilitate comparisons                      
           READ (C1(K),'(F13.5)') CONE                                          
           READ (C2(K),'(F13.5)') CTWO                                          
           READ (C3(K),'(F13.5)') CTHREE                                        
C write this record back to CALCOE file if the                                  
C coefficients are not identity or all zeroes                                   
           IF  ((CONE.NE.0.0)                                                   
     $     .OR. (CTWO.NE.1.0).AND.(CTWO.NE.0.0)                                 
     $     .OR. (CTHREE.NE.0.0)) THEN                                           
            WRITE(16,150) C1(K),C2(K),C3(K),STRING(N)(52:68),CALNAM             
           ENDIF                                                                
  150      FORMAT(' LETVAR=3,[',A13,',',A13,',',A13,A17,A8)               
C back to top of loop                                                           
           GOTO 200                                                             
          ENDIF                                                                 
  195    CONTINUE                                                               
C var in CALCOE not found in SDINAM                                             
         WRITE(LUTO,198) CALNAM                                                 
  198    FORMAT(1x,A8,' is deleted from Calibrations file')              
  200 CONTINUE                                                                  
C check any vars in SDINAM not referenced in CALCOE with non-identity           
C coefficients                                                                  
      DO 201 K=1,NFULL                                                          
C write character values to real to facilitate comparisons                      
         READ (C1(K),'(F13.5)') CONE                                            
         READ (C2(K),'(F13.5)') CTWO                                            
         READ (C3(K),'(F13.5)') CTHREE                                          
         IF (.NOT.(FLAG(K))                                                     
     $     .AND. ((CONE.NE.0.0)                                                 
     $     .OR.  (CTWO.NE.1.0).AND.(CTWO.NE.0.0)                                
     $     .OR.  (CTHREE.NE.0.0))) THEN                                         
C  write these out also                                                         
          WRITE(16,150) C1(K),C2(K),C3(K),STRING(1)(52:68),SDINAM(K)            
          WRITE(LUTO,1198) SDINAM(K)                                            
 1198     FORMAT(1x,A8,' is added to Calibrations file')    
         ENDIF                                                                  
  201 CONTINUE                                                                  
  900 CLOSE(16)                                                                 
      RETURN                                                                    
      END                                                                       
