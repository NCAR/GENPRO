                   SUBROUTINE TERPIT                                            
C------------------This terps variables back to their original rate             
C                  after their values have been mutated by CALIB.               
                                                                                
                                                                                
      include "gparc.h"
      include "gppms.h"
      include "gprat.h"
      include "gpder.h"
      include "gpcalc.h"
      include "gprad.h"
                                                                                
      INTEGER A,B,C,D,E,J,TOP,M,N
      CHARACTER*9 VNAME                                                         
      CHARACTER*80 LINE, ARLIN                                                  
      DIMENSION ARLIN(20),VNAME(600)                                            
                                                                                
                                                                                
 1010 FORMAT (A80)                                                              
 1012 FORMAT (1x,A80)                                       
 1011 FORMAT (1x,6A9)                                                  
      REWIND(25)                                                                
                                                                                
C-----------CLEARS LINES                                                        
      DO 10, J= 1,10                                                            
         READ(96,1010)LINE                                                      
         WRITE(97,1012)LINE                                                     
   10 CONTINUE                                                                  
      READ(96,1010) LINE                                                        
C------------REMVARS THE PROBES                                                 
 1018 FORMAT (' REMVAR = ',4(A8))                                           
      IF (PMNUM .GT. 0) THEN                                                    
       PMNAME(PMNUM)(8:8) = ' '                                                 
       WRITE(97,1018) (PMNAME(J),J=1,PMNUM)                                     
      ENDIF                                                                     
      DO 20, J=1,10                                                             
         READ(96,1010)LINE                                                      
   20 CONTINUE                                                                  
      WRITE(97,1012) LINE                                                       
      WRITE(97,1016)                                                            
 1016 FORMAT(' /Some of these rates may be unneccessary, but are include
     $d for')                                                                    
      WRITE(97,1017)                                                            
 1017 FORMAT (' / documentation purposes')                                
      WRITE(97,1014)                                                            
 1014 FORMAT(' LOWPAS= YES,%FOR,')                                      
                                                                                
      DO 30, J= 1,600                                                           
         VNAME(J)= '         '                                                  
   30 CONTINUE                                                                  
      TOP = 0                                                                   
                                                                                
C----------------Only terps raw variables, derived V's are set in CALIB         
      DO 40, J=1,TNUM                                                           
       DO 300, M=1,NUMVAR                                                       
        IF (TNAME(J) .EQ. NAME(M)) THEN                                         
         IF (TRATE(J) .LE. 19) THEN                                             
C----------------------------Should be low rate.                                
            TOP = TOP +1                                                        
            VNAME(TOP)(1:8) = NAME(M)                                           
            VNAME(TOP)(9:9) = ','                                               
            RATE(M) = 1                                                         
         ENDIF                                                                  
        ENDIF                                                                   
  300  CONTINUE                                                                 
   40 CONTINUE                                                                  
                                                                                
C----------VARIABLES WITH A PRIMARY INPUT ARE TERPED TO THAT INPUT'S            
C          RAW RATE (EX. TTB,ATB)                                               
                                                                                
      DO 41, J=1,NUMD                                                           
            IF (PRIM(J) .GT. 0) THEN                                            
            IF (PRIM(J) .GT. 5) PRINT *, 'PRIM ERROR'                           
       DO 301, M=1,NUMVAR                                                       
        IF (DNMVAR(J) .EQ. NAME(M)) THEN                                        
               DO 401, N= 1, TNUM                                               
                  IF (DTITLE(PRIM(J),J) .EQ. TNAME(N)) THEN                     
                     IF (TRATE(N) .LE. 19) THEN                                 
                        TOP = TOP +1                                            
                        VNAME(TOP)(1:8) = NAME(M)                               
                        VNAME(TOP)(9:9) = ','                                   
                        RATE(M) = 1                                             
                     ENDIF                                                      
                  ENDIF                                                         
 401           CONTINUE                                                         
        ENDIF                                                                   
  301  CONTINUE                                                                 
           ENDIF                                                                
   41 CONTINUE                                                                  
                                                                                
      DO 52, J=1,NUMVAR                                                         
         IF (NAME(J) .EQ. 'PALT    ') GOTO 53                                   
         IF (NAME(J) .EQ. 'PSFD2   ') GOTO 53                                   
         IF (NAME(J) .EQ. 'PSURF   ') GOTO 53                                   
         IF (NAME(J) .EQ. 'RHODT   ') GOTO 53                                   
         IF (NAME(J) .EQ. 'RHODB   ') GOTO 53                                   
         IF (NAME(J) .EQ. 'TVIR    ') GOTO 53                                   
         IF (NAME(J) .EQ. 'MR      ') GOTO 53                                   
         IF (NAME(J) .EQ. 'SPHUM   ') GOTO 53                                   
         IF (NAME(J) .EQ. 'RHUM    ') GOTO 53                                   
         IF (NAME(J) .EQ. 'DVALU   ') GOTO 53                                   
         GOTO 52                                                                
  53     TOP = TOP + 1                                                          
         VNAME(TOP)(1:8) = NAME(J)                                              
         VNAME(TOP)(9:9) = ','                                                  
         RATE(J) = 1                                                            
  52  CONTINUE                                                                  
                                                                                
                                                                                
      VNAME(TOP)(9:9) = ' '                                                     
      DO 50, J= 0, INT(FLOAT(TOP)/6.0)                                          
         N=(6*J)+1                                                              
         WRITE(97,1011)(VNAME(M),M=N,N+5)                                       
   50 CONTINUE                                                                  
                                                                                
                                                                                
      READ(96,1010)LINE                                                         
      WRITE(97,1012) LINE                                                       
      DO 57, J= 0, INT(FLOAT(TOP)/6.0)                                          
         N=(6*J)+1                                                              
         WRITE(97,1011)(VNAME(M),M=N,N+5)                                       
   57 CONTINUE                                                                  
      READ(96,1010)LINE                                                         
      READ(96,1010)LINE                                                         
                                                                                
      DO 60, J= 1,600                                                           
         VNAME(J)= '         '                                                  
   60 CONTINUE                                                                  
      TOP = 0                                                                   
                                                                                
      DO 70, J=1,TNUM                                                           
C-----------------SKIPS PROBES                                                  
       IF (TNAME(J)(1:4) .EQ. 'ASAS') GOTO 70                                   
       IF (TNAME(J)(1:4) .EQ. 'FSSP') GOTO 70                                   
       IF (TNAME(J)(1:4) .EQ. 'X260') GOTO 70                                   
       IF (TNAME(J)(1:4) .EQ. 'Y200') GOTO 70                                   
       IF (TNAME(J)(1:4) .EQ. 'X200') GOTO 70                                   
       DO 310, M=1,NUMVAR                                                       
        IF (TNAME(J) .EQ. NAME(M)) THEN                                         
         IF ((TRATE(J) .GE. 20).AND.(TRATE(J) .LE. 998)) THEN                   
            TOP = TOP +1                                                        
            VNAME(TOP)(1:8) = NAME(M)                                           
            VNAME(TOP)(9:9) = ','                                               
            RATE(M) = 20                                                        
         ENDIF                                                                  
        ENDIF                                                                   
  310  CONTINUE                                                                 
   70 CONTINUE                                                                  
                                                                                
      DO 54, J=1,NUMVAR                                                         
         IF (NAME(J) .EQ. 'XVI     ') GOTO 55                                   
         IF (NAME(J) .EQ. 'YVI     ') GOTO 55                                   
         GOTO 54                                                                
  55     TOP = TOP + 1                                                          
         VNAME(TOP)(1:8) = NAME(J)                                              
         VNAME(TOP)(9:9) = ','                                                  
         RATE(J) = 20                                                           
  54  CONTINUE                                                                  
                                                                                
      WRITE(97,1233)                                                            
 1233 FORMAT(' RATE=20,%FOR,')                                         
                                                                                
      VNAME(TOP)(9:9) = ' '                                                     
      DO 80, J= 0, INT(FLOAT(TOP)/6.0)                                          
         N=(6*J)+1                                                              
         WRITE(97,1011)(VNAME(M),M=N,N+5)                                       
   80 CONTINUE                                                                  
                                                                                
                                                                                
   95    READ(96,1010,END=90) LINE                                              
         WRITE(97,1012) LINE                                                    
         GOTO 95                                                                
                                                                                
   90 RETURN                                                                    
      END                                                                       
