      SUBROUTINE GETDIC                                                         
C                                                                               
C  fill up the variable names and units dictionaries                            
C  8/4/87  -- currently using GPVAR DBASE                                       
      CHARACTER *80 RECORD                                                      
      INCLUDE "gpifile.h"                                                     
      INCLUDE "gpio.h"                                                          
      REWIND (10)                                                               
      NUMBV=0                                                                   
      NMBUL9=0                                                                  
C                                                                               
C  read in database  ( GPVAR DBASE D )                                          
C                                                                               
    5  READ(10,100,END=500) RECORD                                              
C                                                                               
C  skip cell by cell vars but retain probe keywords                             
C                                                                               
 450   IF (RECORD(1:1).NE.' ') THEN                                             
C                                                                               
C  ASSUMPTION: 1st column empty ==> you got yourself a valid variable           
C                                                                               
        IF (RECORD(51:54).EQ.'ASAS'.OR.                                         
     $      RECORD(51:54).EQ.'FSSP'.OR.                                         
     $      RECORD(51:54).EQ.'X200'.OR.                                         
     $      RECORD(51:54).EQ.'X260'.OR.                                         
     $      RECORD(51:54).EQ.'Y200')    THEN                                    
C modify probe names into keywords                                              
         NUMBV=NUMBV + 1                                                        
         VNAME(NUMBV)=RECORD(51:54)//'A'                                        
         VUNIT(NUMBV)='cnts'                                                    
         NUMBV=NUMBV + 1                                                        
         VNAME(NUMBV)=RECORD(51:54)//'C'                                        
         IF(VNAME(NUMBV).EQ.'ASASC'.OR.VNAME(NUMBV).EQ.'FSSPC') THEN            
          VUNIT(NUMBV)='n/cc'                                                   
         ELSE                                                                   
          VUNIT(NUMBV)='n/l'                                                    
         ENDIF                                                                  
         IF (NUMBV.EQ.MAXNAM) GOTO 600                                          
        ELSE                                                                    
C ordinary variable                                                             
         NUMBV=NUMBV + 1                                                        
         IF (NUMBV.GT.MAXNAM) GOTO 600                                          
         VNAME(NUMBV)=RECORD(51:58)                                             
         VUNIT(NUMBV)=RECORD(42:45)                                             
C flag it if it is a Bulletin 9 variable                                        
         IF (RECORD(60:62).EQ.'STD') THEN                                       
          NMBUL9=NMBUL9 + 1                                                     
          IF (NMBUL9.GT.MAXNAM) GOTO 601                                        
          BULL9(NMBUL9)=NUMBV                                                   
         ENDIF                                                                  
        ENDIF                                                                   
       ENDIF                                                                    
       GOTO 5                                                                   
  500  CONTINUE                                                                 
C      PAUSE 'CHECK FOR DUPES: '                                                
C      DO 233 J=1,NUMBV                                                         
C       DO 233 K=J+1,NUMBV                                                      
C        IF (VNAME(J).EQ.VNAME(K)) WRITE(5,'(''DUPE: '',A8)') VNAME(J)          
C 233  CONTINUE                                                                 
       RETURN                                                                   
  600  CLOSE (10)                                                               
       WRITE(LUTO,'('' NUMBV exceeded MAXNAM in GETDIC'')')                     
       CALL PAUSER(' Hit <r> to continue... contact GUINT expert')              
       STOP                                                                     
  601  WRITE(LUTO,'('' NMBUL9 exceeded MAXNAM in GETDIC'')')                    
       CALL PAUSER(' Hit <r> to continue... contact GUINT expert')              
       STOP                                                                     
  100  FORMAT(A80)                                                              
       END                                                                      
