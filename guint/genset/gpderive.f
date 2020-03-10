      SUBROUTINE DERIVE                                                         
C                                                                               
C   Get list of all possible derived variable names as a function               
C   of variables already present. Separate Keywords into Keywords               
C   array.                                                                      
C   Use GPDERIVE DBASE as the lookup table.                                     
C                                                                               
      INCLUDE "gppdata.h"                                                       
      INTEGER  NMSORC                                                           
      PARAMETER (NMSORC=10)                                                     
      CHARACTER *8 SOURCE(5),DERVAR(DIMD13),STDVAR(NMSORC)                      
      CHARACTER *8 KEY                                                          
      CHARACTER *40 STRING                                                      
      LOGICAL ADDED                                                             
C list of default vars: present in every flight?                                
      DATA STDVAR/'QCXC','PSXC','TASX','DPXC','ATX','QCX','RHOX','AKX',         
     $ 'SSX','TTX'/                                                             
C  none added this pass / number derived vars added = # keywords = 0            
      ADDED=.FALSE.                                                             
C     WRITE(6,'(8(A8,1X))') (DERIV(J),J=1,NDERIV)                               
C     CALL PAUSER(' ... on entry to derive')                              
      NUMDER=0                                                                  
      NUMKEY=0                                                                  
C back to top of file / skip first two records                                  
    5 REWIND (33)                                                               
      READ (33,'(A80)')                                                         
      READ (33,'(A80)')                                                         
C get source vars, number of source vars, and the attendant derived var         
   10 READ(33,'(A40,1X,I1,1X,A8,1X,A7)',END=99)                                 
     $  STRING,NUMSOU,DERVAR(NUMDER+1),KEY                                      
C     WRITE(6,'('' Can I add '',A8,''?'')') DERVAR(NUMDER+1)                    
C search list of derived variables picked up so far to avoid duplicates         
      DO 123 J=1,NUMDER                                                         
       IF (DERVAR(NUMDER+1).EQ.DERVAR(J)) THEN                                  
C       WRITE(6,'('' No: already added'')')                                     
        GOTO 10                                                                 
       ENDIF                                                                    
  123 CONTINUE                                                                  
      DO 124 J=1,NUMKEY                                                         
       IF (DERVAR(NUMDER+1).EQ.KEYWRD(J)) THEN                                  
C       WRITE(6,'('' No: Keyword'')')                                           
        GOTO 10                                                                 
       ENDIF                                                                    
  124 CONTINUE                                                                  
C initialize indices used to parse source variables out of STRING               
      IGO=-7                                                                    
      IEND=0                                                                    
C initialize number of VALID source vars in this list                           
      NMSRC=0                                                                   
      DO 50 J=1,NUMSOU                                                          
C extract each of NUMSOU source vars from STRING                                
       IGO=IGO+8                                                                
       IEND=IEND+8                                                              
C if this source var in STDVAR, then it is automatically OK: don't save         
       DO 45 K=1,NMSORC                                                         
   45   IF(STRING(IGO:IEND).EQ.STDVAR(K)) GOTO 50                               
C ...else it is a VALID source var to be used in ensuing search                 
       NMSRC=NMSRC+1                                                            
       SOURCE(NMSRC)=STRING(IGO:IEND)                                           
   50 CONTINUE                                                                  
C search current lists of sampled and derived vars for all of NMSRC vars        
      DO 55 J=1,NMSRC                                                           
       CALL SERCH2(SOURCE(J),SDINAM,MODE2,NMODE2,INDX)                          
       IF (INDX.EQ.0) THEN                                                      
        CALL SERCH2(SOURCE(J),DERIV,MODE3,NMODE3,INDX)                          
        IF (INDX.EQ.0) THEN                                                     
C        WRITE(6,'('' No: all source vars not present (yet)'')')                
         GOTO 10                                                                
        ENDIF                                                                   
       ENDIF                                                                    
   55 CONTINUE                                                                  
C each of them is already present ==> check if a keyword or if                  
C this derived var should be offered                                            
      IF (KEY.EQ.'KEYWORD') THEN                                                
       NUMKEY=NUMKEY + 1                                                        
       KEYWRD(NUMKEY)=DERVAR(NUMDER+1)                                          
C      WRITE(6,'('' Add to keywords'')')                                        
      ELSE                                                                      
       NDERIV=NDERIV + 1                                                        
       NMODE3=NMODE3 + 1                                                        
       NUMDER=NUMDER+1                                                          
C check for out of bounds indexing                                              
       IF(NUMDER.GE.DIMD13.OR.NDERIV.GE.DIMD13.OR.NMODE3.GE.DIMD13)THEN         
        CALL PAUSER(' Dimension DIMD13 exceeded in DERIVE... Hit <r>')          
        STOP                                                                    
       ENDIF                                                                    
C      WRITE(6,'('' Add to derived: '',A8)') DERVAR(NUMDER)                     
       DERIV(NDERIV)=DERVAR(NUMDER)                                             
       MODE3(NMODE3)=NDERIV                                                     
      ENDIF                                                                     
C indicate a new one was added ==> one more pass thru entire file is            
C necessary to pick any derived vars dependent on this as one of its            
C source vars                                                                   
      ADDED=.TRUE.                                                              
      GOTO 10                                                                   
C EOF / one more pass if indicated                                              
  99  IF (ADDED) THEN                                                           
       ADDED=.FALSE.                                                            
C      WRITE(6,'('' One more pass'')')                                          
       GOTO 5                                                                   
      ENDIF                                                                     
C     WRITE(6,'(8(A8,1X))') (DERIV(J),J=1,NDERIV)                               
C     CALL PAUSER(' ... on exit from derive')                              
C     PAUSE 'DONE'                                                              
      CLOSE(33)                                                                 
      RETURN                                                                    
      END                                                                       
