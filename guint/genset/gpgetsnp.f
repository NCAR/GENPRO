      SUBROUTINE GETSNP(ITIMES,FLUSH,INOP,TIMSEG,TMFLGS,NSEG)                   
      INCLUDE "gpifile.h"                                                       
      LOGICAL INOP                                                              
      INTEGER  ITIMES(6),TIMSEG,NSEG,FLUSH                                      
      DIMENSION TIMSEG(6,MAXSEG),TMFLGS(MAXSEG)                                 
C                                                                               
C  use to create the snapshots for input or output operations, if those         
C  snapshots are desired to be all of the same length without gaps              
C                                                                               
C ON ENTRY --                                                                   
C   ITIMES -- BEGIN/END TIME OF FLIGHT                                          
C   FLUSH  -- DESIRED SNAPSHOT PERIOD                                           
C   INOP  -- TRUE IF FOR INPUT OPERATION, FALSE OTHERWISE (OUTPUT)              
C ON EXIT                                                                       
C   TIMSEG -- ARRAY OF DESIGNATED SNAPSHOTS, ONE 6-WORD ROW FOR EACH            
C   TMFLGS -- ARRAY OF FLAGS DENOTING VALID ROWS IN TIMSEG                      
C   NSEG   -- NUMBER OF VALID ROWS IN TIMSEG                                    
C                                                                               
                                                                                
C     IMPLICIT INTEGER*2 (I-N)                                                  
      INTEGER*4 ISEX,ISTART,IEND                                                
C     WRITE(6,'('' Time of flight: '',6(A2,1X),/,'' requested snap: '',         
C    $ I5,/,'' Input op? '',L6)') TIMES,FLUSH,INOP                              
C   determine default snapshots from begin/end time interval                    
C    ...start by converting character array to integer:                         
C     READ  (TIMES,'(I2)') ITIMES                                               
C  initialize number of time segments and flag array                            
      NSEG=0                                                                    
      DO 100 J=1,MAXSEG                                                         
  100  TMFLGS(J)=0                                                              
C  set first begin snapshot to beginning flight time                            
      TIMSEG(1,1)=ITIMES(1)                                                     
      TIMSEG(2,1)=ITIMES(2)                                                     
      TIMSEG(3,1)=ITIMES(3)                                                     
C determine start and end time (in seconds) of flight                           
      CALL HMS2SX(ITIMES(1),ISEX)                                               
      CALL HMS2SX(ITIMES(4),IEND)                                               
C  adjust if flight flew before midnite until after midnite                     
      IF (IEND.LT.ISEX) IEND=IEND + 86400                                       
C indicate n'th interval is valid; determine remaining intervals                
  10  NSEG=NSEG+1                                                               
      TMFLGS(NSEG)=1                                                            
C add requested snapshot value to seconds                                       
      ISEX=ISEX+FLUSH                                                           
C if input operation, cutoff 1st segment at an 'even' place on clock            
      IF((INOP).AND.ISEX.LT.IEND) ISEX=ISEX - MOD(ISEX,FLUSH)                   
C  determine the n'th end snap, same as the n+1'th begin snap                   
      CALL SX2HMS(ISEX,TIMSEG(4,NSEG))                                          
      CALL SX2HMS(ISEX,TIMSEG(1,NSEG+1))                                        
C test for more snapshots needed                                                
      IF (ISEX.LT.IEND) THEN                                                    
C for input, always set 2 time segments, unless entire flight is less           
C time than requested snapshot period                                           
       IF (INOP) THEN                                                           
        NSEG=2                                                                  
        TMFLGS(2)=1                                                             
       ELSE                                                                     
        GOTO 10                                                                 
       ENDIF                                                                    
      ENDIF                                                                     
C  set last end snapshot to ending flight time for either input or output       
      TIMSEG(4,NSEG)=ITIMES(4)                                                  
      TIMSEG(5,NSEG)=ITIMES(5)                                                  
      TIMSEG(6,NSEG)=ITIMES(6)                                                  
      RETURN                                                                    
      END                                                                       
