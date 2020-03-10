      SUBROUTINE EXTEND(LU)                                                     
      INTEGER*4 LU                                                              
      CHARACTER * 1 DUMMY                                                       
C                                                                               
C  Position a file pointer for unit LU at end of file to extend the file.       
C  I/O with IBM FORTRAN is a little clumsy...to get to the end of a file        
C  (that may be empty) and extend it, here's the procedure:                     
C                                                                               
C  read until end                                                               
  101 READ(LU,'(A1)',END=6969) DUMMY                                            
      GOTO 101                                                                  
C  back up to just before EOF                                                   
 6969 BACKSPACE(LU)                                                             
C  if there is a record there, read it and land just before EOF; else           
C  EOF = BOF and the EOF will be read again; if so, backspace again to          
C  get just before the EOF                                                      
      READ(LU,'(A1)',END=6970) DUMMY                                            
      RETURN                                                                    
C back up one record to prepare for write in correct spot                       
 6970 BACKSPACE(LU)                                                             
      RETURN                                                                    
      END                                                                       
