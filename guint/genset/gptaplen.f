      SUBROUTINE TAPLEN(NSXOUT)                                                 
C                                                                               
C  use information from Taplog file to determine the length in seconds          
C  of output snapshots needed for current project (NSXOUT)                      
C                                                                               
C     INCLUDE (GPIFILE)                                                         
      INTEGER DATSIZ,TBIT                                                       
      CHARACTER * 80 RECORD                                                     
C assumed length in feet of physical tapes, record gap in inches                
      PARAMETER (LENTAP = 2200 , RGAP = .35)                                    
C set default in case of exception                                              
      NSXOUT = 28800                                                            
C search taplog file for needed inputs                                          
    5 READ(51,'(A80)',END=95) RECORD                                            
      IF(INDEX(RECORD,'OUTPUT TAPE SPEC').EQ.0) GOTO 5                          
      READ(51,'(8X,I1,13X,I4)') NTRAK,NDEN                                      
C     WRITE(6,'('' tracks: '',I4,''  density: '',I4)') NTRAK,NDEN               
   15 READ(51,'(A80)',END=96) RECORD                                            
      IF(INDEX(RECORD,'DATA FILE INFORMATION').EQ.0) GOTO 15                    
      READ(51,'(61X,I6)') DATSIZ                                                
   25 READ(51,'(A80)',END=97) RECORD                                            
      IF(INDEX(RECORD,'Logical records per physical').EQ.0) GOTO 25             
      READ(RECORD,'(50X,I1)') LOGREC                                            
C     CALL PAUSER(' PROCEED W/CALCULATION')                                     
C proceed with calculation of output snapshot length                            
C total # bits in one inch of tape                                              
      NBIT=NDEN*(NTRAK-1)                                                       
C length of one data record in inches                                           
      LINREC=DATSIZ/NBIT + 1                                                    
C total # inches on physical tape                                               
      INTAP=LENTAP*12                                                           
C # of data records on LENTAP feet of tape                                      
      NRCTAP=INTAP/LINREC                                                       
C and therefore, the maximum # of seconds of data for physical tape             
      NSXOUT=NRCTAP*LOGREC                                                      
      RETURN                                                                    
   95 CALL PAUSER(' FATAL: # of tracks not found in Taplog .. hit <r>')      
      RETURN                                                                    
   96 CALL PAUSER(' FATAL: Physical record size not found ... hit <r>')         
      RETURN                                                                    
   97 CALL PAUSER(' FATAL: Logical record size not found ... hit <r>')       
      RETURN                                                                    
      END                                                                       
