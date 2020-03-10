      SUBROUTINE HMS2SX(HMS,ISEX)                                               
      INTEGER HMS(3)                                                            
      INTEGER ISEX                                                              
C                                                                               
C  convert a 3-word integer array HMS from HHMMSS time format to total          
C  seconds, return in integer ISEX                                              
C                                                                               
      ISEX=0                                                                    
      ISEX=HMS(1) * 3600 + HMS(2) * 60 + HMS(3)                                 
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SX2HMS(ISEX,HMS)                                               
      INTEGER HMS(3)                                                            
      INTEGER ISEX                                                              
C                                                                               
C  convert integer ISEX from seconds to hours, minutes, seconds, return         
C  in HMS integer array                                                         
C                                                                               
      HMS(1)=MOD(ISEX/3600,24)                                                  
      HMS(2)=MOD(ISEX,3600)/60                                                  
      HMS(3)=MOD(ISEX,60)                                                       
      RETURN                                                                    
      END                                                                       
