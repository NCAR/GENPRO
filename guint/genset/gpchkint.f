      SUBROUTINE CHKINT(CH,ICODE,IVALUE)                                        
      CHARACTER *(*) CH                                                         
C                                                                               
C   ICODE:  1 ==> character   2 ==> integer  3 ==> invalid input                
C   IVALUE: return either EBCDIC value of ICHAR(CH(1:1)) if that char           
C           is a character; else return the value of an integer                 
C  assumed that CH is read with BN edit descriptor or that trailing             
C  blanks are to be ignored                                                     
      CHARACTER *16 FMT                                                         
C  assume for starters that its a character                                     
      ICODE=1                                                                   
      IVALUE=ICHAR(CH(1:1))                                                     
      IF (IVALUE.LT.239.OR.IVALUE.GT.249) RETURN                                
C  first char not char; check for valid integer input; trailing blanks OK       
      ICODE=2                                                                   
      DO 10 J=1,LEN(CH)                                                         
       IVALUE=ICHAR(CH(J:J))                                                    
       IF ((IVALUE.LT.239.OR.IVALUE.GT.249).AND.IVALUE.NE.64) THEN              
C error: started with integer and mixed in a character                          
        ICODE=3                                                                 
        RETURN                                                                  
       ENDIF                                                                    
   10 CONTINUE                                                                  
C  must be an OK integer; assign its value                                      
C determine format for integer write                                            
      WRITE(FMT,'(''(BN,I'',I3,'')'')') LEN(CH)                                 
      READ(CH,FMT) IVALUE                                                       
      RETURN                                                                    
      END                                                                       
