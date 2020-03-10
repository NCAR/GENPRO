      SUBROUTINE TMSEG(BTIM,ETIM,ISTRNG)                                        
C                                                                               
C  add / modify time segments                                                   
C                                                                               
      INCLUDE "gpio.h"                                                          
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INTEGER INTERR                                                            
      COMMON/IOERR/INTERR                                                       
      DIMENSION BTIM(1),ETIM(1),NTIM(6)                                         
      CHARACTER *(*) ISTRNG                                                     
      REAL FLTGO, FLTEND                                                        
  501 CALL system("clear")                                                      
      WRITE(LUTO,9752)                                                          
 9752 FORMAT(                                                                   
     1 ' To change an interval, enter its number and new times',/,              
     1 ' To delete an interval, enter its number and a RETURN',/,               
     1 ' To terminate time entry, enter a RETURN or interval of 0',//,          
     1 ' Enter interval # and times as follows: ')                              
      WRITE(LUTO,9814)(J,(ITMSEG(I,J),I=1,6),J=1,INMSEG)                        
C Assuming screen 'length' of 22 rows                                           
      CALL LINFDS(14-INMSEG,LUTO)                                               
      WRITE(LUTO,9816)                                                          
 9816 FORMAT( ' N Start   End ',/, ' NN HHMMSS HHMMSS')                        
      READ(LUTI,'(A)',END=748)ISTRNG                                            
      if (istrng .eq. "") goto 748
      GOTO 749                                                                  
  748 continue                                                                  
      GOTO 510                                                                  
 9814 FORMAT(3X,I2,1X,3I2,1X,3I2)                                               
 9815 FORMAT(A2,1X,3I2,1X,3I2)                                                  
C  Decode line so interval number does not need to be right justified           
  749 IF(ISTRNG(1:2).EQ.'  ') GOTO 510                                          
11749 READ(ISTRNG(1:2),'(BN,I2)',ERR=520)INTRVL                                 
      IF(INTERR.NE.0) THEN                                                      
       WRITE(6,'('' Non-integer input; try again'')')                           
       INTERR=0                                                                 
       GOTO 11749                                                               
      ENDIF                                                                     
      IF(INTRVL.EQ.0)GO TO 510                                                  
      IF(INTRVL.GT.50 .OR. INTRVL.LT.1)INTRVL=INMSEG+1                          
C  Check that end time is greater than or equal to begin time                   
21749 READ(ISTRNG(4:9),'(3I2)')(NTIM(M),M=1,3)                                  
      IF(INTERR.NE.0) THEN                                                      
       WRITE(6,'('' Non-integer input; try again'')')                           
       INTERR=0                                                                 
       GOTO 21749                                                               
      ENDIF                                                                     
      BTIM(1)=NTIM(1)*3600.+NTIM(2)*60.+NTIM(3)                                 
31749 READ(ISTRNG(11:16),'(3I2)')(NTIM(M),M=4,6)                                
      IF(INTERR.NE.0) THEN                                                      
       WRITE(6,'('' Non-integer input; try again'')')                           
       INTERR=0                                                                 
       GOTO 31749                                                               
      ENDIF                                                                     
      ETIM(1)=NTIM(4)*3600.+NTIM(5)*60.+NTIM(6)                                 
      IF(ETIM(1).GE.BTIM(1))GO TO 503                                           
      WRITE(LUTO,9502)                                                          
 9502 FORMAT(' Begin time greater than end time. Please re-enter')              
      GO TO 501                                                                 
  503 IF(INTRVL.GE.INMSEG+1) THEN                                               
       INTRVL=INMSEG+1                                                          
       INMSEG=INMSEG+1                                                          
      ENDIF                                                                     
      DO 502 J=1,6                                                              
  502 ITMSEG(J,INTRVL)=NTIM(J)                                                  
      ITMFLG(INTRVL)=1                                                          
      IF(ETIM(1).EQ.0) THEN                                                     
       ITMFLG(INTRVL)=0                                                         
      ELSE                                                                      
C  new time segment ==> this setup needs to be saved                            
       FLTSAV=.FALSE.                                                           
      ENDIF                                                                     
      GO TO 501                                                                 
C  Error in interval number entry(non numeric)                                  
  520 WRITE(LUTO,9501)ISTRNG(1:3)                                               
 9501 FORMAT(' Illegal character in interval = ',A3,'  Please re-enter')        
      GO TO 501                                                                 
C                                                                               
C  Times are sorted into ascending order and overlapping periods are            
C     combined to produce the minimum set of intervals                          
C                                                                               
C Convert times to seconds                                                      
  510 K=0                                                                       
C     WRITE(LUTO,5149)(ITMFLG(J),J=1,50)                                        
C5149 FORMAT(25I2)                                                              
      DO 511 J=1,50                                                             
      IF(ITMFLG(J).EQ.0)GO TO 511                                               
      K=K+1                                                                     
      BTIM(K)=ITMSEG(1,J)*3600.+ITMSEG(2,J)*60.+ITMSEG(3,J)                     
      ETIM(K)=ITMSEG(4,J)*3600.+ITMSEG(5,J)*60.+ITMSEG(6,J)                     
  511 CONTINUE                                                                  
      INMSEG=1                                                                  
C     WRITE(LUTO,5148)K,(J,BTIM(J),ETIM(J),J=1,K)                               
C5148 FORMAT(' AFTER ST.511 K='I4/(I5,2F10.2))                                  
      IF(K.EQ.1)GO TO 521                                                       
C  Sort times into increasing order of begin times                              
      DO 512 M=1,K-1                                                            
      DO 512 J=M+1,K                                                            
      IF(BTIM(M).LE.BTIM(J))GO TO 512                                           
      TEMP=BTIM(M)                                                              
      BTIM(M)=BTIM(J)                                                           
      BTIM(J)=TEMP                                                              
      TEMP=ETIM(M)                                                              
      ETIM(M)=ETIM(J)                                                           
      ETIM(J)=TEMP                                                              
  512 CONTINUE                                                                  
C     WRITE(LUTO,5120)(BTIM(J),ETIM(J),J=1,K)                                   
C5120 FORMAT(' SORTED TIMES'/(2F10.3))                                          
C adjust begin/end times if less or greater than flight begin/end times         
      FLTGO=ITIMEF(1)*3600.+ITIMEF(2)*60.+ITIMEF(3)                             
      FLTEND=ITIMEF(4)*3600.+ITIMEF(5)*60.+ITIMEF(6)                            
      IF(BTIM(1).LT.FLTGO) BTIM(1)=FLTGO                                        
      IF(ETIM(K).GT.FLTEND) ETIM(K)=FLTEND                                      
C  Combine overlapping intervals                                                
      INMSEG=K                                                                  
      J=1                                                                       
 5130 J=J+1                                                                     
      IF (J.GT.K) GOTO 5139                                                     
      M=J-1                                                                     
C516  WRITE(LUTO,5131)J,BTIM(J),ETIM(J),M,BTIM(M),ETIM(M)                       
C5131 FORMAT(' BEFORE REARRANG'I5,2F10.3,I5,2F10.3)                             
      IF(BTIM(J)-ETIM(M).GE.0)GO TO 5130                                        
      IF(ETIM(J)-ETIM(M).LT.0)GO TO 515                                         
      ETIM(M)=ETIM(J)                                                           
C  Delete overlapped times and extra intervals                                  
 515  DO 518 L=J+1,K                                                            
      BTIM(L-1)=BTIM(L)                                                         
  518 ETIM(L-1)=ETIM(L)                                                         
      INMSEG=INMSEG-1                                                           
C     WRITE(LUTO,5132)INMSEG,(L,BTIM(L),ETIM(L),L=1,K)                          
C5132 FORMAT(' AFTER REARRANG, INMSEG='I5/(I5,2F10.3))                          
      J=J-1                                                                     
      K=K-1                                                                     
 513  GOTO 5130                                                                 
 5139 CONTINUE                                                                  
C     WRITE(LUTO,5151)(J,ITMFLG(J),BTIM(J),ETIM(J),J=1,K)                       
C5151 FORMAT(' MERGED TIME SEGMENTS'/(2I5,2F10.3))                              
C  Reproduce the times as hours, minutes and seconds in the time seg arr        
      DO 519 J=1,50                                                             
  519 ITMFLG(J)=0                                                               
      DO 514 M=1,INMSEG                                                         
      ITMSEG(1,M)=BTIM(M)/3600.                                                 
      ITMSEG(2,M)=(BTIM(M)-ITMSEG(1,M)*3600.)/60.                               
      ITMSEG(3,M)=BTIM(M)-ITMSEG(1,M)*3600.-ITMSEG(2,M)*60.                     
      ITMSEG(4,M)=ETIM(M)/3600.                                                 
      ITMSEG(5,M)=(ETIM(M)-ITMSEG(4,M)*3600.)/60.                               
      ITMSEG(6,M)=ETIM(M)-ITMSEG(4,M)*3600.-ITMSEG(5,M)*60.                     
      ITMFLG(M)=1                                                               
  514 CONTINUE                                                                  
 521  CALL system("clear")                                                      
      WRITE(LUTO,9813)                                                          
 9813 FORMAT(                                                                   
     1 ' Verify time segments; <cr>=OK, or enter correction:')                  
      WRITE(LUTO,9814)(J,(ITMSEG(I,J),I=1,6),J=1,INMSEG)                        
C Assuming screen 'length' of 22 rows                                           
      CALL LINFDS(18-INMSEG,LUTO)                                               
      WRITE(LUTO,9816)                                                          
      READ(LUTI,'(A)',END=29816) ISTRNG                                         
      if (istrng .eq. "") goto 29816
      GOTO 749                                                                  
29816 continue                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LINFDS(NUM,LU)                                                 
C write out linefeeds to standard output                                        
      DO 1 J=1,NUM                                                              
    1  WRITE(LU,'('' '')')                                                      
      RETURN                                                                    
      END                                                                       
