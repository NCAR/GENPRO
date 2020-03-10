       SUBROUTINE CHGORD(OUTPTR,NUMOUT)                                         
C                                                                               
C  display and offer changes to the output order of variable names              
C                                                                               
C                                                                               
       INCLUDE "gpifile.h"                                                      
       INCLUDE "gpio.h"                                                         
       INTEGER  IDSIZ,ISTRT,IMAX,IMIN,INC,OUTPTR(MAXNAM),NUMOUT                 
       CHARACTER *15 CH,IFMT                                                    
       CHARACTER *8 VAR,alpha                                                   
       CHARACTER *80 STRING                                                     
C                                                                               
      CALL system("clear")                                                      
C
C open stdin to not use first character as print control.
      open (unit=LUTI,form='formatted')
C
      WRITE(LUTO,'(                                                             
     $''        WARNING: It is advised to be certain of all desired''/          
     $''        selections via Main Menu option SE before using this''          
     $/''        operation. Any changes made here will be lost if the''/        
     $''        SE operation is used again. ''//                                
     $''        To exit, enter any character; to proceed, enter <r>'')')        
      READ(LUTI,'(A1)',END=1) ALPHA                                             
      if (alpha .eq. "") goto 1
      RETURN                                                                    
    1 continue
C   1 REWIND(LUTI)                                                              
C  start with 1st element, list 18 rows                                         
C                                                                               
      ISTRT=1                                                                   
      IROWS=18                                                                  
C                                                                               
C  top of processing loop -- display IROWS elements starting with the           
C  ISTRT'th element to screen                                                   
C                                                                               
   10 CONTINUE                                                                  
      CALL system("clear")                                                      
C  get an INTEGER *4 argument for MIN0                                          
      IDSIZ=NUMOUT                                                              
      DO 50 J=ISTRT,MIN0(IDSIZ,ISTRT + IROWS - 1)                               
       WRITE(LUTO,110) J,VRPOSS(OUTPTR(J))                                      
  110  FORMAT(1x,I3,': ',A8)                                                       
   50 CONTINUE                                                                  
      WRITE(LUTO,'(/,'' Enter H for Help ...'')')                               
C                                                                               
C  clear input buffer and read next user input                                  
C                                                                               
   55 CH='               '                                                      
      READ(LUTI,120,END=200) CH                                                 
      if (ch .eq. "") goto 200
      call caps(ch,15)
  120 FORMAT(BN,A15)                                                            
C                                                                               
C  select action                                                                
C                                                                               
      IF (CH.EQ.'Q'.OR.CH.EQ.'S') THEN                                          
C                                                                               
C  Quit                                                                         
C                                                                               
       CALL system("clear")                                                     
       RETURN                                                                   
      ELSEIF (CH.EQ.'F') THEN                                                   
C                                                                               
C  go forward one screen's worth, back to top of loop                           
C                                                                               
       ISTRT=ISTRT + IROWS                                                      
       IF (ISTRT.GT.NUMOUT) ISTRT=1                                             
       GOTO 10                                                                  
      ELSEIF (CH.EQ.'B') THEN                                                   
C                                                                               
C   go backward one screen's worth, back to top of loop                         
C                                                                               
       ISTRT=ISTRT - IROWS                                                      
       IF (ISTRT.LT.1) ISTRT=NUMOUT - IROWS + 1                                 
       GOTO 10                                                                  
      ELSEIF (CH.EQ.'H') THEN                                                   
C                                                                               
C   Display Help screen                                                         
C                                                                               
       CALL system("clear")                                                     
       WRITE(LUTO, 969)                                                         
  969  FORMAT (/'                             COMMANDS'/                        
     $T10,'   B        go backward one screen                      '/           
     $T10,'   F        go forward one screen                       '/           
     $T10,'   H        help screen                                 '/           
     $T10,'   L        list to print file       '/                              
     $T10,'   Q        quit (ignore changes)                '/                  
     $T10,'   S        quit (save changes)                  '/                  
     $T10,'  <r>       forward 1 screen                            '/           
     $T10,'  x y       move the name in position x to the position    '/        
     $T10,'             immediately following name in postion y       '/        
     $T10,'             example: 8 4  -- 8th name to follow 4th name  '/        
     $T10,' x:y z      move names in positions x through y inclusive  '/        
     $T10,'             to the position immediately following name in '/        
     $T10,'             position z                                    '/        
     $T10,'             example: 8:12 2 -- names 8 thru 12 are to     '/        
     $T10,'             follow 2nd name                               '/        
     $T10,'                                                           '/        
     $T10,'   ****    to return to display, enter <r>    ****         '/        
     $)                                                                         
 7070  READ(LUTI,120,END=7071) CH                                               
       if(ch .eq. "") goto 7071
       GOTO 7070                                                                
 7071  continue                                                            
       GOTO 10                                                                  
      ELSEIF (CH.EQ.'L') THEN                                                   
C  list output vars to file                                                     
       CALL system("clear")                                                     
       WRITE(LUTO,'('' list order to file PRTFILE '',A3,''...'')')IPROJ         
       REWIND (10)                                                              
       DO 222 KK=1,NUMOUT                                                       
        REWIND(10)                                                              
  221   READ(10,'(A50,A8)',END=223) STRING,VAR                                  
        IF (VAR.EQ.VRPOSS(OUTPTR(KK))) THEN                                     
         WRITE(LUNPR,'(1x,A8,A50)')VAR,STRING                                 
         GOTO 222                                                               
        ENDIF                                                                   
        GOTO 221                                                                
  223   WRITE(LUNPR,'(1x,A8,42X,''UNKNOWN VARIABLE '')')
     $ VRPOSS(OUTPTR(KK))
  222 CONTINUE                                                                  
      GOTO 10                                                                   
      ELSEIF (INDEX(CH,':').GT.0) THEN                                          
C                                                                               
C  a range has been specified...parse the string to extract beginning           
C  and ending indices of names to be moved                                      
C                                                                               
       IPOS=INDEX(CH,':')                                                       
       WRITE (IFMT,251) IPOS - 1, INDEX(CH,' ') - IPOS - 1                      
  251  FORMAT('(I',I1,',X,I',I1,')')                                            
       READ(CH,IFMT) ICH1,ICH2                                                  
C It is assumed the input has ONE space between the two numbers                 
       IPOS=INDEX(CH,' ')                                                       
C  extract the target                                                           
       WRITE (IFMT,350) IPOS,15-IPOS                                            
  350  FORMAT('(',I2,'X,BN,I',I2,')')                                           
       READ(CH,IFMT) IPOS2                                                      
C error checks                                                                  
       IF (ICH1.GT.ICH2) GOTO 10                                                
       IF(ICH1.GT.MAXNAM.OR.ICH2.GT.MAXNAM.OR.IPOS2.GT.MAXNAM.OR.               
     $    ICH1.LT.1.OR.ICH2.LT.1.OR.IPOS2.LT.1) GOTO 10                         
       IF(IPOS2.GE.ICH1.AND.IPOS2.LE.ICH2) GOTO 10                              
C determine direction and number of moves (forward or backward)                 
       ISTART=ICH2                                                              
       IF (ICH2.LT.IPOS2) ISTART=ICH1                                           
       NMOVES=IABS(ICH1-ICH2) + 1                                               
       DO 360 M=1,NMOVES                                                        
  360    CALL REARNG(ISTART,IPOS2,OUTPTR)                                       
       GOTO 10                                                                  
      ELSE                                                                      
C                                                                               
C   else it is assumed that 2 integers are input to specify a move              
C   from position 1 to 2...if an alphanumeric character or invalid number       
C   is input, simply go back to top of loop                                     
C   The input 'x y' implies "move the x'th value to the position                
C   immediately following the y'th value"                                       
C                                                                               
C It is assumed the input has a space between the two numbers                   
       IPOS=INDEX(CH,' ')                                                       
C  extract the positions                                                        
       WRITE (IFMT,250) IPOS - 1,15-IPOS                                        
  250  FORMAT('(I',I1,',X,BN,I',I2,')')                                         
       READ(CH,IFMT) IPOS1,IPOS2                                                
C range error check                                                             
       IF(IPOS1.GT.MAXNAM.OR.IPOS2.GT.MAXNAM.OR.                                
     $    IPOS1.LT.1.OR.IPOS2.LT.1) GOTO 10                                     
C call the rearrangement routine                                                
       CALL REARNG(IPOS1,IPOS2,OUTPTR)                                          
       GOTO 10                                                                  
      ENDIF                                                                     
C                                                                               
C   Come here on a carriage return input (empty string ==> end of file          
C   on the terminal input device)...go forward one screen and get back          
C   to top of loop                                                              
C                                                                               
  200 CONTINUE                                                                  
      continue                                                              
      ISTRT=ISTRT + IROWS                                                       
      IF (ISTRT.GT.NUMOUT) ISTRT=1                                              
      GOTO 10                                                                   
      END                                                                       
      SUBROUTINE REARNG(IPOS1,IPOS2, OUTPTR)                                    
C                                                                               
C   Move the value in position IPOS1 to the position                            
C   immediately following IPOS2 in OUTPTR array                                 
                                                                                
       INCLUDE "gpdictn.h"                                                      
       INTEGER SAVNAM,OUTPTR(1)                                                 
C save the x'th value                                                           
       SAVNAM=OUTPTR(IPOS1)                                                     
C set up values for do loop                                                     
       IMIN=IPOS1                                                               
       IF (IPOS1.GT.IPOS2+1) THEN                                               
        IOFF=1                                                                  
        IMAX=IPOS2 + 2                                                          
        IOFF2=1                                                                 
       ELSEIF (IPOS2.GT.IPOS1) THEN                                             
        IOFF=-1                                                                 
        IMAX=IPOS2 - 1                                                          
        IOFF2=0                                                                 
       ELSE                                                                     
        RETURN                                                                  
       ENDIF                                                                    
       INC=-1*IOFF                                                              
C   already guaranteed at this point is that                                    
C  either IMIN >= IMAX and INC =1  -- OR -- IMAX >= IMIN and INC=-1             
C  which implies that at least one iteration should occur                       
       ITERNS=MAX(INT((IMAX-IMIN+INC)/INC),1)                                   
       K=IMIN                                                                   
  333  OUTPTR(K)=OUTPTR(K-IOFF)                                                 
       K=K+INC                                                                  
       ITERNS=ITERNS-1                                                          
       IF (ITERNS.GT.0) GOTO 333                                                
C replace x'th position with saved value(s)                                     
      OUTPTR(IPOS2+IOFF2)=SAVNAM                                                
      RETURN                                                                    
      END                                                                       
