      SUBROUTINE SELECT(IARRAY,IDBSIZ,IMODE,ISIZE)                              
      INCLUDE "gpdictn.h"                                                       
      INCLUDE "gpio.h"                                                          
C                                                                               
C  - supply list of all possible variables for selection                        
C  - allow user to specify which ones are desired                               
C                                                                               
C   VARIABLES ---                                                               
C                                                                               
C   IARRAY: incoming array of variable names                                    
C   IDBSIZ: total possible maximum # of names that can be selected              
C   DBASE:  workspace array getting the database                                
C   IMODE:  array of pointers to selected variables in IARRAY                   
C   ISIZE:  # of valid words in IMODE                                           
C   RECORD: single record read/written, various utility usage                   
C   CH:     user response                                                       
C   IFMT:   variable format used to parse a range input                         
C   FLAG:   flag on array element to de-select given variable name              
C   CHOSEN: flag on array element to select given variable name                 
C   IROWS:  number of rows to display to screen                                 
C   IGO, IEND: begin and end characters on each array element for flags         
C   ISTRT:  array element number with which to start next screen display        
C   ICOUNT: total number of variables selected at any given time                
C                                                                               
C   ASSUMPTIONS ---                                                             
C                                                                               
C    1: The incoming array IARRAY is a Character*8 array of no more than        
C       MAXNAM words.                                                           
C                                                                               
      INTEGER IDSIZ                                                             
      CHARACTER *16 DBASE(MAXNAM),RECORD                                        
      CHARACTER * 9 CH , IFMT                                                   
      CHARACTER * 8 IARRAY(1),ALPHA                                             
      CHARACTER * 7 FLAG, CHOSEN                                                
      INTEGER  IMODE(1),ISIZE                                                   
      DATA IROWS/18/IGO/10/IEND/12/FLAG/'   '/CHOSEN/'XXX'/                     
C                                                                               
C  read in database and set flags as function of IMODE array                    
C                                                                               
      DO 444 J=1,IDBSIZ                                                         
       DBASE(J)(1:8)=IARRAY(J)                                                  
       DBASE(J)(9:16)='       '                                                 
  444 CONTINUE                                                                  
C  ISIZE is # of valid words in IMODE, the array of pointers, on entry          
      DO 445 J=1,ISIZE                                                          
       DBASE(IMODE(J))(IGO:IEND)=CHOSEN                                         
  445 CONTINUE                                                                  
C                                                                               
C  start with 1st element                                                       
C                                                                               
      ISTRT=1                                                                   
C                                                                               
C  top of processing loop -- display IROWS elements starting with the           
C  ISTRT'th element to screen                                                   
C                                                                               
   10 CONTINUE                                                                  
      CALL system("clear")                                                      
C  get an INTEGER *4 argument for MIN0                                          
      IDSIZ=IDBSIZ                                                              
      DO 50 J=ISTRT,MIN0(IDSIZ,ISTRT + IROWS - 1)                               
       WRITE(LUTO,110) J,DBASE(J)                                               
  110  FORMAT(' ',I3,': ',A16)                                                  
   50 CONTINUE                                                                  
      WRITE(LUTO,'(/'' Enter H for help ...'')')                                
C                                                                               
C  clear input buffer and read next user input                                  
C                                                                               
   55 CH='         '                                                            
      READ(LUTI,120,END=200) CH                                                 
      if (ch .eq. "") goto 200
      call caps(ch,9)
  120 FORMAT(A9,BN)                                                             
C                                                                               
C  select action                                                                
C                                                                               
      IF (CH.EQ.'C') THEN                                                       
C  count number of currently selected variables                                 
C                                                                               
       ICOUNT=0                                                                 
       DO 255 K=1,IDBSIZ                                                        
        IF (DBASE(K)(IGO:IEND).EQ.CHOSEN) ICOUNT=ICOUNT + 1                     
  255  CONTINUE                                                                 
       WRITE(LUTO,2555) ICOUNT                                                  
 2555  FORMAT(' TOTAL SELECTED: ',I5)                                           
       GOTO 55                                                                  
      ELSEIF (CH.EQ.'S') THEN                                                   
       CALL system("clear")                                                     
       WRITE(LUTO,'(                                                            
     $''      WARNING: if you have already arranged the desired''/              
     $''      output order via Main Menu option OU, proceeding   ''/            
     $''      here will necessitate doing it again. It is advised''/            
     $''      to be certain of your selections here before arranging''/         
     $''      the output order. ''//                                            
     $''      To exit, enter any character; to proceed, enter <r>'')')          
      READ(LUTI,'(A1)',END=1) ALPHA                                             
      if (alpha .eq. "") goto 1
      CALL system("clear")                                                      
      RETURN                                                                    
    1 continue                                                                  
C  Relate DBASE to IARRAY, IMODE and ISIZE                                      
C                                                                               
       ISIZE=0                                                                  
  225  DO 150 K=1,IDBSIZ                                                        
        IF (DBASE(K)(IGO:IEND).EQ.CHOSEN) THEN                                  
         ISIZE=ISIZE+1                                                          
         IMODE(ISIZE)=K                                                         
        ENDIF                                                                   
  150  CONTINUE                                                                 
       CALL system("clear")                                                     
       RETURN                                                                   
      ELSEIF (CH.EQ.'Q') THEN                                                   
C                                                                               
C  Quit without saving anything                                                 
C                                                                               
       CALL system("clear")                                                     
       RETURN                                                                   
      ELSEIF (CH.EQ.'F') THEN                                                   
C                                                                               
C  go forward one screen's worth, back to top of loop                           
C                                                                               
       ISTRT=ISTRT + IROWS                                                      
       IF (ISTRT.GT.IDBSIZ) ISTRT=1                                             
       GOTO 10                                                                  
      ELSEIF (CH.EQ.'B') THEN                                                   
C                                                                               
C   go backward one screen's worth, back to top of loop                         
C                                                                               
       ISTRT=ISTRT - IROWS                                                      
       IF (ISTRT.LT.1) ISTRT=IDBSIZ - IROWS + 1                                 
       GOTO 10                                                                  
      ELSEIF (CH.EQ.'H') THEN                                                   
C                                                                               
C   Display Help screen                                                         
C                                                                               
       CALL system("clear")                                                     
       WRITE(LUTO, 969)                                                         
  969  FORMAT ('                             COMMANDS'//                        
     $T10,'   B        go backward one screen                      '/           
     $T10,'   C        display number of vars selected             '/           
     $T10,'   F        go forward one screen                       '/           
     $T10,'   H        help screen                                 '/           
     $T10,'   Q        quit without saving current list            '/           
     $T10,'   S        quit and save current list           '/                  
     $T10,'  <r>       forward 1 screen'/                                       
     $T10,'   n        for n''th name, select if unselected, vica versa'        
     $,/,T10,'             XXX after name means that name is selected'/         
     $T10,' n1:n2      for n1''th through n2''th names, select each '/          
     $T10,'             if unselected, vica versas...e.g., 32:35     '/         
     $T10,'             ... XXX after a name means it is selected   '/          
     $T10,/'   **** enter <r> to return to display  ****             '          
     $)                                                                         
 7070  READ(LUTI,120,END=7071) CH                                               
       if (ch .eq. "") goto 7071
       GOTO 7070                                                                
 7071  continue                                                                 
       GOTO 10                                                                  
      ELSEIF (INDEX(CH,':').GT.0) THEN                                          
C                                                                               
C  a range has been specified...parse the string to extract beginning           
C  and ending indices in array to be flagged                                    
C                                                                               
       IPOS=INDEX(CH,':')                                                       
       WRITE (IFMT,250) IPOS - 1, INDEX(CH,' ') - IPOS - 1                      
  250  FORMAT('(I',I1,',X,I',I1,')')                                            
       READ(CH,IFMT) ICH1,ICH2                                                  
C                                                                               
C   toggle current flag for each element in range specified, back to top        
C   ... if start of range is greater than end, nothing will happen              
C                                                                               
       DO 251 K=ICH1,ICH2                                                       
        IF (DBASE(K)(IGO:IEND).EQ.FLAG) THEN                                    
         DBASE(K)(IGO:IEND)=CHOSEN                                              
        ELSE                                                                    
         DBASE(K)(IGO:IEND)=FLAG                                                
        ENDIF                                                                   
  251  CONTINUE                                                                 
       GOTO 10                                                                  
      ELSE                                                                      
C                                                                               
C   else it is assumed that a single integer is input to specify that           
C   element for flagging...if an alphanumeric character or invalid number       
C   is input, simply go back to top of loop                                     
C                                                                               
       READ (CH,160,IOSTAT=IERR,ERR=55) ICH                                     
       IF (ICH.GT.IDBSIZ.OR.ICH.LT.1) GOTO 10                                   
  160  FORMAT (BN,I3,6X)                                                        
C                                                                               
C   toggle flag for that element and go back to top of loop                     
C                                                                               
       IF (DBASE(ICH)(IGO:IEND).NE.FLAG) THEN                                   
        DBASE(ICH)(IGO:IEND) = FLAG                                             
       ELSE                                                                     
        DBASE(ICH)(IGO:IEND) = CHOSEN                                           
       ENDIF                                                                    
       GOTO 10                                                                  
      ENDIF                                                                     
  200 CONTINUE                                                                  
C                                                                               
C   Come here on a carriage return input (empty string ==> end of file          
C   on the terminal input device)...go forward one screen and get back          
C   to top of loop                                                              
C                                                                               
      continue                                                                  
      ISTRT=ISTRT + IROWS                                                       
      IF (ISTRT.GT.IDBSIZ) ISTRT=1                                              
      GOTO 10                                                                   
      END                                                                       
