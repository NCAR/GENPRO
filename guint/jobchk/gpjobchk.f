C  extract relevant information from a Genpro logfile                           
C                                                                               
C                                                                               
C                                                                               
C                                                                               
      CHARACTER *133 RECORD                                                     
      CHARACTER *65 PRNAME,TAPNO                                                
      CHARACTER *15 INBYTE                                                      
      CHARACTER * 6 INPUT                                                       
      character*3 name
      character*8 name2,flt
      character*80 arg2,fulpth
      LOGICAL OUTPUT                                                            

      call getarg(1,name)
      call getarg(2,flt)
      call getarg(3,arg2)
      call getarg(4,name2)
C--------------------------------------start to open 7
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      lindex = lindex + 1
      fulpth(lindex:lindex) = '/'
      fulpth(lindex+1:lindex+3) = name
      fulpth(lindex+4:lindex+4) = '.'
      lindex = lindex+4 
      lldex = index(flt,' ') -1
      fulpth(lindex+1:lindex+lldex) = flt(1:lldex)
      fulpth(lindex+1+lldex:lindex+3+lldex) = 'log'
      open (unit=7,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C--------------------------------------start to open 8
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      lindex = lindex +1
      fulpth(lindex:lindex) = '/'
      lldex = index(name2,' ') -1
      fulpth(lindex+1:lindex+lldex) = name2(1:lldex)
      fulpth(lindex+1+lldex:lindex+7+lldex) = '.jobchk'
      open (unit=8,file=fulpth,access='sequential',
     $          form='formatted')

      OUTPUT=.TRUE.                                                             
      REWIND(7)                                                                 
      CALL EXTEND (8)                                                           
      WRITE(6,'('' Extracting info...'')')                                      
    5 READ(7,'(A133)',END=105) RECORD                                           
      IF (INDEX(RECORD,'PROJECT=').NE.0) THEN                                   
       PRNAME=RECORD(1:65)                                                      
      ELSE                                                                      
       GOTO 5                                                                   
      ENDIF                                                                     
      WRITE(6,'('' Got Project Name..'')')                                      
    6 READ(7,'(A133)',END=106) RECORD                                           
      IF (INDEX(RECORD,'TIME  INFORMATION  *****').NE.0) THEN                   
    7  WRITE(8,'(1x,A133)') RECORD            
       READ(7,'(A133)',END=107) RECORD                                          
       IF (INDEX(RECORD,'DRIVER').EQ.0) GOTO 7                                  
      ELSE                                                                      
       GOTO 6                                                                   
      ENDIF                                                                     
      WRITE(6,'('' Got Time information..'')')                                  
      WRITE(8,'(1x,A133)') PRNAME              
C     WRITE(8,'(A133)') TAPNO                                                   
C  if output operation done, extract flushp for that operation                  
  20  READ(7,'(A133)',END=190) RECORD                                           
      IF (INDEX(RECORD,'OUTPUT  FLUSHP =').NE.0) THEN                           
       WRITE(8,'(1x,A133)') RECORD                 
  21   READ(7,'(A133)',END=192) RECORD                                          
C  look for next flush period description or end of that section                
       IF (INDEX(RECORD,'FLUSHP =').EQ.0.AND.                                   
     $     INDEX(RECORD,'(DRIVER)').EQ.0)  THEN                                 
        WRITE(8,'(1x,A133)') RECORD            
        GOTO 21                                                                 
       ENDIF                                                                    
      ELSE                                                                      
       GOTO 20                                                                  
      ENDIF                                                                     
      WRITE(6,'('' Got Output Flush period ..'')')                              
   10 READ(7,'(A133)',END=110) RECORD                                           
      IF (INDEX(RECORD,'SUMMARY OF SCALING ERRORS').NE.0) THEN                  
   15  WRITE(8,'(1x,A133)') RECORD  
       READ(7,'(A133)',END=115) RECORD                                          
       IF (INDEX(RECORD,'PROJECT =').EQ.0.AND.                                  
     $     INDEX(RECORD,'(DRIVER)').EQ.0.AND.                                   
     $     INDEX(RECORD,'HEADER WRITTEN').EQ.0.AND.                             
     $     INDEX(RECORD,'NO SCALING ERRORS').EQ.0) GOTO 15                      
       IF (INDEX(RECORD,'NO SCALING ERROR').NE.0)WRITE(8,'(1x,A133)')
     $     RECORD 
      ENDIF                                                                     
      GOTO 10                                                                   
   26 WRITE(6,'('' No more Scaling Error summaries'')')                         
C look for message concerning success                                           
   27 READ(7,'(1x,A133)',END=120) RECORD     
      IF (INDEX(RECORD,'SUCCESSFUL').NE.0) THEN                                 
       WRITE(8,'(1x,A133)') RECORD     
      ELSE                                                                      
       GOTO 27                                                                  
      ENDIF                                                                     
      WRITE(6,'('' Successful run..'')')                                        
C search for multiple tape inputs, until 'ASSIGN' verb found                    
   25 READ(7,'(A133)',END=160) RECORD                                           
      IF(INDEX(RECORD,'ASSIGN,DN=VOLUME1').NE.0) GOTO 28                        
C  assuming input tape volumes are prefixed with 'V'                            
      IF (((INDEX(RECORD,'BYTES READ TO V').NE.0).OR.      
     $(INDEX(RECORD,'BYTES READ TO R').NE.0))                                 
     $.AND.INDEX(RECORD,'MS000').NE.0)  THEN                     
       WRITE(8,'(1x,A65)') RECORD(41:105)     
      ENDIF                                                                     
      GOTO  25                                                                  
   28 WRITE(6,'('' Got MS read info..'')')                                      
   30 READ(7,'(A133)',END=130) RECORD                                           
      IF (INDEX(RECORD,'BEGIN EXECUTION').NE.0)THEN                             
   35  IF (RECORD(41:41).NE.'*') WRITE(8,'(1x,A65)') RECORD(41:105)       
       READ(7,'(A133)',END=135) RECORD                                          
       IF (INDEX(RECORD,'TIME WAITING TO EXECUTE').EQ.0) GOTO 35                
      ELSE                                                                      
       GOTO 30                                                                  
      ENDIF                                                                     
      REWIND (7)                                                                
      WRITE(6,'('' Extract warnings..'')')                                      
   40 READ(7,'(A133)',END=140) RECORD                                           
      IF (INDEX(RECORD,'**** OPERATION NO.').NE.0) THEN                         
       LINES=1                                                                  
   41  READ(7,'(A133)',END=145) RECORD                                          
       LINES=LINES+1                                                            
       IF (INDEX(RECORD,'****').EQ.0) GOTO 41                                   
C extract warnings, fatal, catastrophe messages only ..                         
       IF (INDEX(RECORD,'INFORMATION').NE.0) GOTO 40                            
       DO 42 J=1,LINES                                                          
   42   BACKSPACE(7)                                                            
       DO 45 J=1,LINES                                                          
        READ(7,'(A133)',END=145) RECORD                                         
   45   WRITE(8,'(1x,A133)') RECORD         
      ELSEIF (INDEX(RECORD,'WARNING: ').NE.0) THEN                              
       WRITE(8,'(1x,A133)') RECORD   
      ENDIF                                                                     
      GOTO 40                                                                   
  105 WRITE(8,'('' PROJECT TITLE NOT FOUND'')')                                 
      GOTO 150                                                                  
  106 WRITE(8,'('' TIME INFORMATION NOT FOUND'')')                              
      REWIND(7)                                                                 
      GOTO 10                                                                   
  107 WRITE(8,'('' TIME INFO END FLAG NOT FOUND'')')                            
      REWIND(7)                                                                 
      GOTO 10                                                                   
  110 WRITE(8,'('' NO MORE SUMMARIES OF SCALING ERRORS'')')                     
      REWIND(7)                                                                 
      GOTO 26                                                                   
  115 WRITE(8,'('' END OF SCALING ERRORS NOT FOUND'')')                         
      REWIND(7)                                                                 
      GOTO 26                                                                   
  120 WRITE(8,'('' SUCCESFUL  NOT FOUND'')')                                    
      REWIND(7)                                                                 
      GOTO 30                                                                   
  130 WRITE(8,'('' BEGIN EXECUTION NOT FOUND'')')                               
      GOTO 150                                                                  
  135 WRITE(8,'('' USER NUMBER NOT FOUND'')')                                   
      GOTO 150                                                                  
  140 WRITE(8,'('' NO MORE WARNINGS'')')                                        
      GOTO 150                                                                  
  145 WRITE(8,'('' NO **** FLAG TO END MESSAGE'')')                             
      GOTO 150                                                                  
  155 WRITE(8,'('' NWVOL=  NOT FOUND'')')                                       
      GOTO 150                                                                  
  190 WRITE(8,'('' OUTPUT FLUSHP NOT FOUND'')')                                 
      OUTPUT=.FALSE.                                                            
      REWIND(7)                                                                 
      GOTO 26                                                                   
  192 WRITE(8,'('' END OF OUTPUT FLUSHP NOT FOUND'')')                          
      REWIND(7)                                                                 
      GOTO 26                                                                   
  160 WRITE(8,'('' BYTES READ TO  NOT FOUND'')')                                
  150 WRITE(8,'(//)')                                                           
      END                                                                       
