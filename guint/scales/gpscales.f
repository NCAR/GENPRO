C     PROGRAM SCALER                                                            
C                                                                               
C examine a Genpro Logfile Statistics section and report any                    
C variables that have a minimum value less than -999., which                    
C would result in scaling errors in the Output Operation                        
C                                                                               
      CHARACTER *8 VAR                                                          
      REAL VALUE                                                                
      CHARACTER *133 RECORD                                                     
      character*80 arg2,fulpth
      character*9 prname
      character*7 flt

      call getarg(1,flt)
      call getarg(2,arg2)
      call getarg(3,prname)
C--------------------------------------start to open 17
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      lldex=index(prname,' ') - 1
      fulpth(lindex+1:lindex+1) = '/'
      fulpth(lindex+2:lindex+lldex+1) = prname(1:lldex)
      llndex = lindex +lldex + 1
      fulpth(llndex+1:llndex+1) = '.'
      lidx = index(flt,' ') - 1
      fulpth(llndex+2:llndex+1+lidx) = flt(1:lidx)
C-----------------------start to open 17
      open (unit=17,file=fulpth,access='sequential',
     $          form='formatted')
      nnn = 0
    5 READ(17,'(A133)',END=100) RECORD                                          
      IF (INDEX(RECORD,'DISPLAY INTERVAL FOR STATS').EQ.0) GOTO 5               
C     PAUSE 'FOUND DISPLAY INTERVAL'                                            
   10 READ(17,'(A133)',END=200) RECORD                                          
      IF (INDEX(RECORD,'NAMVAR      RATE        UNITS ').EQ.0) GOTO 10          
C     PAUSE 'FOUND NAMVAR'                                                      
   15 READ(17,'(A133)',END=300) RECORD                                          
      IF (RECORD.EQ.'   '.OR.INDEX(RECORD,'*').NE.0.                            
     $    .OR.RECORD(1:1).EQ.'1') GOTO 5                                        
      VAR=RECORD(3:10)                                                          
      READ(RECORD,'(72X,G12.3)',END=400) VALUE                                  
C     WRITE(6,'('' VAR: '',A8,''  VALUE: '',G12.3)') VAR,VALUE                  
      if ((value .lt. -999) .and. (nnn .eq. 0)) then
         nnn = 1
C--------------------------------------start to open 18
         do 14, i = 1, 80
            fulpth(i:i) = ' '
 14      continue
         lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
         lindex = lindex -1
         fulpth(1:lindex) = arg2(1:lindex)
         lldex=index(prname,' ') - 1
         fulpth(lindex+1:lindex+1) = '/'
         fulpth(lindex+2:lindex+lldex+1) = prname(1:lldex)
         llndex = lindex +lldex + 1
         fulpth(llndex+1:llndex+5) = '.scle'
C-----------------------start to open 18
         open (unit=18,file=fulpth,access='sequential',recl=80,
     $          form='formatted')
      endif
      IF (VALUE.LT.-999.) WRITE(18,'(A8,2X,F16.0)') VAR,VALUE                   
      GOTO 15                                                                   
  100 CONTINUE                                                                  
C     PAUSE ' NO MORE DISPLAY INTERVALS'                                        
      STOP                                                                      
  200 CONTINUE                                                                  
C     PAUSE ' NO MORE NAMVARS'                                                  
      STOP                                                                      
  300 CONTINUE                                                                  
C     PAUSE ' NO FIND END OF DISPLAY'                                           
      STOP                                                                      
  400 CONTINUE                                                                  
C     PAUSE ' NO MORE VARIABLES'                                                
      END                                                                       
