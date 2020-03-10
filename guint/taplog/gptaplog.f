C   Extract pertinent information from file of type JOBCHK, created             
C   via GENPRO Exec 'short check' option. Write info to file of                 
C   type TAPLOG, in form suitable for Production Tape Log document.             
C                                                                               
C   The entire functionality of this routine is based on the output             
C   produced by JOBCHK FORTRAN; as well, assumptions are made concerning        
C   MS file naming protocols, Genpro output formats, flight number              
C   protocols, and the like. Look for changes in these areas if errors          
C   are encountered here.                                                       
C                                                                               
      CHARACTER *133 RECORD                                                     
      CHARACTER *8 FLDATE                                                       
      CHARACTER *6 IOSEQ                                                        
      CHARACTER *6 INPUT(20),OUTPUT                                             
      CHARACTER *5 FLTNO                                                        
      character *3 prnum
      character *9 prname
      character*80 arg2,fulpth
      DIMENSION HMS(3),BEGSNP(10,3),ENDSNP(10,3),BEGSEX(10),ENDSEX(10)          
      INTEGER HMS,BEGSNP,ENDSNP,BEGSEX,ENDSEX                                   
C                                                                               
C   Unit 9: JOBCHK file (input)  Unit 51: TAPLOG file (output) ...              
C   Calling program must stack the IOSequence number of the Job                 
C   extract (short check) in the JOBCHK file that is desired for                
C   entry into the TAPLOG file                                                  
      call getarg(1,prnum)
      call getarg(2,arg2)
      call getarg(3,ioseq)
      call getarg(4,prname)

C--------------------------------------start to open 9
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+1) = '/'
      lldex = index(prname,' ') - 1
      fulpth(lindex+2:lindex+1+lldex) = prname(1:lldex)
      fulpth(lindex+lldex+2:lindex+lldex+8) = '.jobchk'
      open (unit=9,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 52
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/taplog.'
      fulpth(lindex+9:lindex+11) = prnum
      fulpth(lindex+12:lindex+13) = 'tp'
      open (unit=52,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 51
      do 17, i = 1, 80
         fulpth(i:i) = ' '
 17   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/taplog.'
      fulpth(lindex+9:lindex+11) = prnum
      fulpth(lindex+12:lindex+13) = 'hd'
      open (unit=51,file=fulpth,access='sequential',
     $          form='formatted')
C   search input for this IOSequence #; line look like this:                    
C  MACHINE = CRAY             JOBIDS =   SIO3424                                
      REWIND (9)                                                                
    5 READ(9,'(A133)',END=150) RECORD                                           
      IF(INDEX(RECORD,IOSEQ).EQ.0) GOTO 5                                       
C   got it; go back 3 lines and get the flight date:                            
C  PRDATE = 07  08  87        PRTIME = 15H 57M 17S                              
C  EXDATE = 22  DEC 87        EXTIME = 18H 10M 33S                              
C  MACHINE = CRAY             JOBIDS =   SIO3424                                
      DO 1 J=1,3                                                                
    1  BACKSPACE(9)                                                             
      READ(9,'(13X,A3,1X,A3,1X,A2)') FLDATE(1:3),FLDATE(4:6),FLDATE(7:8)        
C  get output snapshots                                                         
      NMSNAP=0                                                                  
   24 READ(9,'(A133)',END=1024) RECORD                                          
      IF (INDEX(RECORD,'OUTPUT  FLUSHP').EQ.0) GOTO 24                          
   25 READ(9,'(A133)') RECORD                                                   
      IF(INDEX(RECORD,'ENDSNP').EQ.0) THEN                                      
       NMSNAP=NMSNAP+1                                                          
       READ(RECORD,'(17X,I2,7X,I2,7X,I2)')(BEGSNP(NMSNAP,K),K=1,3)              
       DO 251 K=1,3                                                             
  251   HMS(K)=BEGSNP(NMSNAP,K)                                                 
       CALL HMS2SX(HMS,BEGSEX(NMSNAP))                                          
       GOTO 25                                                                  
      ELSE                                                                      
       READ(RECORD,'(17X,I2,7X,I2,7X,I2)')(ENDSNP(1,K),K=1,3)                   
       DO 252 K=1,3                                                             
  252   HMS(K)=ENDSNP(1,K)                                                      
       CALL HMS2SX(HMS,ENDSEX(1))                                               
       DO 26 J=2,NMSNAP                                                         
        READ(9,'(17X,I2,7X,I2,7X,I2)')(ENDSNP(J,K),K=1,3)                       
        DO 253 K=1,3                                                            
  253    HMS(K)=ENDSNP(J,K)                                                     
   26   CALL HMS2SX(HMS,ENDSEX(J))                                              
      ENDIF                                                                     
C ensure that the length of first output snapshot does not exceed max           
C that will fit on one physical tape                                            
      CALL TAPLEN(NSXOUT)                                                       
      IF (ENDSEX(1)-BEGSEX(1)+1.GT.NSXOUT) THEN                                 
       WRITE(6,'(//,'' This flight will NOT be logged. Enter Setup pr''
     $,''ogram, SAve '',/,'' a setup for this flight,and rerun to get''        
     $,'' the correct'',/,'' output.'',/)')                                     
       PAUSE ' Hit <r> to continue'                                             
       STOP                                                                     
      ENDIF                                                                     
C  get all Input Tape numbers                                                   
      NMTAPE = 0                                                                
  124 READ(9,'(A133)') RECORD                                                   
      IF (INDEX(RECORD,'BEGIN EXECUTION').NE.0) GOTO 125                        
      ISTART=INDEX(RECORD,'BYTES READ TO ')                                     
      IF (ISTART.EQ.0) GOTO 124                                                 
      NMTAPE=NMTAPE + 1                                                         
      INPUT(NMTAPE)=RECORD(ISTART+14:ISTART+19)                                 
      GOTO 124                                                                  
C   loop until OUTPUT to MS text is found                                       
C TEXT='FLNM=/RAF/1987/267/HRT/RF01A,MVN=CTRAFDMG'.(                      
 125  READ(9,'(A133)',END=160) RECORD                                           
C  start point of output tape number                                            
      INSTRT=INDEX(RECORD,'MVN=CTRAFDMG')                                       
      IF(INSTRT.NE.0.) THEN                                                     
C   get output volume number                                                    
       OUTPUT="      "                                         
C assuming flight number is 4th level in MS path name                           
       NUMSLS=0                                                                 
       DO 127 J=1,133                                                           
        IF(RECORD(J:J).EQ.'/') THEN                                             
         NUMSLS=NUMSLS + 1                                                      
         IF (NUMSLS.EQ.5) ISTART=J + 1                                          
         iend = index(record,',') -1
        ENDIF                                                                   
  127  CONTINUE                                                                 
       FLTNO=RECORD(ISTART:IEND)                                                
      ELSE                                                                      
       GOTO 125                                                                 
      ENDIF                                                                     
C   write all pertinent info to output file                                     
C RF01A 07 08 87 57441.00 64257.00 V55932 G55932 IO3424                         
      CALL EXTEND(52)                                                           
      WRITE(52,'(1x,A5,1X,A8,1X,2(A6,1X),2(2X,3(I2,1X)),2X,I5,1X,I5)')      
     $ FLTNO,FLDATE,INPUT(1),OUTPUT,                                            
     $ (BEGSNP(1,K),K=1,3),(ENDSNP(1,K),K=1,3),BEGSEX(1),ENDSEX(1)              
      DO 133 J=2,MIN0(NMTAPE,NMSNAP)                                            
 133   WRITE(52,'(1x,A5,10X,A6,8X,2(2X,3(I2,1X)),2X,I5,1X,I5)')  
     $ FLTNO,INPUT(J),                                                          
     $ (BEGSNP(J,K),K=1,3),(ENDSNP(J,K),K=1,3),BEGSEX(J),ENDSEX(J)              
      IF(NMTAPE.GT.NMSNAP) THEN                                                 
       DO 134 J=NMSNAP+1,NMTAPE                                                 
 134    WRITE(52,'(1x,A5,10X,A6)') FLTNO,INPUT(J)               
      ELSEIF (NMSNAP.GT.NMTAPE) THEN                                            
       DO 135 J=NMTAPE+1,NMSNAP                                                 
 135    WRITE(52,'(1x,A5,24X,2(2X,3(I2,1X)),2X,I5,1X,I5)') FLTNO,     
     $ (BEGSNP(J,K),K=1,3),(ENDSNP(J,K),K=1,3),BEGSEX(J),ENDSEX(J)              
      ENDIF                                                                     
      STOP                                                                      
C   exceptions                                                                  
 150  WRITE(52,'('' IOSEQUENCE '',A6,''  NOT FOUND'')') IOSEQ                   
      STOP                                                                      
 160  WRITE(52,'('' OUTPUT TO MS NOT FOUND '')')                                
      STOP                                                                      
 170  WRITE(52,'('' KPRINT TO MS NOT FOUND '')')                                
      STOP                                                                      
1024  WRITE(52,'('' OUTPUT  FLUSHP NOT FOUND '')')                              
      END                                                                       
