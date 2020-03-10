C   GPRAWDMP - user interface to set up JCL and run the GPADSRAW               
C   program on Cray (for getting a raw tape dump)                               
C                                                                               
C     Input:  unit 91 -- SUMMARY file                                           
C             unit 93 -- HEADER file                                            
C             unit 95 -- GPADSRAW JOB -- prototype deck to be modified          
C     Output: unit 90 -- *DATA FILE*
C             unit 92 -- *INSERT INFO*
C                                                                               
      character*1 arg3,iscale,icol,ihow
      integer     year,ihr1,imin1,isec1,ihr2,imin2,isec2,irec1,irec2
      CHARACTER*3 PROJNO,AIRCFT                                                 
      character*80 miscp
      CHARACTER*4 ACCNT                                                         
      CHARACTER*5 FLTNO                                          
      CHARACTER*6 TAPENO                                                        
      PARAMETER (MAXSDI=200)                                                    
      CHARACTER*8 VARS(99),SDINAM(MAXSDI)                                       
      CHARACTER*8 USER                                                         
      CHARACTER*80 RECORD,fulpth,arg2                                  
      DIMENSION SDIPTR(MAXSDI)                                                  
      INTEGER SDIPTR,BEGIV(3),ENDIV(3),lindex                                  
      LOGICAL FOUND,HEADER                                                      
      DATA IHR1/0/IMIN1/0/ISEC1/0/IHR2/0/IMIN2/0/
     $ISEC2/0/
      DATA IREC1/0/IREC2/0/  
C get project number, derive aircraft number                                    
      call GETARG(1,projno)
      call getarg(2,arg2)
      call getarg(3,arg3)
      if (arg3 .eq. 'T') then
          header = .TRUE.
      else
          header = .FALSE.
      endif 
C--------------------------------------start to open 91
         fulpth = ' '
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = projno
      open (unit=91,file=fulpth,access='sequential')
C-----------------------start to open 90
c *****for pgrdata I used only rawdmp.777*****
         fulpth = ' '
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/rawdmp.'
      fulpth(lindex+9:lindex+11) = projno
c     fulpth(lindex+12:lindex+14) = 'job'
      open (unit=90,file=fulpth,access='sequential')
C-------------start to open 92
         fulpth = ' '
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/insert.'
      fulpth(lindex+9:lindex+14) = projno
      open (unit=92,file=fulpth,access='sequential')
C-------------open 93 if header is true
      if (header) then
            fulpth = ' '
         fulpth(1:lindex) = arg2(1:lindex)
         fulpth(lindex+1:lindex+9) = '/header.'
         fulpth(lindex+10:lindex+12) = projno
         open (unit=93,file=fulpth,access='sequential')
      endif
      call getenv ('rawdmp',miscp)
         fulpth = ' '
      lindex = index(miscp,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = miscp(1:lindex)
      fulpth(lindex+1:lindex+13) = '/gpadsraw.job'
      open (unit=95,file=fulpth,access='sequential')
C-----------propjno and header are now read as command line arguments  
      IF(PROJNO(1:1).NE.'2') THEN                                               
       AIRCFT='30'//PROJNO(1:1)                                                 
      ELSE                                                                      
       AIRCFT='312'                                                             
      ENDIF                                                                     
C get user name and account #                                                   
      REWIND(91)                                                                
      READ(91,'(A8,9X,A4)') USER,ACCNT                                         
C get project year: last flight in project defines this                         
      call gtyear(year,91)
C offer list of flight numbers                                                  
      CALL DISPLA(FOUND,TAPENO,FLTNO,BEGIV,ENDIV)                               
      IF (.NOT.(FOUND)) RETURN                                                  
c     CALL system("clear")                                             
C write out 1st 2 JCL statements                                                
c     WRITE(90,'('' JOB,JN=RAWDMP,US='',A4,                                   
c    $''41113'',A3,'',T=59,CL=EX1,OLM=9999,*MS.'',/,                        
c    $'' ACCOUNT,AC='',A4,''41113'',A3,''.'')')ACCNT,AIRCFT,ACCNT,AIRCFT        
C  append first portion only of GPADSRAW JOB to allow for user specifics        
C  on $IN section                                                               
      REWIND(95)                                                               
  400 READ(95,'(A80)') RECORD                                                   
      WRITE(90,'(1X,A80)') RECORD                                          
      IF(INDEX(RECORD,'DO YOU WANT A RAW TAPE DUMP').EQ.0) GOTO 400             
      WRITE(90,'(''  1'')')                                                
C now modify default values in $IN according to user specs                      
      WRITE(6,'(/'' Select Raw Dump: (0) Before or (1) After Scaling''/         
     $ )')                                                                      
      READ(5,'(A1)',END=500) ISCALE                                             
      if (iscale .eq.  '') goto 500
      WRITE(90,'('' RAW DUMP BEFORE (0) OR AFTER SCALING (1)?'')')            
      WRITE(90,'(1X,A1)') ISCALE                                         
      WRITE(6,'(/'' Select Dump by: (0) Record number or (1) Time''/            
     $ )')                                                                      
      READ(5,'(a1)',END=500) IHOW                                               
      if (ihow .eq. '') goto 500
      WRITE(90,'('' DUMP BY RECORD NUMBER (0) OR TIME (1)?'')')  
      WRITE(90,'(1X,I1)') IHOW                                           
C     WRITE(6,'('' PRE-TIME,PROJNO,HEADER:'',A3,1X,L1)')PROJNO,HEADER           
C     PAUSE                                                                     
      IF(IHOW.EQ. '1 ') THEN                  
       WRITE(6,'('' Range of times: '',3(I2,1X),'' to '',3(I2,1X))')            
     $ BEGIV,ENDIV                                                              
       WRITE(6,'(/'' Indicate Start HR MIN SEC (hh mm ss): '')')                
       READ(5,'(BN,3(I2,1X))',END=500) IHR1,IMIN1,ISEC1                         
       if (ihr1 .eq. 0) goto 500
       WRITE(6,'(/'' Indicate Stop HR MIN SEC (hh mm ss): '')')                 
       READ(5,'(BN,3(I2,1X))',END=500) IHR2,IMIN2,ISEC2                         
       if (ihr2 .eq. 0) goto 500
      ELSE                                                                      
       WRITE(6,'(/'' Indicate first/last record numbers (fffff lllll):''        
     $ )')                                                                      
       READ(5,'(BN,I5,1X,I5)',END=500) IREC1,IREC2                              
       if (irec1 .eq. 0) goto 500
      ENDIF                                                                     
C     WRITE(6,'('' POSTTIME,PROJNO,HEADER:'',A3,1X,L1)')PROJNO,HEADER           
C     PAUSE                                                                     
      WRITE(90,'('' HR    MI   SE'')')                                          
      WRITE(90,'(1X,BN,3I5,/,1X,3I5)') IHR1,IMIN1,ISEC1,IHR2,IMIN2,ISEC2      
      WRITE(90,'('' START STOP  (RECORD NUMBERS)'')')           
      WRITE(90,'(1X,BN,2I5)') IREC1,IREC2                              
      WRITE(6,'(/'' Select Print by (0) Column or (1) Horizontally''/           
     $ )')                                                                      
      READ(5,'(A1)',END=500) ICOL                                               
      if (icol .eq. '') goto 500
      WRITE(90,'('' PRINTED BY COLUMN (0) OR HORIZONTALLY (1)?'')')    
      WRITE(90,'(1X,A1)') ICOL                                           
      WRITE(90,'('' HOW MANY VARIABLES DO YOU WANT TO DUMP?'')')
c     CALL system("clear")                                              
      NUM=0                                                                     
C     WRITE(6,'('' PRE-IF,PROJNO,HEADER:'',A3,1X,L1)')PROJNO,HEADER             
C     PAUSE                                                                     
      IF (HEADER) THEN                                                          
C get list of variables in header                                               
       CALL RDHDR(SDINAM,MAXSDI,NFULL)                                          
C   Get Variables to dump                                                       
       CALL SELECT(SDINAM,NFULL,SDIPTR,NUM)                                     
      ELSE                                                                      
       WRITE(6,'('' Enter Variables for dump ... <r> when done: '')')           
 1100  READ(5,'(A8)',END=1101) SDINAM(NUM+1)                                    
       call caps(sdinam(num+1))
       if (sdinam(num+1) .eq. '') goto 1101
       NUM=NUM+1                                                                
       SDIPTR(NUM)=NUM                                                          
       GOTO 1100                                                                
      ENDIF                                                                     
 1101 continue 
C don't continue if no vars selected                                            
      IF (NUM.EQ.0) GOTO 500                                                    
      WRITE(90,'(1X,BN,I2)') NUM                                           
      WRITE(90,'(''  LIST OF VARIABLES FOR RAW DUMP: '')')                      
      WRITE(90,'(1X,8(1X,A8))') (SDINAM(SDIPTR(J)),J=1,NUM)        
c     WRITE(90,'('' ENDPROC.''/'' ECHO,ON.'')')         
C write out command line -- if we've gotten this far, it means user input       
C is completed and the Append file will exist as flag for calling program       
c     WRITE(92,'('' ADSRAW,'',A8,'',YEAR=19'',I2,'','',A3,'','',A5,         
c    $  '',NFILES=1,RAWDUMP,TAPE='',A6,''.'')')                                 
c    $  USER,YEAR,PROJNO,FLTNO,TAPENO                                           
c     WRITE(92,'('' EXIT.'',/,'' DUMPJOB.'',/,'' DEBUG.'')')          

c	**************************************** 

        WRITE(92,*) " " 
        WRITE(92,'(" newacct 41113",A3)')
     $  AIRCFT
        WRITE(92,*) " " 
        WRITE(92,*) "ja "
        WRITE(92,*) " " 
	WRITE(92,'(" set USER = ",A8")') USER
	WRITE(92,'(" set PROGM = adsraw")')
c *******I think I need rawdmp.777job for pgrdata*****
	WRITE(92,'(" set PGRDATA = rawdmp.",A3)') PROJNO
	WRITE(92,'(" set TAPENO = ",A6")') TAPENO
	WRITE(92,'(" set CALOUT = CALOUT")')
	WRITE(92,'(" set ADSUD = ADSUD")')
	WRITE(92,'(" set ADSDAT = ADSDAT")')
	WRITE(92,'(" set FILEFLT = gpadsout",A3,".",A5)') 
     $  PROJNO, FLTNO
	WRITE(92,'(" set ADSOUT = gpadsout")')
        WRITE(92,'(" set DEST = ",A)') arg2
        WRITE(92,'(" set SOURCE = ",A)') arg2
	IF (FLTNO(5:5) .EQ. " ") THEN
	  WRITE(92,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A4,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
	ELSE
	  WRITE(92,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A5,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
	ENDIF
	WRITE(92,'(" set HOST = spock.atd.ucar.edu")')

c	*****************************************
  500 continue                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DISPLA(FOUND,TAPENO,FLTNO,BEGIV,ENDIV)                         
C                                                                               
C  display choices from SUMMARY file and get user choice; return FOUND          
C false if choice not found, true if found                                      
C                                                                               
      character  bob
      CHARACTER *5 FLTNO,FLTNUM                                                 
      CHARACTER *6 TAPENO                                                       
      CHARACTER*90 STRING                                                       
      INTEGER BEGIV(3),ENDIV(3)                                                 
      LOGICAL FOUND                                                             
C                                                                               
      CALL system("clear")                                            
      REWIND(91)                                                                
C skip 1st record of project information                                        
      READ(91,'(A80)') STRING(1:80)                                             
C display 4 across screen, get user response                                    
      J=0                                                                       
      STRING=' '                                                                
 1215 READ(91,'(16X,A5)',END=1116)  FLTNUM                                      
      WRITE(STRING(J*20+1:J*20+5),'(A5)') FLTNUM                           
      J=J+1                                                                     
      IF (J.EQ.4) THEN                                                          
       J=0                                                                      
       WRITE(6,'('' '',A80)') STRING                                            
       STRING=' '                                                               
      ENDIF                                                                     
      GOTO 1215                                                                 
 1116 IF (J.LT.4) WRITE(6,'('' '',A80)') STRING                                 
      WRITE(6,'(//,'' Select Flight -- <r> to exit: '')')                       
  401 READ(5,'(A5)',END=402) FLTNO                                              
      call lcaps(fltno)
      if (fltno .eq. '') goto 402
C  search SUMMARY file for FLTNO                                                
      REWIND(91)                                                                
C skip 1st record of project information                                        
      READ(91,'(A5)') FLTNUM                                                    
    5 READ(91,'(16X,A5)',END=999) FLTNUM                                        
      IF(FLTNUM.NE.FLTNO) GOTO 5                                                
      FOUND=.TRUE.                                                              
      BACKSPACE(91)                                                             
      READ(91,'(A6,10X,A5,2X,3(I2),1X,3(I2))') TAPENO,FLTNO,BEGIV,ENDIV         
      RETURN                                                                    
 999  WRITE(6,'('' Flight # not found; retry or <cr> to exit'')')               
      GOTO 401                                                                  
  402 continue                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SELECT(IARRAY,IDBSIZ,IMODE,ISIZE)                              
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
C   ISIZE:  # of selected vars in IARRAY                                        
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
C       IDBSIZ words.                                                           
C                                                                               
      INTEGER IDSIZ                                                             
      CHARACTER *16 DBASE(200),RECORD                                           
      CHARACTER * 9 CH , IFMT                                                   
      CHARACTER * 8 IARRAY(1)                                                   
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
c     CALL system("clear")                                              
C  get an INTEGER *4 argument for MIN0                                          
      IDSIZ=IDBSIZ                                                              
      DO 50 J=ISTRT,MIN0(IDSIZ,ISTRT + IROWS - 1)                               
       WRITE(6,110) J,DBASE(J)                                                  
  110  FORMAT(1X,I3,': ',A16)                                                      
   50 CONTINUE                                                                  
      WRITE(6,'(/'' Select Variables for dump ... H for help ...'')')           
C                                                                               
C  clear input buffer and read next user input                                  
C                                                                               
   55 CH='         '                                                            
      READ(5,120,END=200) CH                                                    
      call caps(ch)
      if (ch .eq. '') goto 200 
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
       WRITE(6,2555) ICOUNT                                                     
 2555  FORMAT(' TOTAL SELECTED: ',I5)                                           
       GOTO 55                                                                  
      ELSEIF (CH.EQ.'S') THEN                                                   
C                                                                               
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
c      CALL system("clear")                                          
       WRITE(6, 969)                                                            
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
 7070  READ(5,120,END=7071) CH                                                  
      call caps(ch)
      if (ch .eq. '') goto 7071
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
  250  FORMAT(' (I',I1,',X,I',I1,')')                                           
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
      ISTRT=ISTRT + IROWS                                                       
      IF (ISTRT.GT.IDBSIZ) ISTRT=1                                              
      GOTO 10                                                                   
      END                                                                       
      SUBROUTINE RDHDR(SDINAM,MAXSDI,NFULL)                                     
C                                                                               
C   read header info prepared by ADSRAW from GENSET <projno> C                  
C                                                                               
      CHARACTER *(*) SDINAM (1)                                                 
      CHARACTER *80 DUMMY                                                       
 1045 FORMAT(3(I2,1X),4X,A3)                                                    
 1080 FORMAT(3X,A4)                                                             
 1082 FORMAT(I6)                                                                
 1131 FORMAT(A4)                                                                
      REWIND(93)                                                                
      DO 100 I=1,14                                                             
       READ(93,'(A80)') DUMMY                                                   
  100 CONTINUE                                                                  
      READ(93,1082) NFULL                                                       
      IF (NFULL.GT.MAXSDI) NFULL=MAXSDI                                         
      READ(93,1082) NUMPMS                                                      
      DO 231 I=1,NUMPMS                                                         
         READ(93,1131) DUMMY                                                    
  231 CONTINUE                                                                  
      DO 1200 I=1,NFULL                                                         
       READ(93,1150)SDINAM(I)                                                   
 1200 CONTINUE                                                                  
 1150 FORMAT(7X,A8)                                                             
      RETURN                                                                    
      END                                                                       
