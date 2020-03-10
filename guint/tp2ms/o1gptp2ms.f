      PROGRAM TP2MS
C  GPTP2MS FORTRAN --  create Cray job to stage in flight tapes                 
C                                                                               
C   Input --   unit 34: SUMMARY file for project                                
C              on command line: project number     
C   Output --  unit 35: JOB &  ACCOUNT statements for JCL           
C              unit 36: command lines for TPSTAGE Proc          
C              unit 37: TAPELIST EXEC input file                
C
C  Revised to include project number on each msimport request
C    by Ron Ruth  August 1991
C
      character *11 summary
      INTEGER YEAR                                                              
      CHARACTER*3 PROJNO,AIRCFT                                                 
      character*2 inp
      CHARACTER*4 ACCNT                                                         
      CHARACTER*5 FLTNO(100)                                                    
      CHARACTER*6 TAPENO(100)                                                   
      CHARACTER*8 USER                                                          
      CHARACTER*80 STRING                                                       
      CHARACTER*5 FLTNUMB
      CHARACTER*12 tpjob
c get project number from command line
      call getarg (1,projno)
c open input/output files
      open (unit=34,file="summary."//projno)
C  derive aircraft number                                    
      IF(PROJNO(1:1).NE.'2') THEN                                               
       AIRCFT='30'//PROJNO(1:1)                                                 
      ELSE                                                                      
       AIRCFT='312'                                                             
      ENDIF                                                                     
C get user name and account #                                                   
      REWIND(34)                                                                
      READ(34,'(A8,9X,A4)') USER,ACCNT                                          
C get project year                                                              
      CALL GTYEAR(YEAR,34)                                                      
C offer individual tapes or all of 'em                                          
      open (unit=12, file="xom")
      write(6,'(///'' Are the tapes on:'',/,'' 1: 1/2 magnetic tape'',
     $/,'' 2: Exabyte'',/''    Choose 1 or 2 or <cr> to exit'')')
      read(5,'(i1)') kk
      if ((kk .lt. 1) .or. (kk .gt. 2)) goto 500
      if (kk .eq. 1) inp = 'MT'
      if (kk .eq. 2) inp = 'EX'
      WRITE(12,*) inp
      WRITE(6,'(//'' To stage in selected flights, choose option 1;'')')        
      WRITE(6,'('' to stage in all, choose option 2: '')')                      
      WRITE(6,'(//,''  (1) Specific tapes  (2) All tapes'')')                   
      READ(5,'(I1)') N                                                  
      if (n.eq.0) goto 500
      NUM=0     
      IF (N.EQ.1) THEN                                                          
        CALL DISPLA(TAPENO,FLTNO,NUM)                                            
        if (num.eq.0) goto 500
c       open (unit=35,file="header.jcl")
c       open (unit=36,file="commands.jcl")

C **************************************************
C  create a unicos job script

        open (unit=37,file="tapelist")
c  This needs to be changed 
        open (unit=38,file="tp2ms."//projno)
c open (unit=38, file="unicos")
	write(38,*) "# QSUB -eo"
	write(38,*) "# QSUB -s /bin/csh" 
	write(38,*) "# QSUB -q reg" 
	write(38,*) "# QSUB -lt 99"
	write(38,*) " "
C	write(38,'(" newacct 41113",A3)') 
C     $  aircft
C	write(38,*) " "
	write(38,*) "cd $TMPDIR"
	write(38,*) "ja jacct"
C  Obsolete code below
C	write(38,*) 'cat << "EOF" >! .ntwkparms'
C	write(38,'(" SCI=",A4)')
C     $  ACCNT
C	write(38,'(" PROJ=41113",A3)')
C     $  aircft
C        write(38,*) '"EOF"'
C  Obsolete code above
	write(38,*) " "

C	write(38,*) "# If an error occurs, you may want to insert the"
C	write(38,*) "#  -j option in the msimport."
	write(38,*) " "

c	for selected flights which were placed in the fltno array
c	down in the display subroutine

	DO 45 K=1,NUM
	FLTNUMB = FLTNO(K)
	call caps (FLTNO(K))

c	if flt value only has 4 characters, must change the format
c	so there are no blanks

	IF (FLTNUMB(5:5) .EQ. " ") THEN
C	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99
	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99 -j -P41113"A3
     +    " -fBI -wRAFDMG -t4095 -uRAF \\"/" -vCTRAFDMG -d1600 -m",A2,
     +    " -MU -N1 ",A6," /RAF/19",I2,"/",A3,"/",A4,"/",A6)')
     +    AIRCFT,inp,TAPENO(K),YEAR,PROJNO,FLTNO(K),TAPENO(K)
	ELSE
C	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99
	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99 -j -P41113"A3
     +    " -fBI -wRAFDMG -t4095 -uRAF \\"/" -vCTRAFDMG -d1600 -m",A2,
     +    " -MU -N1 ",A6," /RAF/19",I2,"/",A3,"/",A5,"/",A6)')
     +    AIRCFT,inp,TAPENO(K),YEAR,PROJNO,FLTNO(K),TAPENO(K)
	ENDIF
        WRITE(37,'(1x,A6)') TAPENO(K)                                          
	WRITE(38,*) " "
	WRITE(38,'(" if ($status !=  0 ) then")')
	WRITE(38,'(" echo unsuccessful import of tape #",A6)')
     $  TAPENO(K)
	WRITE(38,'(" endif")')
	WRITE(38,*) " "

   45	CONTINUE

      ELSE
        open (unit=37,file="tapelist")
	open (unit=38, file="tp2ms."//projno)
	write(38,*) "# QSUB -eo"
	write(38,*) "# QSUB -s /bin/csh"
	write(38,*) "# QSUB -q reg"
	write(38,*) "# QSUB -lt 99"
	write(38,*) " "
	write(38,'(" newacct 41113",A3)') 
     $  aircft
	write(38,*) " "
	write(38,*) "cd $TMPDIR"
	write(38,*) "ja jacct"
C  Obsolete code below
C	write(38,*) 'cat << "EOF" >! .ntwkparms'
C	write(38,'(" SCI=",A4)')
C     $  ACCNT
C	write(38,'(" PROJ=41113",A3)')
C     $  aircft
C        write(38,*) '"EOF"'
C  Obsolete code above
	write(38,*) " "
C	write(38,*) "# If an error occurs, you may want to insert the"
C	write(38,*) "#  -j option in the msimport."
	write(38,*) " "

c	must read in summary file and skip the first record
	READ(34,'(A80)') STRING(1:80)
   50   READ(34,'(A6,10X,A5)',END=100) TAPENO(1), FLTNO(1)
	FLTNUMB = FLTNO(1)
	call caps (FLTNO(1))

c	if flt value only has 4 characters, must change the format
c	so there are no blanks

	IF (FLTNUMB(5:5) .EQ. " ") THEN
C	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99
	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99 -j -P41113"A3
     +    " -fBI -wRAFDMG -t4095 -uRAF \\"/" -vCTRAFDMG -d1600 -m",A2,
     +    " -MU -N1 ",A6," /RAF/19",I2,"/",A3,"/",A4,"/",A6)')
     +    AIRCFT,inp,TAPENO(1),YEAR,PROJNO,FLTNO(1),TAPENO(1)
	ELSE
C	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99
	  WRITE(38,'(" msimport -n -b50000 -Eyes -e99 -j -P41113"A3
     +    " -fBI -wRAFDMG -t4095 -uRAF \\"/" -vCTRAFDMG -d1600 -m",A2,
     +    " -MU -N1 ",A6," /RAF/19",I2,"/",A3,"/",A5,"/",A6)')
     +    AIRCFT,inp,TAPENO(1),YEAR,PROJNO,FLTNO(1),TAPENO(1)
	ENDIF

        WRITE(37,'(1x,A6)') TAPENO(1)                                          

	WRITE(38,*) " "
	WRITE(38,'(" if ($status !=  0 ) then")')
	WRITE(38,'(" echo unsuccessful import of tape #",A6)')
     $  TAPENO(1)
	WRITE(38,'(" endif")')
	WRITE(38,*) " "

c	must loop for all flights
	GOTO 50
  100   CONTINUE

      ENDIF
	write(38,*) " "
	write(38,*) "ja -cst jacct"
  500   CONTINUE
C       RETURN
	END
C **************************************************    

C write out 1st 2 JCL statements                                                
c      WRITE(35,'('' JOB,JN=TP2MS,US='',A4,
c    $ ''41113'',A3,'',T=99,OLM=9999,*MS.'',/,
c    $ '' ACCOUNT,AC='',A4,''41113'',A3,''.'')')
c    $ ACCNT,AIRCFT,ACCNT,aircft
c      DO 40 J=1,NUM                                                            
c       WRITE(36,'('' TPSTAGE,'',A8,'','',A6,'','',A3,'','',A5,'','',  
c    $  ''YEAR=19'',I2,'',INP='',a2,'',NFILES=1.'')')                     
c    $  USER,TAPENO(J),PROJNO,FLTNO(J),YEAR,inp                                 
C also write out one line per tape to TAPELIST input file                       
c       WRITE(37,'(1x,A6)') TAPENO(J)                                          
c 40   CONTINUE                                                                 
c      IF(NUM.NE.0)
c    $  WRITE(36,'('' EXIT.'',/,'' DUMPJOB.'',/,'' DEBUG.'')')         
c     ELSE                                                                      
c      open (unit=35,file="header.jcl")
c      open (unit=36,file="commands.jcl")
c      open (unit=37,file="tapelist")
C write out 1st 2 JCL statements                                                
c      WRITE(35,'('' JOB,JN=TP2MS,US='',A4,
c    $ ''41113'',A3,'',T=99,OLM=9999,*MS.'',/,
c    $ '' ACCOUNT,AC='',A4,''41113'',A3,''.'')')ACCNT,AIRCFT,ACCNT,
c    $ AIRCFT  
C skip 1st record of project information                                        
c      READ(34,'(A80)') STRING(1:80)                                            
C read rest of SUMMARY file, writing out one command line for each tape         
c  50  READ(34,'(A6,10X,A5)',END=100) TAPENO(1),FLTNO(1)                        
c      WRITE(36,'('' TPSTAGE,'',A8,'','',A6,'','',A3,'','',A5,'','',  
c    $ ''YEAR=19'',I2,'',INP='',a2,'',NFILES=1.'')')                        
c    $ USER,TAPENO(1),PROJNO,FLTNO(1),YEAR,inp                                  
C also write out one line per tape to TAPELIST input file                       
c      WRITE(37,'(1x,A6)') TAPENO(1)                                  
c      GOTO 50                                                                  
c 100  CONTINUE                                                                 
c      WRITE(36,'('' EXIT.'',/,'' DUMPJOB,BLOCKS.'',/,'' DEBUG.'')')   
c     ENDIF                                                                     
c 500 CONTINUE                                                                  
c     RETURN                                                                    
c     END                                                                       
      SUBROUTINE DISPLA(TAPENO,FLTNO,NUM)                                       
C                                                                               
C  display choices from SUMMARY file and get user choice; return FOUND          
C false if choice not found, true if found                                      
C                                                                               
      CHARACTER *5 FLTNO(*),FLTNUM                                              
      CHARACTER *6 TAPENO(*)                                                    
      CHARACTER*90 STRING                                                       
C                                                                               
      write(6,'(////)')
      REWIND(34)                                                                
C skip 1st record of project information                                        
      READ(34,'(A80)') STRING(1:80)                                             
C display 4 across screen, get user response                                    
      J=0                                                                       
      STRING=' '                                                                
 1215 READ(34,'(16X,A5)',END=1116)  FLTNUM                                      
	call caps (FLTNO)
      WRITE(STRING(J*20+1:J*20+5),'(A5)') FLTNUM                                
      J=J+1                                                                     
      IF (J.EQ.4) THEN                                                          
       J=0                                                                      
       WRITE(6,'('' '',A80)') STRING                                            
       STRING=' '                                                               
      ENDIF                                                                     
      GOTO 1215                                                                 
 1116 IF (J.LT.4) WRITE(6,'('' '',A80)') STRING                                 
      WRITE(6,'(//,'' Select Flight(s) -- <r> to exit: '')')                    
      WRITE(6,'('' Select one at a time and hit '')')                    
      WRITE(6,'('' return <r> after each entry. '')')                    
  401 READ(5,'(A5)') FLTNO(NUM+1)                                       
c   removed l from caps below
      call lcaps(fltno(num+1))
      if (fltno(num+1).eq."") return
C  search SUMMARY file for FLTNO                                                
      REWIND(34)                                                                
C skip 1st record of project information                                        
      READ(34,'(A5)') FLTNUM                                                    
    5 READ(34,'(16X,A5)',END=999) FLTNUM                                        
      IF(FLTNUM.NE.FLTNO(NUM+1)) GOTO 5                                         
      NUM=NUM+1                                                                 
      BACKSPACE(34)                                                             
      READ(34,'(A6,10X,A5)') TAPENO(NUM),FLTNO(NUM)                             
      GOTO 401                                                                  
 999  WRITE(6,'('' Flight # not found; retry or <cr> to exit'')')               
      GOTO 401                                                                  
      END                                                                       
