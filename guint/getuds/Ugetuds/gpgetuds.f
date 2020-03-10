C  GPGETUDS FORTRAN --  create Cray JCL to get user directives for              
C   Genpro via Adsraw pgm                                                       
C                                                                               
C   Input --   unit 44: SUMMARY file for project                                
C              on stack: project number                                         
C   Output --  unit 45: JOB FILE -- JOB &  ACCOUNT statements for JCL           
C              unit 46: COMMAND FILE -- command lines for TPSTAGE Proc          
C                                                                               
      integer     YEAR                                                          
      CHARACTER*3 PROJNO,AIRCFT                                                 
      CHARACTER*4 ACCNT                                                         
      CHARACTER*5 FLTNO                                                         
      CHARACTER*6 TAPENO                                                        
      CHARACTER*8 USER                                                         
      character*80 fulpth,arg2
C get project number, derive aircraft number                                    
      call GETARG(1,projno)
      call getarg(2,arg2)
C--------------------------------------start to open 44
         fulpth = ' '
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = projno
      open (unit=44,file=fulpth,access='sequential')
       if ($status .ne. 0) then
       write(6,'(" error with unit 44 /summary.",projno)')
       endif
C-----------------------start to open 45
         fulpth = ' '
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
c     fulpth(lindex+1:lindex+8) = '/adsraw.'
c     fulpth(lindex+9:lindex+11) = projno
c     fulpth(lindex+12:lindex+14) = 'job'
      fulpth(lindex+1:lindex+8) = '/insert.'
      fulpth(lindex+9:lindex+11) = projno
c     fulpth(lindex+12:lindex+14) = 'job'
      open (unit=45,file=fulpth,access='sequential')
	if ($status .ne. 0) then
	 write(6,'(" error with unit 45 /inset.",projno)')
	endif
C-------------start to open 46
         fulpth = ' '
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/command.file'
      open (unit=46,file=fulpth,access='sequential')
       if ($status .ne. 0) then
       write(6,'(" error with unit 46 /command.file")')
       endif
      IF(PROJNO(1:1).NE.'2') THEN                                               
       AIRCFT='30'//PROJNO(1:1)                                                 
      ELSE                                                                      
       AIRCFT='312'                                                             
      ENDIF                                                                     
      REWIND(44)                                                                
      READ(44,'(A8,9X,A4)') USER,ACCNT                                         
C get project year: last flight in project defines this                         
      call gtyear(year,44)
C get user name and account #                                                   
   15 REWIND(44)                                                                
      READ(44,'(A8,9X,A4)') USER,ACCNT                                         
C write out 1st 2 JCL statements                                                
c      WRITE(45,'('' JOB,JN=ADSRAW,US='',A4,                                  
c     $''41113'',A3,'',T=99,OLM=9999,*MS.'',/,                               
c     $'' ACCOUNT,AC='',A4,''41113'',A3,''.'')')ACCNT,AIRCFT,ACCNT,AIRCFT       
      READ(44,'(A6,10X,A5)',END=100) TAPENO,FLTNO

c  ******************************************

        WRITE(45,*) " "
        WRITE(45,'(" newacct 41113",A3)')
     $  AIRCFT
        WRITE(45,*) " "
        WRITE(45,*) "ja "
        WRITE(45,*) " "
	WRITE(45,'(" set DEST = ",A)') arg2       
	WRITE(45,'(" set SOURCE = ",A)') arg2       
	WRITE(45,'(" set USER = ",A8)') USER
        WRITE(45,'(" set HOST = spock.atd.ucar.edu")')
        WRITE(45,'(" set TAPENO = ",A6")') TAPENO
        WRITE(45,'(" set PGRDATA = gpgetuds.data")')
        WRITE(45,'(" set CALOUT = CALOUT")') 
        WRITE(45,'(" set ADSUD = ADSUD")') 
        WRITE(45,'(" set ADSDAT = ADSDAT")')
        WRITE(45,'(" set ADSOUT = gpadsout")')
        IF (FLTNO(5:5) .EQ. " ") THEN
          WRITE(45,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A4,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
        ELSE
          WRITE(45,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A5,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
        ENDIF
        WRITE(45,'(" set FILEFLT = gpadsout",A3,".",A5)')
     $  PROJNO, FLTNO
        WRITE(45,'(" set PROGM = gpgetuds")')
        WRITE(45,'(" set USETAPE = usr/tmp/",A8,"/",A6)')
     $  HOST, TAPENO

c    **************************************
C read first record of SUMMARY file, writing out one command line               
c      WRITE(46,'('' ADSRAW,'',A8,'',19'',i2,'','',A3,'','',A4,'',             
c     $'',''TAPE'',A1,''='',A6,'',UDS.'')')                                     
c     $USER,YEAR,PROJNO,FLTNO(1:4),FLTNO(5:5),TAPENO                            
  100 CONTINUE                                                                  
      END                                                                       
