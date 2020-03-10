C  GPGETUDS FORTRAN --  create Cray JCL to get user directives for              
C   Genpro via Adsraw pgm                                                       
C                                                                               
C   Input --   unit 44: SUMMARY file for project                                
C              on stack: project number                                         
C   Output --  unit 45: INSERT FILE 

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
       open (unit=44,file=fulpth,access='sequential',
     $          form='formatted')
C-----------------------start to open 45


c         fulpth = ' '
C---------adjust lindex to the end of the string arg2
c      fulpth(1:lindex) = arg2(1:lindex)
c     fulpth(lindex+1:lindex+8) = '/adsraw.'
c     fulpth(lindex+9:lindex+11) = projno
c     fulpth(lindex+12:lindex+14) = 'job'
c      fulpth(lindex+1:lindex+8) = '/insert.'
c      fulpth(lindex+9:lindex+11) = projno
c     fulpth(lindex+12:lindex+14) = 'job'
c      open (unit=45,file=fulpth,access='sequential',
c     $   form='formatted',iostat=ierr,err=110)
C-------------start to open 46


c         fulpth = ' '
C---------adjust lindex to the end of the string arg2
c      fulpth(1:lindex) = arg2(1:lindex)
c      fulpth(lindex+1:lindex+13) = '/command.file'
c      open (unit=46,file=fulpth,access='sequential',
c     $   form='formatted',iostat=ierr,err=110)

	open (unit=45,file="insert."//projno,form='formatted')

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
      READ(44,'(A6,10X,A5)',END=100) TAPENO,FLTNO
C       call caps (FLTNO)
C       call lcaps (USER)

c  ******************************************

        WRITE(45,*) " "
        WRITE(45,'(" newacct 41113",A3)')
     $  AIRCFT
        WRITE(45,*) " "
        WRITE(45,*) "ja "
        write(45,*) 'cat << "EOF" >! .ntwkparms'
        write(45,'(" SCI=",A4)')
     $  ACCNT
        write(45,'(" PROJ=41113",A3)')
     $  aircft
        write(45,*) '"EOF"'
        WRITE(45,*) " "
	WRITE(45,'(" set DEST = ",A)') arg2       
        WRITE(45,'(" set SOURCE = /home/local/guint/adsraw")')        
	WRITE(45,'(" set USER = ",A8)') USER
        WRITE(45,'(" set HOST = chinook.atd.ucar.edu")')
        WRITE(45,'(" set TAPENO = ",A6)') TAPENO
        WRITE(45,'(" set PGRDATA = gpgetuds.data")')
        WRITE(45,'(" set CALOUT = calcoe.",A3)') PROJNO
        WRITE(45,'(" set ADSUD = adsud.",A3)') PROJNO
        WRITE(45,'(" set ADSDAT = header.",A3)') PROJNO
        call caps (FLTNO)
        IF (FLTNO(5:5) .EQ. " ") THEN
          WRITE(45,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A4,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
        ELSE
          WRITE(45,'(" set MSSFILE = /RAF/19",I2,"/",A3,"/",A5,"/",
     $    A6)') YEAR, PROJNO, FLTNO, TAPENO
        ENDIF
c        WRITE(45,'(" set FILEFLT = gpadsout",A3,".",A5)')
c     $  PROJNO, FLTNO
        WRITE(45,'(" set PROGM = gpadsraw")')

c    **************************************
  100 CONTINUE                                                                  
      END                                                                       
