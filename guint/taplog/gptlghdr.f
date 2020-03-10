C   Extract pertinent information from file of TDUMP file to create             
C   the header for Production Tape Log document (TAPLOG file)                   
C                                                                               
C   The entire functionality of this routine is based on the output             
C   produced by TDUMP FORTRAN. This output must be downloaded into              
C   fixed-80 format onto a CMS disk.                                            
      CHARACTER *80  RECORD,fulpth,arg2
      CHARACTER *60  MSPATH                                                     
      CHARACTER *16  SCNTST(8),PRGMR(8),PRNAME                                  
      CHARACTER *6 HLGBIT,HDTSIZ,LOGBIT,DATSIZ,DATLOG                           
      CHARACTER *4 BPI                                                          
      CHARACTER *3 PRNUM,AIRCFT,TURBRT,HNMPHY                                   
      CHARACTER *2 WRDSIZ,TRACK                                                 
      CHARACTER *1 CYCTIM                                                       
      INTEGER YEAR                                                              
      DATA NMSCN/0/ NMPRG/0/ TRACK/'9'/ BPI/'6250'/                             
C   Unit 19: TDUMP file (input)  Unit 20: TAPLOG file (output)                  
C   calling program must stack project number, name                             
      call getarg(1,prnum)
      call getarg(2,arg2)
      call getarg(3,prname)
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
C  derive aircraft number from project number                                   
      IF(PRNUM(1:1).NE.'2') THEN                                                
       AIRCFT='30'//PRNUM(1:1)                                                  
      ELSE                                                                      
       AIRCFT='312'                                                             
      ENDIF                                                                     
C--------------------------------------start to open 20
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ') - 1
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/taplog.'
      fulpth(lindex+9:lindex+11) = prnum
      fulpth(lindex+12:lindex+13) = 'hd'
      open (unit=20,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 19
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/tdump.'
      fulpth(lindex+8:lindex+10) = prnum
      fulpth(lindex+11:lindex+13) = 'log'
      open (unit=19,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 30
      do 24, i = 1, 80
         fulpth(i:i) = ' '
 24   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = prnum
      open (unit=30,file=fulpth,access='sequential',
     $          form='formatted')
C go IA to get scientist names, programmer names, output tape track and         
C density specifications, and MS file path name                                 
      CALL system("clear")            
      WRITE(6,'('' Enter Scientist Name(s) - terminate with <r>:''//)')         
    1 READ(5,'(A16)',END=100) SCNTST(NMSCN+1)                                   
      if (scntst(nmscn+1) .eq. "") goto 100
      NMSCN=NMSCN + 1                                                           
      GOTO 1                                                                    
  100 continue                                                                  
      IF (NMSCN.EQ.0) STOP                                                      
      CALL system("clear")                                                       
      WRITE(6,'('' Enter Programmer Name(s) -terminate with <r>:''//)')         
    2 READ(5,'(A16)',END=200) PRGMR(NMPRG+1)                                    
      if (prgmr(nmprg+1) .eq. "") goto 200
      NMPRG=NMPRG + 1                                                           
      GOTO 2                                                                    
  200 continue                                                                  
      IF (NMPRG.EQ.0) STOP                                                      
      CALL system("clear")                                                       
      WRITE(6,'('' Enter Output tape # of tracks - default 9:''//)')            
      READ(5,'(A2)',END=300) TRACK                                              
      if(track .eq. "") track = ' 9'
  300 continue                                                                  
      CALL system("clear")                                                       
  305 WRITE(6,'('' Enter Output tape Density BPI - default 6250''//)')          
      READ(5,'(A4)',END=400) BPI                                                
      if(bpi .eq. "") bpi = '6250'
  400 continue                                                                  
  405 CALL system("clear")                                                       
C get Turbulence rate                                                           
      REWIND(30)                                                                
      READ(30,'(11X,A3)')  TURBRT                                               
C get project year                                                              
      CALL GTYEAR(YEAR,30)                                                      
      WRITE(MSPATH,'(''/RAF/19'',I2,''/'',A3,A3,                                
     $''/RFxx/'')') YEAR,PRNUM,TURBRT                         
C get remainder of info from Tdump file                                         
      CALL system("clear")                                                       
      WRITE(6,'('' Extracting info from Tdump file''//)')                       
      REWIND (19)                                                               
    5 READ(19,'(A80)',END=150) RECORD                                           
      IF(INDEX(RECORD,'CYCLE INTERVAL IS ').EQ.0) GOTO 5                        
      CYCTIM=RECORD(74:74)                                                      
   15 READ(19,'(A80)',END=150) RECORD                                           
      IF(INDEX(RECORD,'PHYSICAL RECORDS IN THE HEADER').EQ.0) GOTO 15           
C Number of Physical records in header                                          
      HNMPHY=RECORD(12:14)                                                      
      READ(HNMPHY,'(BN,I3)') NHDREC                                             
   20 READ(19,'(A80)',END=150) RECORD                                           
      IF(INDEX(RECORD,'HEADER FILE').EQ.0) GOTO 20                              
C Logical record size of header                                                 
      HLGBIT=RECORD(25:30)                                                      
      READ(19,'(A80)',END=150) RECORD                                           
C Physical record size of header                                                
      READ(19,'(24X,A6)',END=150) HDTSIZ                                        
C Data Logical record size                                                      
      READ(19,'(24X,A6)',END=150) LOGBIT                                        
C # logical records per data physical record                                    
      READ(19,'(24X,A6)',END=150) DATLOG                                        
C Data physical record size                                                     
      READ(19,'(24X,A6)',END=150) DATSIZ                                        
C Word size                                                                     
   25 READ(19,'(A80)',END=150) RECORD                                           
      IF(INDEX(RECORD,'BIT INTEGER WORDS').EQ.0) GOTO 25                        
      WRDSIZ=RECORD(33:34)                                                      
C   write all pertinent info to output file                                     
      WRITE(20,'(//,26X,''PRODUCTION TAPE LOG'',//,                             
     $''  Project no.: '',A3,2X,'' Project name:  '',A16,/,             
     $''  Aircraft:    '',A3,2X,'' Scientist(s): '',(T36,A16,1X,A16))')   
     $ PRNUM,PRNAME,AIRCFT,(SCNTST(J),J=1,NMSCN)                                
      WRITE(20,'(''  Data type: '',2X,A3,2X,                                 
     $'' Programmer(s): '',(T36,A16,1X,A16))')               
     $ TURBRT,(PRGMR(J),J=1,NMPRG)                                              
      WRITE(20,'(/,''  OUTPUT TAPE SPECIFICATIONS:'',/                   
     $,''  Track: '',A2,2X,'' Density: '',A4,2X,'' Word size: '',A2    
     $,'' bits/word'',/,''  MSS path name: '',A60,//                 
     $,''  HEADER FILE INFORMATION:'',/                    
     $,''  Logical record size: '',A6                         
     $,'' bits'',6X,''Physical record size: '',A6,'' bits''                     
     $/,''  No. of physical records: '',I3,/         
     $)')                                                                       
     $TRACK,BPI,WRDSIZ,MSPATH,HLGBIT,HDTSIZ,NHDREC-1                            
C  above 'NHDREC-1' is done because Tdump is long by one (Celia Chen)           
      WRITE(20,'(                                                               
     $ ''  DATA FILE INFORMATION:'',/                                           
     $,''  Logical record size: '',A6                 
     $,'' bits'',6X,''Physical record size: '',A6,'' bits'',/                   
     $,''  No. of Logical records per physical record: '',A6,/    
     $,''  No. of seconds per logical record (GENPRO cycle time): '',A2     
     $)') LOGBIT,DATSIZ,DATLOG,CYCTIM                                           
      WRITE(20,'(                                                               
     $//,72(''-''),/,                                                           
     $''  Fltno   Date   Input  Output    Start       ''           
     $,''Stop     Start  Stop''                  
     $,/,72(''-''),/                                                            
     $)')                                                                       
      STOP                                                                      
C   exceptions                                                                  
  150 WRITE(6,'('' DATA NOT FOUND '')')                                         
      write(6,'('' hit <r> to continue'')')
      read(5,'(i1)') ll
 1000 continue                                                                  
      END                                                                       
