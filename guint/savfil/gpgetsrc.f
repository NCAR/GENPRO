C  Read a CALIBS <project number> file, unit 13, extracting all                 
C  the fileids for the calibration source files, assumed to be acquired         
C  with the GETSRC procedure in project deck. Write the command lines           
C  to a temp file to specify the file id and user name. Assumed called          
C  only from GPSAVFIL EXEC, and CALIBS file set up by SR CALIBS in              
C  GPCALVEC FORTRAN file                                                        
                                                                                
      CHARACTER *80 RECORD                                                      
      CHARACTER *38 PATH                                                        
      CHARACTER *35 TITLE                                                       
      CHARACTER *8 FLNM,FLTY,USER                                               
      CHARACTER *4 ACCT                                                         
      CHARACTER *3 FLMD,AIR,PROJNO                                              
      character*80 fulpth,arg2
      
      call getarg(1,projno)
      call getarg(2,arg2)
      call getarg(3,path)
      call getarg(4,title)
      lindex = index(arg2,' ') -1 
C--------------------------------------start to open 40
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = projno
C-----------------start to open 40
      open (unit=40,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 13
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calibs.'
      fulpth(lindex+9:lindex+11) = projno
C---------------------start to open 13
      open (unit=13,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 11
      do 15, i = 1, 80
         fulpth(i:i) = ' '
 15   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/tempxx'
C-----------------------start to open 11
      open (unit=11,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 12
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/savefile.app'
C-----------------------start to open 12
      open (unit=12,file=fulpth,access='sequential',
     $          form='formatted')
C--------------------------------------start to open 14
      do 17, i = 1, 80
         fulpth(i:i) = ' '
 17   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/savefile.pre'
C-----------------------start to open 14
      open (unit=14,file=fulpth,access='sequential',
     $          form='formatted')

      lenpth = index(path,' ')
      if (lenpth .eq. 0) lenpth = 38
      lentit = index(title,' ')
      if (lentit .eq. 0) lentit = 35
C Derive aircraft number from project number                                    
      IF(PROJNO(1:1).NE.'2') THEN                                               
       AIR='30'//PROJNO(1:1)                                                    
      ELSE                                                                      
       AIR='312'                                                                
      ENDIF                                                                     
C get user name and scientist number from Summary file                          
      REWIND(40)                                                                
      READ(40,'(A8,9X,A4)') USER,ACCT                                           
C create JOB statement for JCL                                                  
      WRITE(14,'(                                                               
     $ '' JOB,JN=SAVFIL,US='',A4,''41113'',A3,'',*D1,*MS,CL=EX1.'',/,       
     $ '' ACCOUNT,AC='',A4,''41113'',A3,''.'')')ACCT,AIR,ACCT,AIR    
    5 READ (13,'(A80)',END=500) RECORD                                          
C  extract filename, filetype, and user machine                                 
      IPTR=INDEX(RECORD,'&PROJECT,')                                            
C XTRACT starts at the position indicated with 1st arg, extracts string         
C up to character indicated by second arg, returns the string and pointer       
C to the next character                                                         
      CALL XTRACT(IPTR+9,',',RECORD,FLNM,IPTR)                                  
      CALL XTRACT(IPTR+1,',',RECORD,FLTY,IPTR)                                  
      CALL XTRACT(IPTR+1,',',RECORD,USER,IPTR)                                  
      CALL XTRACT(IPTR+1,'.',RECORD,FLMD,IPTR)                                  
      WRITE(12,'('' GETFILE,'',2(A8,'',''),A3,'','',A8,''.'')')          
     $ FLNM, FLTY, FLMD, USER                                            
      GOTO 5                                                                    
  500 CONTINUE                                                                  
      WRITE(12,'('' SAVEFILE,^'')')                    
      WRITE(12,'(1x,(A),'',^'')') PATH(1:LENPTH)                
      WRITE(12,'(1x,(A),''.'')') TITLE(1:LENTIT)               
      END                                                                       
      SUBROUTINE XTRACT(IPTR,CHAR,WORD,SUBWRD,NXTPTR)                           
C   Extract all characters starting at position IPTR up to but not              
C   including the character CHAR from string WORD. Return the subword           
C   in SUBWRD and the position of CHAR in NXTPTR                                
      CHARACTER *(*) WORD                                                       
      CHARACTER *(*) SUBWRD                                                     
      CHARACTER * 1 CHAR                                                        
      ITMP=IPTR                                                                 
      LNTH=LEN(WORD)                                                            
C     WRITE(6,'('' SEARCH: '',(A))') WORD(ITMP+1:LNTH)                          
      NXTPTR=INDEX(WORD(ITMP+1:LNTH),CHAR) + ITMP                               
      SUBWRD=WORD(ITMP:NXTPTR-1)                                                
C     WRITE(6,'('' SUBWORD: '',(A))') SUBWRD                                    
      RETURN                                                                    
      END                                                                       
