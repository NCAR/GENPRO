C                                                                               
C   Invoked only from GPSAVFIL Exec, use to save a given type of file           
C    to MS and D1                                                               
C                                                                               
C                                                                               
      CHARACTER *85 FLNM                                                    
      CHARACTER *3 VADDR,AIR,PROJNO                                             
      CHARACTER *4 ACCT                                                         
      CHARACTER *1 FLMD                                                         
      CHARACTER *5 USER                                                         
      character*8 flty
      CHARACTER *35 TITLE,FMT                                                   
      CHARACTER *38 PATH                                                        
      character*80 fulpth,arg2
      integer lndex
           


      flty = 'none'
      flmd= 'X'
      
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
      open (unit=40,file=fulpth,access='sequential')
C--------------------------------------start to open 13
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calibs.'
      fulpth(lindex+9:lindex+11) = projno
C---------------------start to open 13
      open (unit=13,file=fulpth,access='sequential')
C--------------------------------------start to open 11
      do 15, i = 1, 80
         fulpth(i:i) = ' '
 15   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/tempxx'
C-----------------------start to open 11
      open (unit=11,file=fulpth,access='sequential')
C--------------------------------------start to open 12
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/savefile.app'
C-----------------------start to open 12
      open (unit=12,file=fulpth,access='sequential')
C--------------------------------------start to open 14
      do 17, i = 1, 80
         fulpth(i:i) = ' '
 17   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/savefile.pre'
C-----------------------start to open 14
      open (unit=14,file=fulpth,access='sequential')

      lenpth = index(path,' ')
      if (lenpth .eq. 0) lenpth = 38
      lentit = index(title,' ')
      if (lentit .eq. 0) lentit = 35
C     WRITE(6,'('' Lenpath: '',I2,'' lenTitle: '',I2,/,''Path: '',A50,          
C    $ /,'' TITLE: '',A40)') LENPTH,LENTIT ,PATH ,TITLE                         
C Derive aircraft number from project number                                    
      IF(PROJNO(1:1).NE.'2') THEN                                               
       AIR='30'//PROJNO(1:1)                                                    
      ELSE                                                                      
       AIR='312'                                                                
      ENDIF                                                                     
C get user name and scientist number from Summary file                          
      REWIND(40)                                                                
      READ(40,'(17X,A4)') ACCT                                           
      USER= 'CHINK'
C-----------Use above to indicate that files come from chinook
C     READ(40,'(A8,9X,A4)') USER,ACCT                                           
C create JOB statement for JCL                                                  
      WRITE(14,'(                                                               
     $ '' JOB,JN=SAVFIL,US='',A4,''41113'',A3,'',*D1,*MS,CL=EX1.'',/,     
     $ '' ACCOUNT,AC='',A4,''41113'',A3,''.'')')ACCT,AIR,ACCT,AIR  
   5  READ (11,'(85a)',END=100) FLNM                    
      call flxt(flnm,lndex)
      WRITE(12,'('' GETFILE,('',A25,''),^'')')  FLNM(lndex:lndex+24) 
      write(12,'(1x,a4,'','',a1,'','',A5,''.'')') flty,flmd,USER    
      GOTO 5                                                                    
  100 CONTINUE                                                                  
      WRITE(12,'('' SAVEFILE,^'')')                                        
      WRITE(12,'(1x,(A),'',^'')') PATH(1:LENPTH)                              
      WRITE(12,'(1x,(A),''.'')') TITLE(1:LENTIT)             
      END                                                                       
      


      subroutine flxt(flnm,lndex)

      character*85 flnm
      integer lndex, ind2, ind3


      ind2 = index(flnm,'/')
  10  lndex = index(flnm(ind2+1:85),'/')
      if (lndex .ne. 0) then
         ind2 = lndex + ind2
         goto 10
      endif
      lndex = ind2 + 1
      return
      end
