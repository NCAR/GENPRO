      SUBROUTINE USRVAR                                                         
      INCLUDE "gpio.h"                                                
      INCLUDE "gppdata.h"                                             
      INCLUDE "gpifile.h"                                                     
      INCLUDE "gpusrvar.h"                                                
C                                                                               
C  add user defined variables to array DERIV, and obtain accompanying           
C  title, units, and derivations                                                
C                                                                               
      CHARACTER *125 VECTOR                                                     
      EQUIVALENCE (VECTOR,SOURCE)                                               
      CHARACTER *40 USRT2                                                      
      CHARACTER *8 SOURCE(9),tname                                              
      character*4 tunit
      CHARACTER *1 CHNMSO,choice                                                
      logical      raw,cook,cook2

      inquire(unit=61,opened=raw)
      inquire(unit=62,opened=cook)
      cook2 = .false.
 10   CALL system("clear")                                                      
      write(luto,'('' Do you wish to add a new'',/,10x,''1) Raw '',
     $''Variable'',/,10x,''2) Derived Variable'',/,10x,
     $''3) Exit'')')
      read(luti,'(a1)',end=1000) choice
      if (choice .eq. '2') then
          cook2 = .true.
         if (.not. raw) then
C--------------------------------------start to open 61
           do 13, i = 1, 80
              fulpth(i:i) = ' '
 13        continue
           lindex = index(arg2,' ')
           lindex = lindex -1
           fulpth(1:lindex) = arg2(1:lindex)
           fulpth(lindex+1:lindex+11) = '/gpvar.temp'
           open (unit=61,file=fulpth,access='sequential',recl=80)
         endif
         if (.not. cook) then
           do 14, i = 1, 80
              fulpth(i:i) = ' '
 14        continue
           lindex = index(arg2,' ')
           lindex = lindex -1
           fulpth(1:lindex) = arg2(1:lindex)
           fulpth(lindex+1:lindex+14) = '/gpgender.temp'
           open (unit=62,file=fulpth,access='sequential',recl=80)
           do 16, i = 1, 80
              fulpth(i:i) = ' '
 16        continue
           lindex = index(arg2,' ')
           lindex = lindex -1
           fulpth(1:lindex) = arg2(1:lindex)
           fulpth(lindex+1:lindex+14) = '/gpderive.temp'
           open (unit=63,file=fulpth,access='sequential',recl=80)
         endif
         WRITE(LUTO,'('' Variable Name (8 chars or less): '')')         
         READ(LUTI,'(A8)',END=1000)  tname
         if (tname .eq. "") goto 1000
         call caps (tname,8)
         DERIV(NDERIV+1)  = tname           
C add it to other derived vars list                                   
         NDERIV=NDERIV+1                                             
         NMODE3=NMODE3+1                                            
         MODE3(NMODE3)=NMODE3                                      
C add it to the dictionary of names and units also                              
         DO 25 J=1,NUMBV                                       
          IF (VNAME(J).EQ.DERIV(NDERIV)) GOTO 30               
   25    CONTINUE                                              
         NUMBV=NUMBV+1                                         
         VNAME(NUMBV)=DERIV(NDERIV)                            
   30    WRITE(LUTO,'('' Variable Units (4 chars or less): '')') 
         READ(LUTI,'(A4)') tunit                        
         call caps (tunit,4)
         vunit(numbv) = tunit
         WRITE(LUTO,'('' Title (40 chars or less): '')')        
         READ (LUTI,'(A40)') USRT2                            
         call caps (usrt2,40)
C  get source variables for its derivation                    
         do 49, j=1,5
         source(j) = ""
  49     continue
         NUMSOU=0                                              
         WRITE(LUTO,'('' Specify source vars (5 max);'',
     $   '' <cr> when done: '')')  
  50     READ(LUTI,'(A8)',END=500) SOURCE(NUMSOU+1)                         
         if (source(numsou+1) .eq. "") goto 500
         NUMSOU=NUMSOU+1                                                   
         IF (NUMSOU.EQ.5) GOTO 500                                        
         GOTO 50                                                         
 500     do 51, j=1,5
            call caps(source(j),8)
  51     continue
         write(61,'(1x,a40,1x,a4,5x,a8)')usrt2,vunit(numbv),
     $   deriv(nderiv)
         write(62,'(1x,(5(a8)),1x,i1,1x,a8)') (source(j), j=1, 5),
     $   numsou,deriv(nderiv)
         write(63,'(1x,(5(a8)),1x,i1,1x,a8)') (source(j), j=1, 5),
     $   numsou,deriv(nderiv)
C  pack info into USRVEC: name,units,title,# of source vars,source vars
C        WRITE(CHNMSO,'(A1)') NUMSOU                                  
C        NMUSRV=NMUSRV + 1                                           
C        USRVEC(NMUSRV)=DERIV(NDERIV)//VUNIT(NUMBV)//USRT2
C    $   //CHNMSO//VECTOR     
         GOTO 10                                                               
      endif
      if (choice .eq. '1') then
         if (.not. raw) then
C--------------------------------------start to open 61
            do 17, i = 1, 80
               fulpth(i:i) = ' '
 17         continue
            lindex = index(arg2,' ')
            lindex = lindex -1
            fulpth(1:lindex) = arg2(1:lindex)
            fulpth(lindex+1:lindex+11) = '/gpvar.temp'
            open (unit=61,file=fulpth,access='sequential',recl=80)
         endif
         write(luto,'('' This option is used to include titles and'',
     $   '' units for raw variables'',/,'' the variable itself must'',
     $   '' be available. Hit <r> to quit.'')')
         WRITE(LUTO,'('' Variable Name (8 chars or less): '')')         
         READ(LUTI,'(A8)',END=1000) tname               
         if (tname .eq. "") goto 1000
         call caps(tname,8)
C find it in the dictionary of names and units also               
         DO 26 nloc=1,NUMBV                                       
          IF (VNAME(nloc).EQ.tname) goto 31
   26    CONTINUE                                              
         write(luto,'('' Variable '',a8,'' not found in header'',/,
     $   '' Check your data.  Hit <r> to continue.'')') tname
         read (luti,'(a)')
         goto 10
   31    WRITE(LUTO,'('' Variable Units (4 chars or less): '')') 
         READ(LUTI,'(A4)') tunit                        
         call caps(tunit,4)
         vunit(nloc) = tunit
         WRITE(LUTO,'('' Title (40 chars or less): '')')        
         READ (LUTI,'(A40)') USRT2                            
         call caps(usrt2,40)
C  Write out data                    
         write(61,'(1x,a40,1x,a4,5x,a8)') usrt2,vunit(nloc),
     $   tname
         goto 10
      endif
 1000 continue                                                         
C     if (cook2) call getord
      call fndun
      RETURN                                                              
      END                                                                   
