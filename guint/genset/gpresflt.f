      SUBROUTINE RESFLT                                                         
C                                                                               
C   restore a flight setup                                                      
C                                                                               
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gpio.h"                                                          
      INCLUDE "gppdata.h"                                                       
      INTEGER INTERR                                                            
      COMMON/IOERR/INTERR                                                       
      LOGICAL IVIEW                                                             
      CHARACTER*5 TMPFLT                                                        
      CHARACTER*40 TMPTIT                                                       
      CHARACTER*50 TMPDES                                                       
C  character strings for months of year                                         
      CHARACTER*3 MONTHS(12)                                                    
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',        
     $            'OCT','NOV','DEC'/                                            
C  IVIEW is true only if user wants to examine a specific flight setup          
      IVIEW=.FALSE.                                                             
C Unit 20 is file of saved flight setups                                        
C--------------------------------------start to open 20
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/fltsav.'
      fulpth(lindex+9:lindex+11) = iproj
C-----------------------open 20
      open (unit=20,file=fulpth,iostat=ierr,
     $form='unformatted',status='old',err=18711)
C     OPEN(20,FILE='FLTSAV',IOSTAT=IERR,STATUS='OLD',ERR=18711,                 
C    $FORM='UNFORMATTED')                                                       
18711 IF(IERR.NE.0)  THEN                                                       
       IERR=0                                                                   
       WRITE(6,'('' No flight setup file saved .. <r> to continue'')')          
       READ(LUTI,'(A)',END=18712) I                                             
18712  continue                                                             
       CALL INIFLT                                                              
       CLOSE(20)                                                                
       RETURN                                                                   
      ENDIF                                                                     
C  save current flight identifier variables                                     
C--------------------------------------start to open 26
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+13) = '/scratch.file'
C-----------------------open 26
      open (unit=26,file=fulpth,iostat=ierr,form='unformatted')
C     OPEN(26,FORM='UNFORMATTED')                                               
      REWIND(26)                                                                
      WRITE(26) C1,C2,C3,SDINAM,NFULL,ITMSEG,INMSEG,NUMVOL,                     
     $TAPNO,IDATEF,ITIMEF,QCREF                                                 
C  Offer menu of all saved setups to facilitate choice                          
 6766 CALL system("clear")                                                      
      REWIND(20)                                                                
      WRITE(LUTO,67660)                                                         
67660 FORMAT(' FLIGHT SETUPS MENU',//)                                          
      J=0                                                                       
67661 READ(20,END=9493) TMPDES,TMPFLT                                           
      J=J+1                                                                     
      WRITE(LUTO,67662) J,TMPDES,TMPFLT                                         
67662 FORMAT(1x,I2,':',1X,A50,' ( Flight ',A5,')')      
      GOTO 67661                                                                
 9493 IF (J.EQ.0) THEN                                                          
       CALL PAUSER('  No flights saved yet...hit <r> to continue')              
       CALL INIFLT                                                              
       RETURN                                                                   
      ENDIF                                                                     
      WRITE(LUTO,9494)                                                          
 9494 FORMAT(/,' Select setup # to examine, <cr> to skip it: ')                 
19494 READ(LUTI,'(BN,I2)',END=8887,err=71111) IREC                              
      if (irec .eq. 0) goto 8887
      goto 71112
71111  WRITE(6,'('' Error on input; try again'')')                           
       GOTO 19494                                                               
71112 IF(IREC.LT.1.OR.IREC.GT.J) THEN                                           
       CALL PAUSER('  Bad choice. Try again')                                   
       GOTO 6766                                                                
      ENDIF                                                                     
      IVIEW=.TRUE.                                                              
C  display loop...                                                              
      REWIND(20)                                                                
      DO 9495 J=1,IREC-1                                                        
 9495  READ(20,END=6766)                                                        
      READ(20,END=6766) TMPDES,TMPFLT,C1,C2,C3,SDINAM,NFULL,                    
     $ ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                               
     $ NUMVOL,TAPNO,TAPE1,OUTPUT,GAP,IDATEF,ITIMEF,QCREF,USRTIT                 
      CALL system("clear")                                                      
      WRITE(LUTO,10000) TMPDES,TMPFLT                                           
      WRITE(LUTO,10003) IDATEF,ITIMEF,QCREF                                     
      WRITE(LUTO,10001) ((ITMSEG(I,J),I=1,6),J=1,INMSEG)                        
      WRITE(LUTO,10002) NSXIN,(TAPNO(J),J=1,NUMVOL)                             
10000 FORMAT(/,' FLIGHT SETUP: ',A50,/,'    Flight ',A5)                        
10001 FORMAT(/,15X,' Input Snapshots: ',/,                                      
     $ (18X,I2,':',I2,':',I2,4X,I2,':',I2,':',I2))                              
10002 FORMAT(/' Input snapshot period: ',I6, ' seconds',/,                      
     $' Input Volume(s): ',/,                                                   
     $(A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X))                  
10003 FORMAT(/'  Flight Date and Time: ',I2,2('/',I2),2X,I2,2(':',I2),          
     $' to ',I2,2(':',I2),/,                                                    
     $'  QC reference sensors: ',A3,' and ',A3)                                 
      CALL PAUSER(' Hit <cr> to examine calibration coefficients')              
      CALL DISCAL(.TRUE.)                                                       
C     CALL PAUSER(' Hit <cr> to return to menu of setups')                      
      GOTO  6766                                                                
C select setup to restore                                                       
 8887 continue                                                              
      WRITE(LUTO,8888)                                                          
 8888 FORMAT(' Specify setup # to restore, <cr> to skip it: ')                  
18888 READ(LUTI,'(BN,I2)',END=77777,err=71113) IREC                   
      if (irec .eq. 0) goto 77777
      goto 77787
71113  WRITE(6,'('' Error on input; try again'')')                           
       GOTO 18888                                                               
77777 continue                                                              
      IF (IVIEW) THEN                                                           
C  restore current flight identifiers that were used in display loop            
       REWIND(26)                                                               
       READ(26) C1,C2,C3,SDINAM,NFULL,ITMSEG,INMSEG,                            
     $ NUMVOL,TAPNO,IDATEF,ITIMEF,QCREF                                         
       CLOSE(26)                                                                
      ENDIF                                                                     
      RETURN                                                                    
C  do the flight setup restore                                                  
77787 WRITE(LUTO,77997)                                                         
77997 FORMAT(' Select restore field(s): (1) general (2) coefficients',          
     $ /,    '                          (3) both   <r> = both')                 
87997 READ(LUTI,'(BN,I2)',END=77998,err=71114) IFIELD             
      if (ifield .eq. 0) goto 77998
      goto 77999
71114  WRITE(6,'('' Non-integer input; try again'')')                           
       GOTO 87997                                                               
77998 continue                                                                  
      IFIELD=3                                                                  
C  position file at selected record                                             
77999 REWIND(20)                                                                
      DO 8889 J=1,IREC-1                                                        
 8889  READ(20)                                                                 
      REWIND(26)                                                                
C  replace all fields and then restore original values from unit 26             
C  for those fields not designated for replacement                              
C ...in any event, current values of DESCRP and IFLGHT are to be retained       
C  unless this is an initial restore ==> give 'em values from file              
      IF (IFIELD.EQ.1) THEN                                                     
C  replace only general setup items                                             
       READ (20,IOSTAT=IERR)DESCRP,IFLGHT,C1,C2,C3,SDINAM,NFULL,                
     $ ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                               
     $ NUMVOL,TAPNO,TAPE1,OUTPUT,GAP,IDATEF,ITIMEF,QCREF,USRTIT,                
     $ NSXOUT,NSXIN,IVTITL                                                      
       CALL SETTIT                                                              
C  which means recover coefficients:                                            
       READ(26) C1,C2,C3,SDINAM,NFULL                                           
      ELSEIF (IFIELD.EQ.2) THEN                                                 
C  replace the coefficients only                                                
       READ (20,IOSTAT=IERR)TMPDES,TMPFLT,C1,C2,C3,SDINAM,NFULL                 
       CALCHG=.TRUE.                                                            
      ELSEIF (IFIELD.EQ.3) THEN                                                 
C  replace both general items and coefficients                                  
       READ (20,IOSTAT=IERR)DESCRP,IFLGHT,C1,C2,C3,SDINAM,NFULL,                
     $ ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                               
     $ NUMVOL,TAPNO,TAPE1,OUTPUT,GAP,IDATEF,ITIMEF,QCREF,USRTIT,                
     $ NSXOUT,NSXIN,IVTITL                                                      
       CALCHG=.TRUE.                                                            
       CALL SETTIT                                                              
      ENDIF                                                                     
      CLOSE(20,IOSTAT=IERR)                                                     
      CLOSE(26)                                                                 
 8998 FORMAT(9A8)                                                               
C  if restore on program entry but user chose cal. coeff's only, restore        
C  flight number and description anyway                                         
      IF (.NOT.(FLTSET).AND.IFIELD.EQ.2) THEN                                   
       IFLGHT=TMPFLT                                                            
       DESCRP=TMPDES                                                            
      ENDIF                                                                     
      FLTSET=.TRUE.                                                             
      FLTSAV=.TRUE.                                                             
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PAUSER(STRING)                                                 
      INCLUDE "gpio.h"                                                          
      CHARACTER *(*) STRING                                                     
C                                                                               
C  display STRING to output LU and wait for <r> to continue                     
C                                                                               
      WRITE(LUTO,'(A)') STRING                                                  
      READ(LUTI,'(A)',END=100) IWAIT                                            
  100 continue                                                             
      RETURN                                                                    
      END                                                                       
