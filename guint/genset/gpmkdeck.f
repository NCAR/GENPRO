        PROGRAM MKDECK
C                                                                               
C  This program is intended to produce                                          
C  project decks for RAF projects.                                              
C     It reads from various files and outputs a deck for a project.             
C     This program was derived from  GENIN 2.1 A.                               
C     Files used:                                                               
C     GENDER DBASE---Contains variables and derivations for CALIB               
C     INICAL DBASE---Contains variables and derivations for INICAL              
C     DERIVE DBASE---Contains all derivations for GENSET to determine           
C                    which variables to offer the user.                         
C     ADSUD (PROJNO)---Contains ADSRAW output for the INPUT op.                 
C     CALCOE (PROJNO)---Contains caibration coeffs. for CALIB op.               
C                                                                               
C  Version 1.0   Generates a project deck.                                      
C                Jeff Berry x1044.                                              
C                July, 1987                                                     
C  Version 1.1   Modified for first release of GPGENSET                         
C  Version 2.0   Rewrite, processing is now OUTPUT oriented instead of          
C                CALIB oriented.  Also, bugfix.                                 
C  Version 3.0   Conversion to run on MassComp (spock.atd.ucar.edu)
C  Version 3.1   Modularized, rewrite using includes for all common blocks
C
C  Version 4.0   Conversion to run on Sun (chinook.atd.ucar.edu)
C                                                                               
C-----------DESCRIPTIONS OF VARIABLES                                           
C  TITLE,UNITTS, VARNAM information from VAR DBASE file(VAREND)                
C  PRARR  variables requested by USER (PRNUM)                                   
C  NAME  variable names needed in CALIB  (NUMVAR)                               
C  DFNAME  contains the array that is {NAME}-{CNAME}=(DFNAME)                   
C  FNAME   contains variables with changes made for FUNCT(FNUM)                 
C  DTITLE,DNMVAR,NS,NAMC,DCONS,CONSU,and PRIM are the information               
C                      from GENDER DBASE                                        
C  CNAME, CNUMV are the variables with Calibration coeff.                       
C  TNAME, TRATE, TNUM are the variables from the header/adsraw,                 
C      thus the array contains all raw vars, and their rates.                   
C  KFLAG is set to indicate what plane is being flown.{2,7,8}                   
C  STDVAR is a list of all Bulletin 9 variables which appear in                 
C            in the header.                                                     
                                                                                
C 15   $workpath/gpvar.dba                                       
C 21   $genset/direct.$projno
C 24   $genset/gplookup.table
C 25   $workpath/adsud.$projno                              
C 35   $workpath/calcoe.$projno                             
C 45   $workpath/gpgender.dba                               
C 47   $genset/gpinical.dba                               
C 52   $genset/gpkey.dba                                  
C 63   $workpath/plot1.$projno                              
C 64   $workpath/plot2.$projno                              
C 65   $genset/gpprod.deck                                
C 85   $genset/gpdme.titles                               
C 94   $genset/gpdespik.lrn                               
C 95   $genset/gpdespik.pcl                               
C 96   $genset/gpterp2.opc                                
C 98   $workpath/claib.$projno                              
C 36   $workpath/addto.dba                                  
C 97   $workpath/$prname.$projno                            
C 99   $workpath/output.$projno
                                                                                
                                                                                
      include "gparc.h" 
      include "gpcalc.h" 
      include "gpder.h" 
      include "gpder2.h" 
      include "gpifile.h"
      include "gpkey.h" 
      include "gppdata.h"
      include "gpplrar.h" 
      include "gppms.h" 
      include "gprad.h" 
      include "gprat.h" 
      include "gpref.h" 
      include "gprfl.h" 
      include "gprfl2.h" 
      include "gpstd.h" 
      include "gpstv.h" 
C     include "gpusrvar.h"

C------------VARIABLES                                                          
      CHARACTER*1 CHOICE,DUMMY2                                                 
      CHARACTER*3 TURB
      character*9 prnam
      CHARACTER*80 LINE,gspath,arg2,fulpth                              
      CHARACTER*12 JOB                                                          
      INTEGER LRN1,DME,HGME,CHGME,THR,I,ATCNT,OUT,PMK,RDME,PLT2
      INTEGER HGCAL,VFLG,VFLG1,lk                           



      call getarg(1,iproj)
      call getarg(2,arg2)
      call getarg(3,prnam)
      call getenv('genset',gspath)
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
C-----------------------start to open 21
      do 18, i = 1, 80
         fulpth(i:i) = ' '
 18   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/direct.'
      fulpth(lindex+9:lindex+11) = iproj
      open (unit=21,file=fulpth,form='unformatted',access='sequential')
C--------------------------------------start to open 24
      mindex = index(gspath,' ') - 1
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+15) = '/gplookup.table'
      open (unit=24,file=fulpth,access='sequential',form='formatted')
C-------------start to open 15
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/gpvar.dba'
      open (unit=15,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 25
      do 32, i = 1, 80
         fulpth(i:i) = ' '
 32   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/adsud.'
      fulpth(lindex+8:lindex+10) =iproj
      open (unit=25,file=fulpth,access='sequential',form='formatted')
C-------------start to open 45
      do 33, i = 1, 80
         fulpth(i:i) = ' '
 33   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/gpgender.'
      fulpth(lindex+11:lindex+13) = "dba"
      open (unit=45,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 47
      do 34, i = 1, 80
         fulpth(i:i) = ' '
 34   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+10) = '/gpinical.'
      fulpth(mindex+11:mindex+13) ="dba"
      open (unit=47,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 52
      do 35, i = 1, 80
         fulpth(i:i) = ' '
 35   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+10) = '/gpkey.dba'
      open (unit=52,file=fulpth,access='sequential',form='formatted')
C-------------start to open 63
      do 36, i = 1, 80
         fulpth(i:i) = ' '
 36   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/plot1.'
      fulpth(lindex+8:lindex+10) =iproj
      open (unit=63,file=fulpth,access='sequential',form='formatted')
C-------------start to open 64
      do 39, i = 1, 80
         fulpth(i:i) = ' '
 39   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/plot2.'
      fulpth(lindex+8:lindex+10) =iproj
      open (unit=64,file=fulpth,access='sequential',form='formatted')
C-------------start to open 65
      do 37, i = 1, 80
         fulpth(i:i) = ' '
 37   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+12) = '/gpprod.deck'
      open (unit=65,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 85
      do 38, i = 1, 80
         fulpth(i:i) = ' '
 38   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+13) = '/gpdme.titles'
      open (unit=85,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 94
      do 43, i = 1, 80
         fulpth(i:i) = ' '
 43   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+13) = '/gpdespik.lrn'
      open (unit=94,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 95
      do 44, i = 1, 80
         fulpth(i:i) = ' '
 44   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+13) = '/gpdespik.pcl'
      open (unit=95,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 96
      do 45, i = 1, 80
         fulpth(i:i) = ' '
 45   continue
      fulpth(1:mindex) = gspath(1:mindex)
      fulpth(mindex+1:mindex+12) = '/gpterp2.opc'
      open (unit=96,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 98
      do 46, i = 1, 80
         fulpth(i:i) = ' '
 46   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calibs.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=98,file=fulpth,access='sequential',form='formatted')
C-------------start to open 36
      do 47, i = 1, 80
         fulpth(i:i) = ' '
 47   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/addto.dba'
      open (unit=36,file=fulpth,access='sequential',form='formatted')
C-------------start to open 97
      do 48, i = 1, 80
         fulpth(i:i) = ' '
 48   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      lldex =index(prnam,' ') 
      fulpth(lindex+1:lindex+1) = '/'
      fulpth(lindex+2:lindex+lldex) = prnam(1:lldex)
      fulpth(lindex+1+lldex:lindex+1+lldex) = '.'
      fulpth(lindex+2+lldex:lindex+4+lldex) = 'job'
      open (unit=97,file=fulpth,access='sequential',form='formatted')
C-------------start to open 99
      do 49, i = 1, 80
         fulpth(i:i) = ' '
 49   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/output.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=99,file=fulpth,access='sequential',form='unformatted')
C-------------start to open 35
      do 51, i = 1, 80
         fulpth(i:i) = ' '
 51   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calcoe.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=35,file=fulpth,access='sequential',form='formatted')
                                                                                
                                                                                
                                                                                
 1111 FORMAT(A80)                                                               
 2111 FORMAT(1x,A80)                                                 
 1112 FORMAT(A12)                                                               
 1113 FORMAT(' RATE=',I4,',%FOR,THF')                  
 1234 FORMAT(' YAXIS = RHOLA1,RFLAG1,%FOR,PDLA1')               
 1235 FORMAT (' YAXIS = RHOLA,RFLAG,%FOR,PDLA')                   
 2010 FORMAT(A)                                                 
 2015 FORMAT (' ORDVAR=   ',A5,A34, ', ', A5, A11)                
 2020 FORMAT (1X,' ORDFUN=',A6,' ,',A6,'  ,',A8,'       ,       ',A6)
                                                                                
C-----------Load GPVAR DBASE                                                    
      CALL RDBASE                                                               
C----------Initalize a series of flags                                          
      KFLAG ='0'                                                                
      LRN1 = 0                                                                  
      HGME = 0                                                                  
      CHGME = 0                                                                 
      DME=0                                                                     
      THR=0                                                                     
                                                                                
                                                                                
   08 PRINT *                                                                   
                                                                                
      DUMMY2 = ' '                                                  
      PRINT *, 'Processing.....'                                                
C----------Load Calibration Coeff. Variables                                    
      CALL RDCAL(HGCAL)                                                         
      CALL RCLR(CNAME,CNUMV,600)                                                
      REWIND(35)                                                                
C----------Load GPGENDER DBASE                                                  
      CALL RDDERV                                                               
C---------Load Variables  from ADSUD file, set series of flags.                 
C------------------This also sets raw rates                                     
      CALL RADSUD(LRN1,DME,HGME,CHGME,THR,PMK,lk)                               
C--------SETS ARRAY FOR OUTPUT                                                  
      CALL STDSET                                                               
C------------Reads DIRECT FILE.                                                 
      CALL MEMRD(OUT,JOB,THR,PMK,DME,PLT2,TURB)                                 
C----------MEMRD may not be called before RADSUD, or the values for             
C-------------THR, PMK, and DME will be wrong.                                  
      IF (OUT .EQ. 1) GOTO 400                                                  
      CALL RCLR(NAME,NUMVAR,600)                                                
C----------Rate matching.                                                       
      CALL SRT                                                                  
      CALL DFSET                                                                
C-----------Prepare derivations.                                                
      CALL DFIX                                                                 
                                                                                
   12 READ(65,1111,END=400)LINE                                                 
C--------Clears preliminary gobbledy-gook from file                             
      IF (LINE(1:3) .NE. 'JOB') GOTO 12                                         
                                                                                
      LINE(19:30) = JOB                                                         
      WRITE(97,2111)LINE                                                        
      READ(65,1111)LINE                                                         
      LINE(12:23) = JOB                                                         
      WRITE(97,2111)LINE                                                        
                                                                                
C--------Sets rates for use in CALIB                                            
      REWIND(25)                                                                
      VFLG=0                                                                    
      VFLG1=0                                                                   
      IF (TURB .EQ. 'HRT') THEN                                                 
         CALL RTSORT(VFLG,VFLG1)                                                
      ELSE                                             
         DO 67, J=1, NUMVAR                                                     
            RATE(J) = 1                                                         
            IF (NAME(J) .EQ. 'RHOLA   ') VFLG=1                                 
            IF (NAME(J) .EQ. 'RHOLA1  ') VFLG1=1                                
   67    CONTINUE                                                               
      ENDIF                                                                     
                                                                                
                                                                                
      PRINT *,'Still Processing.....'                                           
      ATCNT = 0                                                                 
                                                                                
C-------Reads and copys the Master file, line by line. Checks each              
C-------ATTENTION keyword, and takes appropraite action.                        
                                                                                
      WRITEN = 1                                                                
   10 READ(65,1111,END=400) LINE                                                
      IF (WRITEN .GE. 1)  WRITE(97,2111) LINE                                   
      IF (INDEX(LINE,'ATTENTION') .NE. 0) THEN                                  
         ATCNT = ATCNT+1                                                        
C----------COPYS CALIBS &PROJNO                                                 
         IF (ATCNT .EQ. 1) CALL COPY(98)                                        
C-------------------COPIES INPUT UD                                             
         IF (ATCNT .EQ. 2) CALL COPY(25)                                        
         IF(ATCNT .EQ. 3) THEN                                                  
            READ(65,1111)LINE                                                   
            READ(65,1111)LINE                                                   
            IF((HGME.EQ.1).AND.(CHGME.EQ.1))LINE(45:55) = ',HGME,CHGME'         
            WRITE(97,2111) LINE                                                 
         ENDIF                                                                  
         IF (ATCNT .EQ. 6) THEN                                                 
            READ(65,1111)LINE                                                   
            READ(65,1111) LINE                                                  
            WRITE(97,2111) LINE                                                 
            READ(65,1111) LINE                                                  
            IF (LRN1 .EQ. 1)  LINE(8:26) =',CLAT,CLON,CCEP,CGS'                 
            IF (DME .EQ. 1) LINE(27:41) = ',RANG,FLAG,FREQ'                     
            WRITE(97,2111) LINE                                                 
         ENDIF                                                                  
         IF (ATCNT .EQ. 7) THEN                                                 
            READ(65,1111)LINE                                                   
            IF (DME .EQ. 1) LINE(45:59) = ',RANG,FLAG,FREQ'                     
            IF (LRN1 .EQ. 1) THEN                                               
               LINE(60:60) = ','                                                
               WRITE(97,2111)LINE                                               
               LINE(1:18) = 'CLAT,CLON,CCEP,CGS'                                
               LINE(19:80) = '                                                  
     &                          '                                               
               WRITE(97,2111)LINE                                               
            ENDIF                                                               
         ENDIF                                                                  
         IF (ATCNT .EQ. 8) THEN                                                 
            READ(65,1111)LINE                                                   
            WRITE(97,1113)THR                                                   
         ENDIF                                                                  
C------------------SETS UP INICAL OP                                            
         IF (ATCNT .EQ. 9) THEN                                                 
           IF (HGCAL .EQ. 0) THEN                                               
             WRITE(97,3125)                                                     
             WRITE(97,3126)                                                     
 3125        FORMAT(' KUTOFF= 1, %FOR,HGM')                
 3126        FORMAT(' NCFBLO = 2, %FOR,HGM')                       
           ENDIF                                                                
            CALL COPY(35)                                                       
            IF (HGCAL .EQ. 0) WRITE(97,3127)                                    
 3127       FORMAT(' LETVAR =2,[111.76 ,-101.60,-0.4,146.304,-15.24 ],',  
     $      '''23SEP83'' ,%FOR,HGM')                                            
            CALL RDINI                                                          
            CALL DERTB2                                                         
         ENDIF                                                                  
         IF ((ATCNT .EQ. 10) .and. (lk .eq. 1))  CALL COPY(24)          
         IF (ATCNT .EQ. 11) THEN                                                
            WRITE (97,2015) 'TITLE', DUMMY2, 'UNITS', '/NAME'            
            CALL MATCH(CNAME,CNUMV)                                             
         ENDIF                                                                  
         IF((ATCNT .EQ. 12) .AND. (DME .EQ. 1))  CALL COPY(85)                  
         IF ((ATCNT .EQ. 13) .AND. (LRN1 .EQ. 1)) CALL COPY(94)                 
C-------------TERP OPERATION (PRE-CALIB)                                        
         IF(ATCNT .EQ. 16) THEN                                                 
            CALL TSRT(THR,TURB)                                                 
            ATCNT=ATCNT +3                                                      
         ENDIF                                                                  
         IF (ATCNT .EQ. 20) CALL PRATCK                                         
C-------------SETS UP  CALIB OP                                                 
         IF (ATCNT .EQ. 21) THEN                                                
            CALL VECGEN(NAME,NUMVAR)                                            
            CALL CNSGEN(KFLAG)                                                  
            CALL DERTAB                                                         
         ENDIF                                                                  
         IF (ATCNT .EQ. 22) THEN                                                
            CALL CSRT                                                           
         ENDIF                                                                  
         IF (ATCNT .EQ. 23) CALL PMSCHK                                         
         IF(ATCNT .EQ. 24) THEN                                                 
            WRITE (97,2015) 'TITLE', DUMMY2, 'UNITS', '/NAME'           
            CALL MATCH(DFNAME,DFNUM)                                            
         ENDIF                                                                  
C-----------TERPS (POST CALIB)(TERP2) & DESPIKE IF RFLAG OR RFLAG1              
         IF (ATCNT .EQ. 26) THEN                                                
C------------(TERP2 FIRST, 2/6/89 AS PER GH)                                    
           IF (TURB .EQ. 'HRT') CALL TERPIT                                     
C-------------DO NOT ADD  GPDESPIK POSTCAL AS PER CC, 30MAY1989                 
C          IF ((VFLG  .EQ. 1) .OR. (VFLG1 .EQ. 1)) CALL COPY(95)                
         ENDIF                                                                  
C-------------OUTPUT OP                                                         
         IF(ATCNT .EQ. 27) CALL OUTGEN(PRARR,NUMPR,600)                         
C----------------PRINTS/STATS                                                   
         IF(ATCNT .EQ. 29) THEN                                                 
            CALL PBCHK(PRARR,NUMPR,3,600)                                       
            CALL RCLR(PRARR,NUMPR,600)                                          
            CALL PCLR(PRARR,NUMPR,600)                                          
            CALL VECGEN(PRARR,NUMPR)                                            
         ENDIF                                                                  
         IF(ATCNT .EQ. 30) CALL VECGEN(PRARR,NUMPR)                             
C-----------------PLOT OPERATION #1                                             
         IF(ATCNT .EQ. 31) CALL FSET(PRARR,NUMPR,63)                            
         IF(ATCNT .EQ. 34) THEN                                                 
            WRITE (97,2020) 'XAXIS', 'YAXIS', 'BOTRNG', 'MAJMIN'                
            CALL PRFUNC                                                         
            CALL COPY(63)                                                       
         ENDIF                                                                  
C---------------PLOT OPERATION  #2                                              
         IF ((ATCNT .EQ. 35) .AND. (NUMPL .LE. 0)) WRITEN = 0                   
            IF((ATCNT .EQ. 36) .AND. (WRITEN .GE. 1))                           
     $        CALL FSET(PLARR,NUMPL,64)                                         
            IF((ATCNT .EQ. 39) .AND. (WRITEN .GE. 1)) THEN                      
               WRITE (97,2020) 'XAXIS', 'YAXIS', 'BOTRNG', 'MAJMIN'             
               CALL PRFUNC                                                      
               CALL COPY(64)                                                    
            ENDIF                                                               
         IF (ATCNT .EQ. 40) WRITEN = 1                                          
      ENDIF                                                                     
      GOTO 10                                                                   
                                                                                
                                                                                
                                                                                
                                                                                
  400 END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                   SUBROUTINE TSRT(THR,TURB)                                    
C-------------This routine takes the rates determined in RTSORT                 
C             and determines what needs to be terp'ed in the pre-CALIB          
C             terp operation and writes the output out.                         
                                                                                
      include "gparc.h"
      include "gprat.h"
      include "gpder.h"
      include "gprad.h"
                                                                                
                                                                                
      CHARACTER*1 CHOICE                                                  
      CHARACTER*3 TURB                                                          
      CHARACTER*8 LOWARR(300),HIARR(300)                                        
      CHARACTER*9 VNAME(300),DUMMY                                              
      CHARACTER*80 LINE                                                         
      INTEGER NMLO,NMHI,I,J,K,L,NEWFLG,THR
                                                                                
C------ADDS VARIBLES FOR THIS OP ONLY                                           
      TNAME(TNUM+1) = 'THF     '                                                
      TNAME(TNUM+2) = 'XVI     '                                                
      TNAME(TNUM+3) = 'YVI     '                                                
      TRATE(TNUM+1) = THR                                                       
      TRATE(TNUM+2) = 10                                                        
      TRATE(TNUM+3) = 10                                                        
      TNUM = TNUM + 3                                                           
                                                                                
      NMHI = 0                                                                  
      NMLO = 0                                                                  
                                                                                
      DO 100,I=1,NUMVAR                                                         
         NEWFLG = 0                                                             
C-------------Probes stay at same rate.                                         
         IF (NAME(I)(1:4) .EQ. 'ASAS') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'FSSP') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'X260') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'X200') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'Y200') GOTO 100                                 
         IF (NAME(I) .EQ. 'STROBE  ') GOTO 100                                  
                                                                                
C------------------------Determines if the variable is raw                      
         DO 200,J=1,TNUM                                                        
            IF (NAME(I) .EQ. TNAME(J)) THEN                                     
                                                                                
C----If so, then puts it into the appropriate array.                            
               IF(RATE(I) .NE. TRATE(J)) THEN                                   
                  IF (RATE(I) .EQ. 1) THEN                                      
                     NMLO = NMLO +1                                             
                     LOWARR(NMLO) = NAME(I)                                     
                  ELSE                                                          
                     NMHI = NMHI+1                                              
                     HIARR(NMHI) = NAME(I)                                      
                  ENDIF                                                         
               ENDIF                                                            
            ENDIF                                                               
  200    CONTINUE                                                               
  100 CONTINUE                                                                  
                                                                                
                                                                                
C------This whole section does some formatting and prints out                   
C      the low rate terps.                                                      
                                                                                
      DO 300,I=1,10                                                             
         READ(65,1111)LINE                                                      
  300 CONTINUE                                                                  
                                                                                
      if (nmlo .gt. 0) then
      WRITE(97,1171)                                                        
 1171 FORMAT(' LOWPAS=YES,%FOR,')                                            
 1111 FORMAT(A80)                                                               
 1121 FORMAT(1x,A80)                                                    
                                                                                
      DO 30, I = 1, 300                                                         
         VNAME(I) = '        '                                                  
   30 CONTINUE                                                                  
                                                                                
      DO 10, I=1, NMLO                                                          
            VNAME(I)(1:8) = LOWARR(I)                                           
            VNAME(I)(9:9) = ','                                                 
   10 CONTINUE                                                                  
      VNAME(NMLO)(9:9) = ' '                                                    
      K = INT(FLOAT(NMLO)/6.0)                                                  
                                                                                
      DO 20, J =0, K                                                            
         M = (6*J) + 1                                                          
         WRITE(97,2002) (VNAME(L), L=M, M+5)                                    
   20 CONTINUE                                                                  
                                                                                
      READ(65,1111)LINE                                                         
      READ(65,1111)LINE                                                         
      WRITE(97,1172)                                                        
 1172 FORMAT(' RATE=1,%FOR,')                                            
                                                                                
      DO 25, J =0, K                                                            
         M = (6*J) + 1                                                          
         WRITE(97,2002) (VNAME(L), L=M, M+5)                                    
   25 CONTINUE                                                                  
 2002 FORMAT(9X,6A9)                                                            
      endif
                                                                                
C---------This whole section does the same as above, but for high               
C         rate terps.                                                           
      READ(65,1111)LINE                                                         
      READ(65,1111) LINE                                                        
      IF ((TURB .EQ. 'HRT').and. (nmhi .gt. 0)) THEN                         
      WRITE(97,1173)                                                        
 1173 FORMAT(' RATE=20,%FOR,')                                          
                                                                                
                                                                                
                                                                                
      DO 35, I = 1, 300                                                         
         VNAME(I) = '        '                                                  
   35 CONTINUE                                                                  
                                                                                
      DO 11, I=1, NMHI                                                          
            VNAME(I)(1:8) = HIARR(I)                                            
            VNAME(I)(9:9) = ','                                                 
   11 CONTINUE                                                                  
      VNAME(NMHI)(9:9) = ' '                                                    
      K = INT(FLOAT(NMHI)/6.0)                                                  
                                                                                
      DO 45, J =0, K                                                            
         M = (6*J) + 1                                                          
         WRITE(97,2002) (VNAME(L), L=M, M+5)                                    
   45 CONTINUE                                                                  
      ENDIF                                                                     
                                                                                
      READ(65,1111)LINE                                                         
                                                                                
C---------REMOVES VARIABLES ADDED ABOVE                                         
      TNUM = TNUM -3                                                            
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE CSRT                                              
C-------This Subroutine sorts and prints rates for derived variables            
                                                                                
                                                                                
      include "gparc.h"
      include "gprat.h"
      include "gpder.h"
      include "gprad.h"
                                                                                
      CHARACTER*1 CHOICE                                                  
      CHARACTER*8 LOWARR(300),HIARR(300)
      CHARACTER*9 VNAME(300),DUMMY                                              
      CHARACTER*80 LINE                                                         
      INTEGER NMLO,NMHI,I,J,K,L,NEWFLG,NTOT                                
                                                                                
                                                                                
      NMHI = 0                                                                  
      NTOT = 0                                                                  
                                                                                
      DO 300,I=1,7                                                              
         READ(65,1111)LINE                                                      
  300 CONTINUE                                                                  
C----------Sorts the varaibles into arrays by rate, if they need to             
C          to be set in CALIB.                                                  
                                                                                
 3333 NMHI=NMHI+1                                                               
      NMLO = 0                                                                  
      IF (NUMVAR .GT. 600) PRINT *, 'NUMVAR EXCEEDS NAME ARRAY'                 
      DO 100,I=1,NUMVAR                                                         
               IF (RATE(I) .EQ. NMHI) THEN                                      
                  NMLO = NMLO +1                                                
                  NTOT = NTOT + 1                                               
                  IF (NMLO .GT. 300) PRINT *, 'LOWARR BOUND EXCEED'             
                  LOWARR(NMLO) = NAME(I)                                        
               ENDIF                                                            
  100 CONTINUE                                                                  
      IF (NMHI .GT. 999) GOTO 2003                                              
      IF (NMLO .LT. 1) GOTO 3333                                                
                                                                                
                                                                                
                                                                                
 1111 FORMAT(A80)                                                               
 1121 FORMAT(1x,A80)                                                   
 1112 FORMAT(' RATE=',I4,',%FOR,')                                   
                                                                                
C----------The rest prints them out after FORMATing.                            
                                                                                
                                                                                
      WRITE(97,1112) NMHI                                                       
      DO 30, I = 1, 300                                                         
         VNAME(I) = '         '                                                 
   30 CONTINUE                                                                  
                                                                                
      IF (NMLO .GT. 300) PRINT *, 'NMLO GT 300'                                 
      DO 10, I=1, NMLO                                                          
            VNAME(I)(1:8) = LOWARR(I)                                           
            VNAME(I)(9:9) = ','                                                 
   10 CONTINUE                                                                  
      VNAME(NMLO)(9:9) = ' '                                                    
      K = INT(FLOAT(NMLO)/6.0)                                                  
                                                                                
      DO 20, J =0, K                                                            
         M = (6*J) + 1                                                          
         WRITE(97,2002) (VNAME(L), L=M, M+5)                                    
   20 CONTINUE                                                                  
                                                                                
                                                                                
 2002 FORMAT(9X,6A9)                                                            
      IF (NTOT .LT. NUMVAR) GOTO 3333                                           
                                                                                
                                                                                
 2003 RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                   SUBROUTINE COPY(WHICH)                                       
C----------------COPY takes the unit defined by which, and copies into          
C                 the output file.                                              
                                                                                
      CHARACTER*80 TEMP                                                         
      INTEGER WHICH                                                             
                                                                                
 1010 FORMAT(A80)                                                               
 1011 FORMAT(1X,A80)                                                        
                                                                                
   10 READ(WHICH,1010,END=200) TEMP                                             
      WRITE(97,1011) TEMP                                                       
      GOTO 10                                                                   
                                                                                
  200 RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                   SUBROUTINE DFIX                                              
C-----This routine takes the reference sensors, and adjusts                     
C     whichever variable derivations are affected by them.                      
                                                                                
                                                                                
      include "gparc.h"
      include "gpref.h"
      include "gpder.h"
                                                                                
      DIMENSION REF(4)                                                          
      CHARACTER*8 REF                      
      INTEGER J,K,L,M,N,O,FLAG
      CHARACTER*1 CHOICE                                                  
                                                                                
 1010 FORMAT(A8)                                                                
 1020 FORMAT(1X,A8,1X,A8,1X,A8,1X,A8)                                           
 1030 FORMAT(A1)                                                                
                                                                                
                                                                                
                                                                                
                                                                                
C-----INDEX is a system call that returns an integer value that                 
C       represents the first occurence in the string of the given               
C       character.                                                              
                                                                                
      L=INDEX(RFSEN(1),' ')                                                     
      M=INDEX(RFSEN(2),' ')                                                     
      N=INDEX(RFSEN(3),' ')                                                     
      O=INDEX(RFSEN(4),' ')                                                     
                                                                                
      DO 150, J= 1, NUMD                                                        
         DO 160,K= 1, 5                                                         
            IF ((DTITLE(K,J) (1:3)) .EQ. 'TTX') THEN                            
               IF (L .EQ. 4) DTITLE(K,J)(3:3) = RFSEN(1)(3:3)                   
               IF (L.NE. 4) CALL ODDFIX(L,K,J,RFSEN(1))                         
            ENDIF                                                               
            IF ((DTITLE(K,J) (1:3)) .EQ. 'ATX') THEN                            
               IF (L.EQ. 4) DTITLE(K, J)(3:3) = RFSEN(1)(3:3)                   
               IF (L .NE. 4) CALL ODDFIX(L,K,J,RFSEN(1))                        
            ENDIF                                                               
                                                                                
            IF ((DTITLE(K,J) (1:3)) .EQ. 'QCX') THEN                            
               IF (M .EQ. 4) DTITLE(K,J)(3:3) = RFSEN(2)(3:3)                   
               IF (M .NE. 4) CALL ODDFIX(M,K,J,RFSEN(2))                        
            ENDIF                                                               
            IF ((DTITLE(K,J) (1:4)) .EQ. 'TASX') THEN                           
               IF (M .EQ. 4) DTITLE(K, J)(4:4) = RFSEN(2)(3:3)                  
               IF (M .NE. 4) CALL ODDFIX(M,K,J,RFSEN(2))                        
            ENDIF                                                               
                                                                                
            IF ((DTITLE(K,J) (1:3)) .EQ. 'DPX') THEN                            
               IF (N .EQ. 4) DTITLE(K,J)(3:3) = RFSEN(3)(3:3)                   
               IF (N .NE. 4) CALL ODDFIX(N,K,J,RFSEN(3))                        
            ENDIF                                                               
            IF ((DTITLE(K,J) (1:5)) .EQ. 'RHODX') THEN                          
               IF (N .EQ. 4) DTITLE(K, J)(5:5) = RFSEN(3)(3:3)                  
               IF (N .NE. 4) CALL ODDFIX(N,K,J,RFSEN(3))                        
            ENDIF                                                               
                                                                                
            IF ((DTITLE(K,J) (1:3)) .EQ. 'PSX') THEN                            
               IF (O .EQ. 4) DTITLE(K,J)(3:3) = RFSEN(4)(3:3)                   
               IF (O .NE. 4) CALL ODDFIX(O,K,J,RFSEN(4))                        
            ENDIF                                                               
                                                                                
            IF (DTITLE(K,J) (1:3) .EQ. 'AKX') DTITLE(K,J) = RFSEN(5)            
            IF (DTITLE(K,J) (1:3) .EQ. 'SSX') DTITLE(K,J) = RFSEN(6)            
                                                                                
                                                                                
                                                                                
  160    CONTINUE                                                               
                                                                                
  150 CONTINUE                                                                  
                                                                                
      END                                                                       
                                                                                
                                                                                
                   SUBROUTINE ODDFIX(POS,IND,INDA,FILL)                         
C----------Subroutine ODDFIX is intended to fill in "X" variables               
C            when the fill in character is longer than 1                        
C            Parameter POS is the length of the variable, IND, and              
C            INDA are its location in the DTITLE array, and fill                
C            is the reference sensor.                                           
                                                                                
      include "gpder.h"
                                                                                
      CHARACTER*8 FILL                                       
      INTEGER POS,IND,INDA,J,K,L
      CHARACTER*4 HOLD                                                          
                                                                                
      J=POS-1                                                                   
      K=INDEX(DTITLE(IND,INDA),' ')                                             
      IF (K .EQ. 4) DTITLE(IND,INDA)(3:J) = FILL(3:J)                           
      IF (K .GT. 4) THEN                                                        
         HOLD(1:K-4) = DTITLE(IND,INDA)(4:K-1)                                  
         DTITLE(IND,INDA)(3:J) = FILL(3:J)                                      
         DTITLE(IND,INDA)(POS:POS+K-5) = HOLD(1:K-4)                            
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                   SUBROUTINE RDDERV                                            
C----RDDERV reads the GENDER database into the arrays DTITLE, DNMVAR,           
C    NS,DCONS, and CONSU.                                                       
                                                                                
      include "gpder.h"
                                                                                
      INTEGER J,K
                                                                                
 1090 FORMAT (5(A8),1X,I1,I1,A8,1X,A8,1X,A6,1X,F8.2)                            
 1080 FORMAT (60X)                                                              
                                                                                
C-----CLEAR HEADER AND BLANK LINE                                               
      READ (45,1080)                                                            
      READ (45,1080)                                                            
                                                                                
C-----Initialize to blanks                                                      
      DO 100, J=1,1000                                                          
         DNMVAR(J) = '        '                                                 
         NAMC(J) = '        '                                                   
         DO 190, K=1,5                                                          
            DTITLE(K,J) = '        '                                            
  190    CONTINUE                                                               
  100 CONTINUE                                                                  
                                                                                
C-----LOAD THE DATABASE                                                         
      NUMD = 1                                                                  
  110 READ (45,1090,END=200) (DTITLE(K,NUMD),K=1,5),NS(NUMD),                   
     $PRIM(NUMD),DNMVAR(NUMD),NAMC(NUMD),CONSU(NUMD),DCONS(NUMD)                
      NUMD = NUMD + 1                                                           
      GOTO 110                                                                  
                                                                                
  200 NUMD = NUMD -1                                                            
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE RDINI                                             
C----RDDERV reads the INICAL database into the arrays DTITL2, DNMVR2,           
C    NS2,DCONS2, and CONSU2.                                                    
                                                                                
      include "gpder2.h"
                                                                                
      INTEGER J,K                                                     
                                                                                
 1090 FORMAT (5(A8),1X,I1,1X,A8,1X,A8,1X,A6,1X,F8.2)                            
 1080 FORMAT (60X)                                                              
                                                                                
                                                                                
C-----Initialize to blanks                                                      
      DO 100, J=1,1000                                                          
         DNMVR2(J) = '        '                                                 
         NAMC2(J) = '        '                                                  
         DO 190, K=1,5                                                          
            DTITL2(K,J) = '        '                                            
  190    CONTINUE                                                               
  100 CONTINUE                                                                  
                                                                                
C-----LOAD THE DATABASE                                                         
      NUMD2 = 1                                                                 
  110 READ (47,1090,END=200) (DTITL2(K,NUMD2),K=1,5),                           
     $NS2(NUMD2),DNMVR2(NUMD2),NAMC2(NUMD2),CONSU2(NUMD2),DCONS2(NUMD2)         
      NUMD2 = NUMD2 + 1                                                         
      GOTO 110                                                                  
                                                                                
  200 NUMD2 = NUMD2 -1                                                          
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                   SUBROUTINE DERTAB                                            
C-----Generates the derivation tables for the CALIB operation, and              
C     writes them to the file VECVAR CALIB.                                     
                                                                                
                                                                                
      include "gparc.h"
      include "gpder.h"
                                                                                
                                                                                
      DIMENSION COM(5)                                                          
      CHARACTER*1 COM                                                     
      CHARACTER*8 CTEMP,NTEMP                                
      INTEGER J,L,K
                                                                                
 1090 FORMAT (' ORDVAR=NUMSOU,    DERIVE')                              
 1080 FORMAT (' /')                                                         
 1070 FORMAT (' LETVAR=',I3,',[',5(A8,A1),'], %FOR,',A8)                    
                                                                                
      WRITE (97,1080)                                                           
      WRITE (97,1090)                                                           
                                                                                
      DO 100, J = 1, NUMVAR                                                     
         NTEMP = NAME(J)                                                        
         DO 110,K=1, NUMD                                                       
C--------Initialize comma array                                                 
            DO 120, L=1,5                                                       
               COM(L) = ' '                                                     
  120       CONTINUE                                                            
            DO 130 L=1,NS(K)                                                    
               COM(L) = ','                                                     
  130       CONTINUE                                                            
            L=NS(K)                                                             
            COM(L) = ' '                                                        
                                                                                
            CTEMP = DNMVAR(K)                                                   
              IF (CTEMP .EQ. NTEMP)  THEN                                       
               IF ((NAMC(K)(8:8).EQ. ' ') .OR.(KFLAG.EQ.NAMC(K)(8:8)))          
     $            WRITE(97,1070) NS(K),(DTITLE(L,K),COM(L),L =1,5),             
     $            DNMVAR(K)                                                     
              ENDIF                                                             
  110    CONTINUE                                                               
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE DERTB2                                            
C-----Generates the derivation tables for the INICAL operation, and             
C     writes them to the file VECVAR CALIB.                                     
                                                                                
      include "gparc.h"
      include "gpder2.h"
                                                                                
      INTEGER J,K                                                     
      DIMENSION COM(5)                                                          
      CHARACTER*1 COM                                                    
      CHARACTER*8 CTEMP,NTEMP                                                   
      INTEGER L
                                                                                
 1090 FORMAT (' ORDVAR=NUMSOU,    DERIVE')                                
 1080 FORMAT (' /')                                                          
 1070 FORMAT (' LETVAR=',I3,',[',5(A8,A1),'], %FOR,',A8)              
                                                                                
      WRITE (97,1080)                                                           
      WRITE (97,1090)                                                           
                                                                                
      DO 100, J = 1, NUMVAR                                                     
         NTEMP = NAME(J)                                                        
         DO 110,K=1, NUMD2                                                      
C--------Initialize comma array                                                 
            DO 120, L=1,5                                                       
               COM(L) = ' '                                                     
  120       CONTINUE                                                            
            DO 130 L=1,NS2(K)                                                   
               COM(L) = ','                                                     
  130       CONTINUE                                                            
            L=NS2(K)                                                            
            COM(L) = ' '                                                        
                                                                                
            CTEMP = DNMVR2(K)                                                   
              IF (CTEMP .EQ. NTEMP)                                             
     $    WRITE(97,1070) NS2(K),(DTITL2(L,K),COM(L),L =1,5),DNMVR2(K)           
  110    CONTINUE                                                               
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                   SUBROUTINE CNSGEN(INDEX)                                     
C------Generates the constant bias/recfact tables, and writes them              
C      to VECVAR CALIB                                                          
                                                                                
      include "gparc.h"
      include "gpder.h"
                                                                                
      CHARACTER*8 NTEMP,CTEMP                  
      INTEGER J                                     
                                                                                
 1080 FORMAT (' /')                                                          
 1070 FORMAT (' ORDVAR =     CONSN,   CONSU,   CONS1      /VARNAME')        
 1060 FORMAT (' LETVAR = ''',A8,''',''  ',A6,''',',F8.2,', %FOR, ',A8)        
                                                                               
                                                                                
      WRITE (97,1080)                                                           
      WRITE (97,1070)                                                           
      WRITE (97,1080)                                                           
                                                                                
      DO 100, J = 1, NUMVAR                                                     
         NTEMP = NAME(J)                                                        
         DO 110, K=1,NUMD                                                       
C--------Checks to see if a possible bias, etc. exists.                         
           IF (NAMC(K)(1:7) .NE. '       ') THEN                                
            CTEMP = DNMVAR(K)                                                   
            IF (CTEMP .EQ. NTEMP) THEN                                          
C---------Checks to make sure the data is for the right plane.                  
              IF ((NAMC(K)(8:8) .EQ.' ').OR.(NAMC(K)(8:8) .EQ.KFLAG))           
     &           WRITE(97,1060)NAMC(K),CONSU(K),DCONS(K),DNMVAR(K)              
              ENDIF                                                             
           ENDIF                                                                
  110    CONTINUE                                                               
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
                   SUBROUTINE RCLR(ARR,LIM,rlim)                                
                                                                                
C------------Subroutine RCLR clears all blanks and redundancies                 
C            out of an array.                                                   
C            Parameter ARR is the array, and LIM is the number of valid         
C                entries.                                                       
C            LIM is reset to reflect the new array size.                        
C            rlim is the size of the array
                                                                                
      INTEGER J,K,LIM,I,RFLAG,rlim                                              
      CHARACTER*8 ARR(1)                          
                                                                                
      K=0                                                                       
      DO 110, J=1,LIM                                                           
         IF (ARR(J) .EQ. '        ') GOTO 110                                   
         RFLAG = 0                                                              
                                                                                
         DO 120,I=1,J-1                                                         
            IF (ARR(I) .EQ. ARR(J)) RFLAG = 1                                   
  120    CONTINUE                                                               
         IF (RFLAG .EQ. 1) GOTO 110                                             
         K=K+1                                                                  
         ARR(K) = ARR(J)                                                        
  110 CONTINUE                                                                  
                                                                                
      LIM=K                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE PCLR(ARR,LIM,rlim)       
                                                                                
C------------Subroutine PCLR clears all probes                                  
C            out of an array.                                                   
C            Parameter ARR is the array, and LIM is the number of valid         
C                entries.                                                       
C            LIM is reset to reflect the new array size.                        
                                                                                

      include "gppms.h"
      CHARACTER*8 ARR(1)              
      INTEGER J,K,LIM,I,RFLAG,rlim                                      
                                                                                
      I=0                                                                       
      DO 110, J=1,LIM                                                           
         DO 120, K=1,PMNUM                                                      
            IF (ARR(J)(1:7) .EQ. PMNAME(K)(1:7)) GOTO 110                       
  120    CONTINUE                                                               
         I=I+1                                                                  
         ARR(I) = ARR(J)                                                        
  110 CONTINUE                                                                  
                                                                                
      LIM=I                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE DFSET                                             
C-----------Subroutine DFSET sets the difference array, that is                 
C            {NAME}-{CNAME}.                                                    
                                                                                
                                                                                
                                                                                
                                                                                
      include "gparc.h"
      include "gpcalc.h"
      include "gprfl2.h"
      INTEGER I, J, K, sFLAG
                                                                                
                                                                                
      DFNUM = 0                                                                 
      DO 110, I=1, NUMVAR                                                       
         sFLAG = 0                                                              
         DO 120, J= 1, CNUMV                                                    
            IF (NAME(I) .EQ.CNAME(J)) sFLAG = 1                                 
  120    CONTINUE                                                               
         IF (sFLAG .EQ. 1) GOTO 110                                             
         DFNUM = DFNUM + 1                                                      
         DFNAME(DFNUM) = NAME(I)                                                
  110 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE FSET(ARR,LIM,IND)                                 
C--------------Subrooutine FSET sets the function name array,                   
C              this involves changing a few names, and deleting                 
C              a few more.  After it has set the FNAME array, it calls          
C              PLGEN to generate the VECFUN.                                    
                                                                                
                                                                                
      include "gprfl.h"
      include "gprfl2.h"
      include "gpkey.h"
                                                                                
      CHARACTER*8 ARR(1)                   
      INTEGER  I, J, K
      INTEGER LIM,IND                          
                                                                                
                                                                                
      FNUM = 0                                                                  
      DO 5423, I = 1, 600                                                       
           FNAME(I) = '        '                                                
 5423 CONTINUE                                                                  
      R1FLG2 = 0                                                                
      R0FLG2 = 0                                                                
                                                                                
                                                                                
      DO 110, I= 1, LIM                                                         
        DO 111, J=1,NMKEY                                                       
           IF(ARR(I) .EQ. KEY(J)) GOTO 110                                      
  111   CONTINUE                                                                
        IF (ARR(I) .EQ.'ALAT    ') THEN                                         
           FNUM = FNUM +1                                                       
           FNAME(FNUM) = 'PLALONI '                                             
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'RFLAG   ') THEN                                         
           FNUM = FNUM+1                                                        
           FNAME(FNUM) = 'PDLA    '                                             
           R0FLG2 = 1                                                           
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'RFLAG1  ') THEN                                         
           FNUM = FNUM+1                                                        
           FNAME(FNUM) = 'PDLA1   '                                             
           R1FLG2 = 1                                                           
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'DLAT    ') THEN                                         
           FNUM = FNUM+1                                                        
           FNAME(FNUM) = 'PLALOND '                                             
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'ALATC   ') THEN                                         
           FNUM = FNUM+1                                                        
           FNAME(FNUM) = 'PLALONIC'                                             
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'CLAT    ') THEN                                         
           FNUM = FNUM + 1                                                      
           FNAME(FNUM) = 'PLALONC '                                             
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ.'DEI     ') THEN                                         
           FNUM = FNUM + 2                                                      
           FNAME(FNUM -1) = 'DEN1    '                                          
           FNAME(FNUM) = 'DEN2    '                                             
           GOTO 110                                                             
           ENDIF                                                                
        IF (ARR(I) .EQ. 'ALON    ') GOTO 110                                    
        IF (ARR(I) .EQ. 'ALONC   ') GOTO 110                                    
        IF (ARR(I) .EQ. 'CLON    ') GOTO 110                                    
        IF (ARR(I) .EQ. 'DNI     ') GOTO 110                                    
        IF (ARR(I) .EQ. 'DLON    ') GOTO 110                                    
        IF (ARR(I)(1:4) .EQ. 'AASS') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'CASS') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'AFSP') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'CFSP') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'FSSP') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'ASAS') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'X200') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'Y200') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'X260') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'A20Y') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'C20Y') GOTO 110                                   
        IF (ARR(I)(1:7) .EQ. 'ACCUMX6') GOTO 110                                
        IF (ARR(I)(1:6) .EQ. 'CONCX6') GOTO 110                                 
        IF (ARR(I)(1:4) .EQ. 'A20X') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'C20X') GOTO 110                                   
        IF (ARR(I)(1:4) .EQ. 'GUST') GOTO 110                                   
        IF ((ARR(I)(1:6) .EQ. 'RHOLA ') .AND.(R0FLG .EQ. 1))GOTO 110            
        IF ((ARR(I)(1:6) .EQ. 'RHOLA1') .AND.(R1FLG .EQ. 1))GOTO 110            
        FNUM = FNUM +1                                                          
        FNAME(FNUM) = ARR(I)                                                    
  110 CONTINUE                                                                  
      CALL SPECPL(IND)                                                          
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                   SUBROUTINE SPECPL(IND)                                       
                                                                                
                                                                                
      include "gprfl2.h"
                                                                                
      CHARACTER*80 LINE                                                         
      CHARACTER*8 ARR(600)                   
      INTEGER J, K
      INTEGER LIM,IND                                                           
                                                                                
                                                                                
 1211 FORMAT(A80)                                                               
      DO 1217, J= 1 , FNUM                                                      
        ARR(J) = FNAME(J)                                                       
 1217 CONTINUE                                                                  
      LIM = FNUM                                                                
                                                                                
 1213 READ(IND,1211,END=1212) LINE                                              
      J = INDEX(LINE,'%FOR, ')                                                  
      LIM =  LIM + 1                                                            
      ARR(LIM) = LINE(J+6:J+13)                                                 
      GOTO 1213                                                                 
                                                                                
 1212 REWIND(IND)                                                               
      CALL RCLR(ARR,LIM,600)                                                    
      CALL PLGEN(ARR,LIM)                                                       
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE PRFUNC                                            
C------------Subroutine PRFUNC merely prints out the LETFUN table.              
                                                                                
      include "gprfl2.h"
                                                                                
      INTEGER J                                                      
                                                                                
                                                                               
 2070 FORMAT(' LETFUN=       ,',A8,',               ,',                        
     ^'               ,',' %FOR, ', A8)                                         
 2020 FORMAT(' LETFUN=ALON    ,ALAT    ,[(,.75),(,.75)],',                     
     $'[(5,5),(5,5)],%FOR, PLALONI')                                    
 2021 FORMAT(' LETFUN=CLON    ,CLAT    ,[(,.75),(,.75)],',                    
     $'[(5,5),(5,5)],%FOR, PLALONC')                                            
 2022 FORMAT(' LETFUN=DEI     ,DNI     ,[(,300.),(,300.)],',                 
     $'[(10,3),(10,3)],%FOR,',A8)                                            
 2024 FORMAT(' LETFUN=DLON    ,DLAT    ,[( ,3.0),( ,3.0)],',                 
     $'[(10,3),(10,3)],%FOR, PLALOND')                                     
 2025 FORMAT(' LETFUN=ALONC   ,ALATC   ,[(,.75),(,.75)],',                  
     $'[(5,5),(5,5)],%FOR, PLALONIC')                                     
 2026 FORMAT(' LETFUN=    ,[RHOLA,RFLAG] ,               ,',                 
     $'             ,%FOR, PDLA')                                            
 2027 FORMAT(' LETFUN=    ,[RHOLA1,RFLAG1] ,               ,',              
     $'             ,%FOR, PDLA1')                                              
                                                                                
      DO 100, J=1,FNUM                                                          
         IF (FNAME(J) .EQ. 'PLALONI ')  THEN                                    
            WRITE(97,2020)                                                      
         ELSEIF (FNAME(J) .EQ. 'PLALONC ')  THEN                                
            WRITE(97,2021)                                                      
         ELSEIF (FNAME(J) .EQ. 'PLALONIC')  THEN                                
            WRITE(97,2025)                                                      
         ELSEIF (FNAME(J)(1:5) .EQ. 'PDLA ')  THEN                              
            WRITE(97,2026)                                                      
         ELSEIF (FNAME(J)(1:5) .EQ. 'PDLA1')  THEN                              
            WRITE(97,2027)                                                      
         ELSEIF (FNAME(J) .EQ. 'PLALOND ')  THEN                                
            WRITE(97,2024)                                                      
         ELSEIF (FNAME(J)(1:3) .EQ. 'DEN') THEN                                 
            WRITE(97,2022)FNAME(J)                                              
         ELSE                                                                   
            WRITE(97,2070)FNAME(J),FNAME(J)                                     
         ENDIF                                                                  
                                                                                
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                   SUBROUTINE MATCH(ARR,LIM)                                    
                                                                                
                                                                                
C This subroutine takes the variables from the array NAME and tries to          
C match them with the variables in the data base.  For every variable           
C that is matched, the title, unit, and variable name are written to            
C print titles; and the var name is written to plot titles. Variables           
C that can not be matched are sent to NOTH.                                     
                                                                                
      include "gparc.h"
      CHARACTER*7 UNITS2                                                        
      CHARACTER*8 ARR(1)                                          
      INTEGER I, J, K, IFLAG, WHICH, L                   
      LOGICAL FOUND                                                             
                                                                                
 2065 FORMAT(' LETVAR=''',A40,''',''',A6,''',%FOR,',A8)                        
                                                                                
                                                                                
      DO 210, I = 1, LIM                                                        
        IFLAG = 0                                                               
        FOUND = .FALSE.                                                         
        IF (ARR(I)(1:4) .EQ. 'LYAP') GOTO 210                                   
        IF (ARR(I)(1:4) .EQ. 'GUST') GOTO 210                                   
  110   DO 220, J = 1, VAREND                                                   
          IF (VARNAM(J) .EQ. ARR(I)) THEN                                       
C----------ADD A BLANK IN FRONT OF UNITS IF POSSIBLE                            
            IF (UNITTS(J) (6:6) .EQ. ' ') THEN                            
              UNITS2 = ' ' // UNITTS(J) (1:6)                               
              UNITTS(J) = UNITS2(1:6)                                  
            END IF                                                              
            WRITE(97,2065) TITLE(J),UNITTS(J),VARNAM(J)                
            FOUND = .TRUE.                                                      
            IFLAG = 0                                                           
          END IF                                                                
  220   CONTINUE                                                                
        IF (.NOT. FOUND) CALL NOTH(I,ARR,IFLAG)                                 
        IF (IFLAG .EQ. 1) GOTO 110                                              
                                                                                
                                                                                
  210 CONTINUE                                                                  
                                                                                
      PRINT *,'Still working.......'                                            
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                  SUBROUTINE PBCHK(ARR,LIM,R,rlim)                              
C-------------Subroutine PBCHK checks for the presence of PMS                   
C             probes, and picks up their associated variables,                  
C             and adds them to  the ARR array.                                  
C             Also does GUST keywords.                                          
                                                                                
                                                                                
                                                                                
      CHARACTER*8 ARR(1)
      INTEGER LIM, R ,rlim                                                      
                                                                                
      DO 12, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'FSSPA   ') CALL PBSET(3,I,1,ARR,LIM,R,rlim)            
   12 CONTINUE                                                                  
      DO 13, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'X200A   ') CALL PBSET(4,I,1,ARR,LIM,R,rlim)            
   13 CONTINUE                                                                  
      DO 14, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'X260A   ') CALL PBSET(1,I,1,ARR,LIM,R,rlim)            
   14 CONTINUE                                                                  
      DO 15, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'Y200A   ') CALL PBSET(5,I,1,ARR,LIM,R,rlim)            
   15 CONTINUE                                                                  
      DO 16, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'ASASA   ') CALL PBSET(2,I,1,ARR,LIM,R,rlim)            
   16 CONTINUE                                                                  
      DO 17, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'DME     ') CALL PBSET(6,I,1,ARR,LIM,R,rlim)            
   17 CONTINUE                                                                  
      DO 18, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'FSSPC   ') CALL PBSET(3,I,0,ARR,LIM,R,rlim)            
   18 CONTINUE                                                                  
      DO 19, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'X200C   ') CALL PBSET(4,I,0,ARR,LIM,R,rlim)            
   19 CONTINUE                                                                  
      DO 20, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'X260C   ') CALL PBSET(1,I,0,ARR,LIM,R,rlim)            
   20 CONTINUE                                                                  
      DO 21, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'Y200C   ') CALL PBSET(5,I,0,ARR,LIM,R,rlim)            
   21 CONTINUE                                                                  
      DO 22, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'ASASC   ') CALL PBSET(2,I,0,ARR,LIM,R,rlim)            
   22 CONTINUE                                                                  
      DO 23, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'GUSTR   ') CALL PBSET(7,I,0,ARR,LIM,R,rlim)            
   23 CONTINUE                                                                  
      DO 24, I= 1, LIM                                                          
        IF (ARR(I) .EQ. 'GUSTO   ') CALL PBSET(8,I,0,ARR,LIM,R,rlim)            
   24 CONTINUE                                                                  
        RETURN                                                                  
        END                                                                     
                                                                                
                   SUBROUTINE NOTH(INDEX,ARR,IFLAG)                             
C---------------Subroutine NOTH deals with unmatched variables, it              
C                 gives the user the options of deleting the variable,          
C                 changing its name, or adding it to the database.              
C                 Parameter INDEX is the variable's location in the             
C                 parameter ARR array, which is the file to be written          
C                 to, and IFLAG  is set if the name is changed,                 
C                 indicating that the new name should be checked.               
                                                                                
                                                                                
      include "gparc.h"
                                                                                
      CHARACTER*40 USRTLE                                                 
      CHARACTER*8 ARR(1),NEWNAM                                   
      CHARACTER*6 USRUNT,USRUN2                                           
      CHARACTER*1 CHOICE                                                  
      INTEGER INDEX,WHICH,IFLAG                             
                                                                             
 2065 FORMAT(' LETVAR=''',A40,''',''',A6,''',%FOR,',A8)                       
 2070 FORMAT(' LETFUN=       ,',A8,',               ,',                      
     ^'               ,',' %FOR, ', A8)                                         
 2077 FORMAT (A)                                                                
 2080 FORMAT (A8)                                                               
 2085 FORMAT (A40)                                                              
 2090 FORMAT (A6)                                                               
 2095 FORMAT (1X,A40,1X,A6,3X,A8)                                               
 3000 FORMAT (1x,A40,1X,A6,3X,A8)                                          
                                                                                
                                                                                
      IFLAG = 0                                                                 
      PRINT *                                                                   
  250 WRITE(6,2075) ARR(INDEX)                                                  
 2075 FORMAT(1X,A8 ,'is NOT in the data base.')                                 
      PRINT *, 'Would you like to :'                                            
      PRINT *, '1) re-enter variable name (to correct spelling)'                
      PRINT *, '2) add this variable to the database '                          
      PRINT *, '3) defer defining a title for this variable'                    
      PRINT *, 'Enter 1, 2, or 3'                                               
 1112 continue                                                                  
      READ (5,2077,END=1112) CHOICE                                             
      if (choice .eq. "") goto 1112
      IF (CHOICE .EQ. '1') THEN                                                 
         PRINT *, 'Please enter correct variable name'                          
 1113    continue                                                               
         READ (5, 2080,END=1113) NEWNAM                                         
         if (choice .eq. "") goto 1113
         ARR(INDEX) = NEWNAM                                                    
         IFLAG = 1                                                              
         GOTO 230                                                               
      ELSE IF (CHOICE .EQ. '3') THEN                                            
         ARR(INDEX) = '        '                                                
         GOTO 230                                                               
      ELSE IF (CHOICE .EQ. '2') THEN                                            
  260    CALL system("clear")                                                
         PRINT *, 'Enter title(40 characters or less) for ',ARR(INDEX)          
 1114    continue                                                               
         READ (5,2085,END=1114) USRTLE                                          
         if (choice .eq. "") goto 1114
         PRINT *, 'Enter units (6 characters or less) for ',ARR(INDEX)          
         PRINT *, 'Return<cr> will leave units blank.'                          
         READ (5,2090,END=1115) USRUNT                                          
         if (choice .eq. "") goto 1115
         GOTO 270                                                               
 1115    continue                                                               
         USRUNT = '      '                                                      
  270    CALL system("clear")                                            
         PRINT *, '     TITLE                              ,UNITS ,NA           
     $ME '                                                                      
         PRINT *                                                                
         WRITE (6,2095) USRTLE, USRUNT, ARR(INDEX)                              
         PRINT *, 'Do you want to :'                                            
         PRINT *, '1) Make a change'                                            
         PRINT *, '2) Add this to the database'                                 
         PRINT *, 'Enter 1 or 2'                                                
 1116    continue                                                               
         READ (5, 2077,END=1116) CHOICE                                         
         if (choice .eq. "") goto 1116
         IF (CHOICE .EQ. '1') THEN                                              
            PRINT *, 'Do you want to :'                                         
            PRINT *, '1) Change variable name'                                  
            PRINT *, '2) Change other '                                         
            PRINT *, 'Enter 1 or 2'                                             
 1117       continue                                                            
            READ (5,2077,END=1117) CHOICE                                       
            if (choice .eq. "") goto 1117
            IF (CHOICE .EQ. '1') THEN                                           
               PRINT *, 'Enter correct  variable name'                          
 1118          continue                                                         
               READ (5,2080,END=1118) NEWNAM                                    
               if (choice .eq. "") goto 1118
               ARR(INDEX) = NEWNAM                                              
            END IF                                                              
               GOTO 260                                                         
         ELSE IF (CHOICE .EQ. '2') THEN                                         
C add a blank space in front of the unit field in the output file if            
C there is enough space                                                         
            IF (USRUNT (6:6) .EQ. ' ') THEN                                     
               USRUN2 = ' ' // USRUNT (1:6)                                     
            END IF                                                              
            WRITE (36,3000) USRTLE, USRUNT, ARR(INDEX)                          
            WRITE(97,2065) USRTLE, USRUN2, ARR(INDEX)                           
         ELSE                                                                   
            GOTO 270                                                            
         END IF                                                                 
      ELSE                                                                      
         GOTO 250                                                               
      END IF                                                                    
  230 RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                    SUBROUTINE PBSET(WHICH,INDEX,AORC,ARR,LIM,R,rlim)  
C-----This subroutine generates variable names for ASAS probes, and             
C      adds them to the name array.  It is called by PBCHK, and calls           
C      PUSH to make room for its new variables.                         
                                                                                
      integer rlim
      CHARACTER*8 ARR(1)
      INTEGER NUMVAR, VAREND,I, J, WHICH,INDEX,RATE,AORC,R                      
                                                                                
      GOTO (30,40,40,40,40,50,140,150) WHICH                                    
                                                                                
  140 CALL PUSH(7,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX+1) = 'UIR     '                                                 
      ARR(INDEX+2) = 'VIR     '                                                 
      ARR(INDEX+3) = 'WIR     '                                                 
      ARR(INDEX+4) = 'UXR     '                                                 
      ARR(INDEX+5) = 'VYR     '                                                 
      ARR(INDEX+6) = 'WDR     '                                                 
      ARR(INDEX+6) = 'WSR     '                                                 
      GOTO 100                                                                  
                                                                                
  150 CALL PUSH(7,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX+1) = 'UI      '                                                 
      ARR(INDEX+2) = 'VI      '                                                 
      ARR(INDEX+3) = 'WI      '                                                 
      ARR(INDEX+4) = 'UX      '                                                 
      ARR(INDEX+5) = 'VY      '                                                 
      ARR(INDEX+6) = 'WD      '                                                 
      ARR(INDEX+7) = 'WS      '                                                 
      GOTO 100                                                                  
                                                                                
   50 CALL PUSH(2,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX) = 'RANG    '                                                   
      ARR(INDEX+1) = 'FLAG    '                                                 
      ARR(INDEX+2) = 'FREQ    '                                                 
      GOTO 100                                                                  
                                                                                
   40 IF (R .LT. 3) THEN                                                        
         ARR(LIM+1)(5:8) = '01  '                                               
         ARR(LIM+2)(5:8) = '02  '                                               
         ARR(LIM+3)(5:8) = '03  '                                               
         ARR(LIM+4)(5:8) = '04  '                                               
         ARR(LIM+5)(5:8) = '05  '                                               
         ARR(LIM+6)(5:8) = '06  '                                               
         ARR(LIM+7)(5:8) = '07  '                                               
         ARR(LIM+8)(5:8) = '08  '                                               
         ARR(LIM+9)(5:8) = '09  '                                               
         ARR(LIM+10)(5:8) = '10  '                                              
         ARR(LIM+11)(5:8) = '11  '                                              
         ARR(LIM+12)(5:8) = '12  '                                              
         ARR(LIM+13)(5:8) = '13  '                                              
         ARR(LIM+14)(5:8) = '14  '                                              
         ARR(LIM+15)(5:8) = '15  '                                              
         LIM=LIM+15                                                             
      ENDIF                                                                     
      GOTO (100,60,70,80,90) WHICH                                              
                                                                                
                                                                                
   30 CALL PUSH(6,INDEX,ARR,R,LIM,rlim)                                         
      IF (R .LT.3) THEN                                                         
         IF (AORC .EQ. 1) ARR(LIM+1) = 'ACCUMX6 '                               
         IF (AORC .EQ. 0) ARR(LIM+1) = 'CONCX6 '                                
         LIM = LIM + 1                                                          
      ENDIF                                                                     
      ARR(INDEX) =   'X260    '                                                 
      ARR(INDEX+1) = 'SUMX6   '                                                 
      ARR(INDEX+2) = 'CONC6   '                                                 
      ARR(INDEX+3) = 'DBAR6   '                                                 
      ARR(INDEX+4) = 'DISP6   '                                                 
      ARR(INDEX+5) = 'PLWC6   '                                                 
      ARR(INDEX+6) = 'DBZ6    '                                                 
      GOTO 100                                                                  
                                                                                
   70 IF (R .LT. 3) THEN                                                        
      DO 110, I=1,15                                                            
         IF (AORC .EQ. 1) ARR (LIM-15+I)(1:4) = 'AFSP'                          
         IF (AORC .EQ. 0) ARR(LIM-15+I)(1:4) ='CFSP'                            
  110 CONTINUE                                                                  
      ENDIF                                                                     
      CALL PUSH(10,INDEX,ARR,R,LIM,rlim)                                        
      ARR(INDEX)   = 'FSSP    '                                                 
      ARR(INDEX+1) = 'SUM15F  '                                                 
      ARR(INDEX+2) = 'CONCF   '                                                 
      ARR(INDEX+3) = 'DBARF   '                                                 
      ARR(INDEX+4) = 'DISPF   '                                                 
      ARR(INDEX+5) = 'FACT    '                                                 
      ARR(INDEX+6) = 'PLWCF   '                                                 
      ARR(INDEX+7) = 'FBMFR   '                                                 
      ARR(INDEX+8) = 'FRANGE  '                                                 
      ARR(INDEX+9)= 'FRESET  '                                                  
      ARR(INDEX+10) = 'FSTROB  '                                                
      GOTO 100                                                                  
                                                                                
   80 IF (R .LT. 3) THEN                                                        
      DO 120, I=1,15                                                            
         IF (AORC .EQ. 1) ARR(LIM-15+I)(1:4) = 'A20X'                           
         IF (AORC .EQ. 0) ARR(LIM-15+I)(1:4) ='C20X'                            
  120 CONTINUE                                                                  
      ENDIF                                                                     
      CALL PUSH(6,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX)   = 'X200    '                                                 
      ARR(INDEX+1) = 'PLWCX   '                                                 
      ARR(INDEX+2) = 'SUM15X  '                                                 
      ARR(INDEX+3) = 'CONCX   '                                                 
      ARR(INDEX+4) = 'DBARX   '                                                 
      ARR(INDEX+5) = 'DISPX   '                                                 
      ARR(INDEX+6) = 'DBZX    '                                                 
      GOTO 100                                                                  
                                                                                
   90 IF (R .LT. 3) THEN                                                        
      DO 130, I=1,15                                                            
         IF (AORC .EQ. 1) ARR(LIM-15+I)(1:4) = 'A20Y'                           
         IF (AORC .EQ. 0) ARR(LIM-15+I)(1:4) ='C20Y'                            
  130 CONTINUE                                                                  
      ENDIF                                                                     
      CALL PUSH(6,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX)   = 'Y200    '                                                 
      ARR(INDEX+1) = 'SUM15Y  '                                                 
      ARR(INDEX+2) = 'CONCY   '                                                 
      ARR(INDEX+3) = 'DBARY   '                                                 
      ARR(INDEX+4) = 'DISPY   '                                                 
      ARR(INDEX+5) = 'PLWCY   '                                                 
      ARR(INDEX+6) = 'DBZY    '                                                 
      GOTO 100                                                                  
                                                                                
   60 IF (R .LT. 3) THEN                                                        
      DO 10, I=1,15                                                             
         IF (AORC .EQ. 1) ARR(LIM-15+I)(1:4) = 'AASS'                           
         IF (AORC .EQ. 0) ARR(LIM-15+I)(1:4) = 'CASS'                           
   10 CONTINUE                                                                  
      ENDIF                                                                     
      CALL PUSH(6,INDEX,ARR,R,LIM,rlim)                                         
      ARR(INDEX)   = 'ASAS    '                                                 
      ARR(INDEX+1) = 'SUM15A  '                                                 
      ARR(INDEX+2) = 'CONCA   '                                                 
      ARR(INDEX+3) = 'DBARA   '                                                 
      ARR(INDEX+4) = 'DISPA   '                                                 
      ARR(INDEX+5) = 'AACT    '                                                 
      ARR(INDEX+6) = 'CAACT   '                                                 
                                                                                
  100 RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                    SUBROUTINE GLCHK                                            
C-----This subroutine checks to see if any keywords are implied and             
C      adds them to the NAME array if they are.                                 
                                                                                
                                                                                
      include "gparc.h"
      include "gppms.h"
      include "gprfl.h"
                                                                                
      INTEGER I, J, WHICH,INDEX,RATE,RAD,NRAD,                   
     $ VLA,VLA1,UIR,AD,UI,BD,rhol,rhol1                       
                                                                                
      PMNUM = 0                                                                 
      RDME = 0                                                                  
      PLT2=0                                                                    
      R1FLG=0                                                                   
      R0FLG = 0                                                                 
      rhol = 0
      rhol1 = 0
      UIR = 0                                                                   
      UI=0                                                                      
      AD=0                                                                      
      BD = 0                                                                    
      vla = 0
      vla1 = 0
      DO 12, J=1,NUMVAR                                                         
         IF (NAME(J)(1:4) .EQ. 'ASAS') THEN                                     
            PMNUM = PMNUM + 1                                                   
            PMNAME(PMNUM) = 'ASAS   ,'                                          
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'FSSP') THEN                                     
            PMNUM = PMNUM + 1                                                   
            PMNAME(PMNUM) = 'FSSP   ,'                                          
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'X260') THEN                                     
            PMNUM = PMNUM + 1                                                   
            PMNAME(PMNUM) = 'X260   ,'                                          
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'X200') THEN                                     
            PMNUM = PMNUM + 1                                                   
            PMNAME(PMNUM) = 'X200   ,'                                          
         ENDIF                                                                  
         IF (NAME(J)(1:4) .EQ. 'Y200') THEN                                     
            PMNUM = PMNUM + 1                                                   
            PMNAME(PMNUM) = 'Y200   ,'                                          
         ENDIF                                                                  
         IF (NAME(J) .EQ. 'UIR     ') UIR=1                                     
         IF (NAME(J) .EQ. 'ADIFR   ') AD=1                                      
         IF (NAME(J) .EQ. 'BDIFR   ') AD=1                                      
         IF (NAME(J) .EQ. 'QCR     ') AD=1                                      
         IF (NAME(J) .EQ. 'UI      ') UI=1                                      
         IF (NAME(J) .EQ. 'ADIF    ') BD=1                                      
         IF (NAME(J) .EQ. 'BDIF    ') BD=1                                      
         IF (NAME(J) .EQ. 'ALATC   ') DMEC2=1                                   
         IF (NAME(J) .EQ. 'XCORR   ') DMEC2=1                                   
         IF (NAME(J)(1:5) .EQ. 'RFLAG ') R0FLG =1                               
         IF (NAME(J)(1:6) .EQ. 'RFLAG1') R1FLG = 1                              
         IF (NAME(J)(1:5) .EQ. 'RHOLA ') rhol =1                               
         IF (NAME(J)(1:5) .EQ. 'RHOLA1') rhol1 =1                               
         IF (NAME(J)(1:6) .EQ. 'VLA   ') vla = 1                              
         IF (NAME(J)(1:6) .EQ. 'VLA1  ') vla1 = 1                              
   12 CONTINUE                                                                  
                                                                                
                                                                                
         IF ((UIR .EQ. 1) .AND. (AD .EQ. 1)) THEN                               
            NUMVAR = NUMVAR+1                                                   
            NAME(NUMVAR) = 'GUSTR   '                                           
         ENDIF                                                                  
         IF ((UI .EQ. 1) .AND. (BD .EQ. 1)) THEN                                
            NUMVAR = NUMVAR+1                                                   
            NAME(NUMVAR) = 'GUSTO   '                                           
         ENDIF                                                                  
         IF (DMEC2  .EQ. 1) THEN                                                
            NUMVAR = NUMVAR+1                                                   
            NAME(NUMVAR) = 'DMEC    '                                           
         ENDIF                                                                  
         IF ((vla1 .EQ. 1) .and. (rhol1 .eq. 1)) THEN        
            NUMVAR = NUMVAR+1                                                   
            NAME(NUMVAR) = 'DLA1    '                                           
         ENDIF                              
         IF ((vla .EQ. 1) .and. (rhol .eq. 1)) THEN                     
            NUMVAR = NUMVAR+1                                                   
            NAME(NUMVAR) = 'DLA     '                                           
         ENDIF                                                                  
                                                                                
         CALL RCLR(PMNAME,PMNUM,6)                                            
                                                                                
      RETURN                                                                    
      END                                                                       
                   SUBROUTINE PUSH(NUM,LOC,ARR,R,LIM,rlim)                      
                                                                                
C---------PUSH simply creates NUM spaces starting at index LOC in the           
C         name array.  The new rates are all set to the RATE of LOC.            
                                                                                
                                                                                
      include "gprat.h"
                                                                                
      integer rlim
      CHARACTER*8 ARR(1)                                                     
      INTEGER J, K ,L,NUM,LOC,R                            
                                                                                
                                                                                
      J=LIM                                                                     
   10 ARR(J+NUM) = ARR(J)                                                       
      IF (R .EQ. 1) RATE(J+NUM) = RATE(J)                                       
      J=J-1                                                                     
      IF(J .le. LOC) GOTO 20                                                    
      GOTO 10                                                                   
   20 LIM = LIM+NUM                                                             
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
                   SUBROUTINE RTSORT(VFLG,VFLG1)                                
                                                                                
                                                                                
C----------------RTSORT fills the rate array parallel to the name               
C             array, and containing the rate each variable should be            
C             when it comes out of the CALIB operation.                         
                                                                                
      include "gparc.h"
      include "gprat.h"
      include "gpder.h"
                                                                                
                                                                                
      CHARACTER*6 DUMMY1                                      
      CHARACTER*8 TNAME(300)
      INTEGER I, J ,TRATE(300),TNUM,CHFLG                  
      INTEGER FFLG,K,L,M,N,RTCONC,VRAT,VRAT1,VFLG,VFLG1            
      CHARACTER*1 DUMMY2                                               
                                                                                
C------This section does needed hardwires to HIGH rate.                         
                                                                                
                                                                                
      DO 100, I=1,NUMVAR                                                        
C--------This block is high since it comes out of GUST.                         
         IF (NAME(I)(1:2) .EQ. 'UI') RATE(I) = 20                               
         IF (NAME(I)(1:2) .EQ. 'VI') RATE(I) = 20                               
         IF (NAME(I)(1:2) .EQ. 'WI') RATE(I) = 20                               
         IF (NAME(I)(1:2) .EQ. 'UX') RATE(I) = 20                               
         IF (NAME(I)(1:2) .EQ. 'VY') RATE(I) = 20                               
         IF (NAME(I)(1:3) .EQ. 'VEW') RATE(I) = 20                              
         IF (NAME(I)(1:3) .EQ. 'VNS') RATE(I) = 20                              
         IF (NAME(I)(1:3) .EQ. 'HI3') RATE(I) = 20                              
         IF (NAME(I)(1:3) .EQ. 'WP3') RATE(I) = 20                              
         IF (NAME(I)(1:4) .EQ. 'GUST') RATE(I) = 20                             
         IF (NAME(I)(1:5) .EQ. 'ACINS') RATE(I) = 20                            
         IF (NAME(I)(1:2) .EQ. 'WD') RATE(I) = 20                               
         IF (NAME(I)(1:2) .EQ. 'WS') RATE(I) = 20                               
                                                                                
C-----------This block is high, because it goes into GUST.                      
         IF (NAME(I) .EQ. 'ALAT    ') RATE(I) = 20                              
         IF (NAME(I)(1:5) .EQ. 'ALPHA') RATE(I) = 20                            
         IF (NAME(I)(1:5) .EQ. 'PITCH') RATE(I) = 20                            
         IF (NAME(I)(1:4) .EQ. 'ROLL') RATE(I) = 20                             
         IF (NAME(I)(1:3) .EQ. 'VZI') RATE(I) = 20                              
         IF (NAME(I)(1:4) .EQ. 'PALT') RATE(I) = 20                             
                                                                                
C----------These are just high.                                                 
         IF (NAME(I)(1:3) .EQ. 'XVI') RATE(I) = 20                              
         IF (NAME(I)(1:3) .EQ. 'YVI') RATE(I) = 20                              
         IF (NAME(I)(1:3) .EQ. 'THF') RATE(I) = 20                              
C-------PMS PROBES don't have rate changes.                                     
         IF (NAME(I)(1:4) .EQ. 'ASAS') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'X200') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'Y200') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'FSSP') GOTO 100                                 
         IF (NAME(I)(1:4) .EQ. 'X260') GOTO 100                                 
         IF (NAME(I) .EQ. 'SUMX6   ')  goto 100           
         IF (NAME(I) .EQ. 'CONC6   ')  goto 100           
         IF (NAME(I) .EQ. 'DBAR6   ')  goto 100           
         IF (NAME(I) .EQ. 'DISP6   ')  goto 100           
         IF (NAME(I) .EQ. 'PLWC6   ')  goto 100           
         IF (NAME(I) .EQ. 'DBZ6    ')  goto 100           
         IF (NAME(I) .EQ. 'ACCUMX6 ')  goto 100     
         IF (NAME(I) .EQ. 'CONCX6  ')  goto 100         
         IF (NAME(I) .EQ. 'SUM15F  ') goto 100           
         IF (NAME(I) .EQ. 'CONCF   ') goto 100           
         IF (NAME(I) .EQ. 'DBARF   ') goto 100           
         IF (NAME(I) .EQ. 'DISPF   ') goto 100           
         IF (NAME(I) .EQ. 'FACT    ') goto 100           
         IF (NAME(I) .EQ. 'PLWCF   ') goto 100 
         IF (NAME(I) .EQ. 'FBMFR   ') goto 100           
         IF (NAME(I) .EQ. 'FRANGE  ') goto 100           
         IF (NAME(I) .EQ. 'FRESET  ') goto 100           
         IF (NAME(I) .EQ. 'FSTROB  ') goto 100           
         IF (NAME(I)(1:4) .EQ. 'AFSP') goto 100          
         IF (NAME(I)(1:4) .EQ. 'CFSP') goto 100          
         IF (NAME(I) .EQ. 'PLWCX   ') goto 100           
         IF (NAME(I) .EQ. 'SUM15X  ') goto 100           
         IF (NAME(I) .EQ. 'CONCX   ') goto 100           
         IF (NAME(I) .EQ. 'DBARX   ') goto 100           
         IF (NAME(I) .EQ. 'DISPX   ') goto 100           
         IF (NAME(I) .EQ. 'DBZX    ') goto 100           
         IF (NAME(I)(1:4) .EQ. 'A20X') goto 100          
         IF (NAME(I)(1:4) .EQ. 'C20X') goto 100          
         IF (NAME(I) .EQ. 'SUM15Y  ') goto 100           
         IF (NAME(I) .EQ. 'CONCY   ') goto 100           
         IF (NAME(I) .EQ. 'DBARY   ') goto 100           
         IF (NAME(I) .EQ. 'DISPY   ') goto 100           
         IF (NAME(I) .EQ. 'PLWCY   ') goto 100           
         IF (NAME(I) .EQ. 'DBZY    ') goto 100           
         IF (NAME(I)(1:4) .EQ. 'C20Y') goto 100  
         IF (NAME(I)(1:4) .EQ. 'A20Y') goto 100          
         IF (NAME(I) .EQ. 'SUM15A  ') goto 100           
         IF (NAME(I) .EQ. 'CONCA   ') goto 100           
         IF (NAME(I) .EQ. 'DBARA   ') goto 100           
         IF (NAME(I) .EQ. 'DISPA   ') goto 100           
         IF (NAME(I) .EQ. 'AACT    ') goto 100           
         IF (NAME(I) .EQ. 'CAACT   ') goto 100           
         IF (NAME(I)(1:4) .EQ. 'AASS') goto 100          
         IF (NAME(I)(1:4) .EQ. 'CASS') goto 100          
         IF (NAME(I) .EQ. 'STROBE  ') GOTO 100                                  
         IF (NAME(I)(1:5) .EQ. 'CONCF') RTCONC = RATE(I)                        
C-----------Everthing at higher rate is dropped to 20.                          
         IF ((RATE(I) .GT. 20).AND.(RATE(I) .NE. 999)) RATE(I) = 20             
C-------------THIS MARKS RATES OF VLA, AND VLA1                                 
         IF (NAME(I) .EQ. 'VLA     ') VRAT = RATE(I)                            
         IF (NAME(I) .EQ. 'VLA1    ') VRAT1 = RATE(I)                           
  100 CONTINUE                                                                  
                                                                                
                                                                                
C----This loop goes through and if the variable is high rate, anything          
C    it is derived from goes high, if it is not high rate but is                
C    derived from something that is, the variable itself goes high.             
C    This continues until a complete pass has been made without                 
C    anything changing.                                                         
                                                                                
  200 CONTINUE                                                                  
      CHFLG = 0                                                                 
      DO 300, I = 1, NUMVAR                                                     
C----------------------------------This covers high rate variables              
       IF (NAME(I) .NE. 'DMEC    ') THEN                                        
C------------There are no terps on account of DMEC, Jan 80                      
         IF (RATE(I) .EQ. 20) THEN                                              
            DO 400,J=1,NUMD                                                     
               IF (NAME(I) .EQ. DNMVAR(J)) THEN                                 
                  DO 500, K=1,NS(J)                                             
                     DO 600, L= 1, NUMVAR                                       
                        IF (NAME(L) .EQ. DTITLE(K,J)) THEN                      
                           IF (RATE(L) .NE. 20)THEN                             
                              RATE(L) = 20                                      
                              CHFLG = 1                                         
                           ENDIF                                                
                        ENDIF                                                   
  600                CONTINUE                                                   
  500             CONTINUE                                                      
               ENDIF                                                            
  400       CONTINUE                                                            
         ENDIF                                                                  
                                                                                
C-----------------------------------This covers unknown variables               
         IF (RATE(I) .EQ. 999) THEN                                             
            DO 700, J= 1, NUMD                                                  
               IF (NAME(I) .EQ. DNMVAR(J)) THEN                                 
                  DO 800, K=1,NS(J)                                             
                     DO 900, L =1,NUMVAR                                        
                        IF (NAME(L) .EQ. DTITLE(K,J)) THEN                      
                           IF (RATE(L) .EQ. 20) THEN                            
                              RATE(I) = 20                                      
                              CHFLG = 1                                         
                           ENDIF                                                
                        ENDIF                                                   
  900                CONTINUE                                                   
  800             CONTINUE                                                      
               ENDIF                                                            
  700       CONTINUE                                                            
         ENDIF                                                                  
                                                                                
       ENDIF                                                                    
  300 CONTINUE                                                                  
      IF (CHFLG .EQ. 1) GOTO 200                                                
                                                                                
                                                                                
C-----If the variable isn't set high, it goes low.                              
                                                                                
      DO 1000,I=1,NUMVAR                                                        
C------------PROBE RATES DO NOT CHANGE                                          
         IF (NAME(I)(1:4) .EQ. 'ASAS') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'X200') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'Y200') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'FSSP') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'X260') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'AFSP') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'CFSP') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'A20X') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'C20X') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'A20Y') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'C20Y') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'AASS') GOTO 1000                                
         IF (NAME(I)(1:4) .EQ. 'CASS') GOTO 1000                                
         IF (NAME(I)(1:7) .EQ. 'ACCUMX6') GOTO 1000                             
         IF (NAME(I)(1:6) .EQ. 'CONCX6') GOTO 1000                              
         IF (NAME(I) .EQ. 'SUMX6   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CONC6   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBAR6   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DISP6   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'PLWC6   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBZ6    ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'SUM15F  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CONCF   ') THEN                                      
               RATE(I) = RTCONC                                                 
               GOTO 1000                                                        
         ENDIF                                                                  
         IF (NAME(I) .EQ. 'DBARF   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DISPF   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'FACT    ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'PLWCF   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'FBMFR   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'FRANGE  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'FRESET  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'FSTROB  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'PLWCX   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'SUM15X  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CONCX   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBARX   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DISPX   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBZX    ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'SUM15Y  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CONCY   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBARY   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DISPY   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'PLWCY   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBZY    ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'SUM15A  ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CONCA   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DBARA   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'DISPA   ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'AACT    ') GOTO 1000                                 
         IF (NAME(I) .EQ. 'CAACT   ') GOTO 1000                                 
                                                                                
         IF (RATE(I) .NE. 20) RATE(I) =1                                        
C-------------TPTIME is hardwired low, because high rate time is silly          
C              but TPTIME is used in high rate calculations.                    
         IF(NAME(I) .EQ. 'TPTIME  ')  RATE(I) = 1                               
C-----------DMEC is hardwired low because the block always comes in low         
         IF (NAME(I)  .EQ. 'DMEC    ') RATE(I) = 1                              
C------This is hardwired low just because.                                      
         IF (NAME(I) .EQ. 'PHDG    ') RATE(I) = 1                               
C--------------Differences at rate 1 always.                                    
         IF (NAME(I)(1:2) .EQ. 'DF') RATE(I) = 1                                
         IF (NAME(I) .EQ. 'POSDF   ') RATE(I) = 1                               
C---------------RHOLA(1) , RFLAG(1) GO TO RATE OF VLA(1)                        
         IF (NAME(I) .EQ. 'RHOLA   ') VFLG = 1                                  
         IF (NAME(I) .EQ. 'RHOLA1  ') VFLG1 = 1                                 
         IF (NAME(I) .EQ. 'RHOLA   ') RATE(I) = VRAT                            
         IF (NAME(I) .EQ. 'RFLAG   ') RATE(I) = VRAT                            
         IF (NAME(I) .EQ. 'RHOLA1  ') RATE(I) = VRAT1                           
         IF (NAME(I) .EQ. 'RFLAG1  ') RATE(I) = VRAT1                           
 1000 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                                                                                
      SUBROUTINE RADSUD(LRN1,DME,HGME,CHGME,THR,PMK,LK)                         
                                                                                
                                                                                
C This subroutine reads the variable names and rates into the TNAME and         
C TRATE arrays.  It initializes the RATE array.                                 
C It sets flags if LRN1,DME,HGME, or CHGME is                                   
C present, and passes back the rate  needed to set THF.                         
                                                                                
      include "gprat.h"
      include "gprad.h"
                                                                                
                                                                                
      CHARACTER*6 DUMMY1                                                        
      CHARACTER*80 INBUF                                                        
      INTEGER I,J,LRN1,DME,HGME,CHGME,THR,PMK,lk                                            
      data tnum/0/
      data trate/300*0/
      data tname/300*' '/
                                                                                
                                                                                
 2085 FORMAT (1X,A6)                                                            
 2095   FORMAT(1X,A6,21X,I4,34X,A8)                                             
 2096 FORMAT(A80)                                                               
                                                                                
   20 READ (25,2085) DUMMY1                                                     
      IF (DUMMY1 .EQ. 'ORDVAR') GOTO 10                                         
      GOTO 20                                                                   
                                                                                
   10 CONTINUE                                                                  
      TNUM = 0                                                                  
      lk=0
      PMK = 0                                                                   
      THR =1                                                                    
   30 TNUM =  TNUM+1                                                            
   31 READ(25,2096) INBUF                                                       
      IF (INBUF(1:1) .NE. '/') THEN                                             
        READ(INBUF,2095) DUMMY1,TRATE(TNUM),TNAME(TNUM)                         
        IF (DUMMY1(1:4) .EQ. 'RATE') GOTO 40                                    
        IF (TNAME(TNUM) .EQ. 'LORN    ') LRN1 = 1                               
        IF (TNAME(TNUM) .EQ. 'DME     ') DME = 1                                
        IF (TNAME(TNUM) .EQ. 'HGME    ') HGME = 1                               
        IF (TNAME(TNUM) .EQ. 'CHGME   ') CHGME = 1                              
        IF (TNAME(TNUM) .EQ. 'PHDG    ') THR = TRATE(TNUM)                      
        IF (TNAME(TNUM) .EQ. 'ASAS    ') PMK= 1                                 
        IF (TNAME(TNUM) .EQ. 'X260    ') PMK= 1                                 
        IF (TNAME(TNUM) .EQ. 'X200    ') PMK= 1                                 
        IF (TNAME(TNUM) .EQ. 'Y200    ') PMK= 1                                 
        IF (TNAME(TNUM) .EQ. 'FSSP    ') PMK= 1                                 
        IF (TNAME(TNUM) .EQ. 'DTT     ') lk= 1                                 
        IF (TNAME(TNUM) .EQ. 'DTB     ') lk= 1                                 
        IF (TNAME(TNUM) .EQ. 'STB     ') lk= 1                                 
        IF (TNAME(TNUM) .EQ. 'STT     ') lk= 1                                 
        GOTO 30                                                                 
      ELSE                                                                      
        GOTO 31                                                                 
      ENDIF                                                                     
                                                                                
   40 TNUM = TNUM - 1                                                           
                                                                                
                                                                                
      DO 100, I = 1,600                                                         
         RATE(I) = 999                                                          
  100 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                   SUBROUTINE SRT                                               
C--------SRT sets the rates for all raw variables.   It matches the             
C        variables to be used in the CALIB operation (NAME array) with          
C        the variables from the input operation (TNAME).  The end               
C        result is the RATE array with the rates or raw variables and           
C        probes runnng parrallel to the NAME array.                             
                                                                                
      include "gparc.h"
      include "gprat.h"
      include "gprad.h"
                                                                                
      CHARACTER*6 DUMMY1                                                  
      INTEGER I,J,LRN1,DME,HGME,CHGME,THR,ASRT,FSRT,X2RT,Y2RT,X6RT                          
      CHARACTER*1 DUMMY2                                                  
                                                                                
                                                                                
                                                                                
                                                                                
      DO 110 I = 1, NUMVAR                                                      
         DO 120, J= 1, TNUM                                                     
            IF (NAME(I) .EQ. TNAME(J))  RATE(I) = TRATE(J)                      
  120    CONTINUE                                                               
         IF (NAME(I) .EQ. 'X260    ') X6RT = RATE(I)                            
         IF (NAME(I) .EQ. 'X200    ') X2RT = RATE(I)                            
         IF (NAME(I) .EQ. 'Y200    ') Y2RT = RATE(I)                            
         IF (NAME(I) .EQ. 'FSSP    ') FSRT = RATE(I)                            
         IF (NAME(I) .EQ. 'ASAS    ') ASRT = RATE(I)                            
  110 CONTINUE                                                                  
                                                                                
      DO 140, I=1,NUMVAR                                                        
         IF (NAME(I) .EQ. 'SUMX6   ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'CONC6   ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'DBAR6   ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'DISP6   ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'PLWC6   ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'DBZ6    ') RATE(I) = INT(FLOAT(X6RT)/70.0)           
         IF (NAME(I) .EQ. 'ACCUMX6 ') RATE(I) = INT((FLOAT(X6RT)/70.0)          
     $   *62.0)                                                                 
         IF (NAME(I) .EQ. 'CONCX6  ') RATE(I) = INT((FLOAT(X6RT)/70.0)          
     $   *62.0)                                                                 
         IF (NAME(I) .EQ. 'SUM15F  ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'CONCF   ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'DBARF   ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'DISPF   ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'FACT    ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'PLWCF   ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'FBMFR   ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'FRANGE  ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'FRESET  ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I) .EQ. 'FSTROB  ') RATE(I) = INT(FLOAT(FSRT)/21.0)           
         IF (NAME(I)(1:4) .EQ. 'AFSP') RATE(I) = INT(FLOAT(FSRT)/21.0)          
         IF (NAME(I)(1:4) .EQ. 'CFSP') RATE(I) = INT(FLOAT(FSRT)/21.0)          
         IF (NAME(I) .EQ. 'PLWCX   ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I) .EQ. 'SUM15X  ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I) .EQ. 'CONCX   ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I) .EQ. 'DBARX   ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I) .EQ. 'DISPX   ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I) .EQ. 'DBZX    ') RATE(I) = INT(FLOAT(X2RT)/21.0)           
         IF (NAME(I)(1:4) .EQ. 'A20X') RATE(I) = INT(FLOAT(X2RT)/21.0)          
         IF (NAME(I)(1:4) .EQ. 'C20X') RATE(I) = INT(FLOAT(X2RT)/21.0)          
         IF (NAME(I) .EQ. 'SUM15Y  ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I) .EQ. 'CONCY   ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I) .EQ. 'DBARY   ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I) .EQ. 'DISPY   ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I) .EQ. 'PLWCY   ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I) .EQ. 'DBZY    ') RATE(I) = INT(FLOAT(Y2RT)/21.0)           
         IF (NAME(I)(1:4) .EQ. 'C20Y') RATE(I) = INT(FLOAT(X2RT)/21.0)          
         IF (NAME(I)(1:4) .EQ. 'A20Y') RATE(I) = INT(FLOAT(X2RT)/21.0)          
         IF (NAME(I) .EQ. 'SUM15A  ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I) .EQ. 'CONCA   ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I) .EQ. 'DBARA   ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I) .EQ. 'DISPA   ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I) .EQ. 'AACT    ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I) .EQ. 'CAACT   ') RATE(I) = INT(FLOAT(ASRT)/21.0)           
         IF (NAME(I)(1:4) .EQ. 'AASS') RATE(I) = INT(FLOAT(ASRT)/21.0)          
         IF (NAME(I)(1:4) .EQ. 'CASS') RATE(I) = INT(FLOAT(ASRT)/21.0)          
  140 CONTINUE                                                                  
                                                                                
                                                                                
  130 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                  SUBROUTINE VECGEN(RNAM,LIM)                                   
C----This subroutine takes the RNAM array, and generates a VECVAR from          
C    it.                                                                        
                                                                                
                                                                                
                                                                                
                                                                                
      CHARACTER*8 RNAM(1)                                           
      CHARACTER*9 VNAME(600),DUMMY                                              
      INTEGER  I, J, K,SKIPS,LIM                                                
                                                                                
      DO 30, I = 1, 600                                                         
         VNAME(I) = '        '                                                  
   30 CONTINUE                                                                  
                                                                                
      SKIPS = 0                                                                 
      DO 10, I=1, LIM                                                           
            DUMMY(1:8) = RNAM(I)                                                
            DUMMY(9:9) = ','                                                    
            VNAME(I) = DUMMY                                                    
   10 CONTINUE                                                                  
      DUMMY = VNAME(LIM)                                                        
      DUMMY(9:9) = ' '                                                          
      VNAME(LIM) = DUMMY                                                        
      K = INT(FLOAT(LIM)/6.0)                                                   
      DO 20, J =0, K                                                            
         M = (6*J) + 1                                                          
         IF (J .EQ. 0) THEN                                                     
            WRITE(97,2001) (VNAME(L), L= M, M+5)                                
         ELSE                                                                   
            WRITE(97,2002) (VNAME(L), L=M, M+5)                                 
      ENDIF                                                                     
   20 CONTINUE                                                                  
 2001 FORMAT(' VECVAR= ',6A9)                                               
 2002 FORMAT(8X,6A9)                                                            
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
                                                                                
                   SUBROUTINE STDSET                                            
C--------This subroutine simply creates the STDVAR array, containing            
C    all Bulletin Nine variables present in the  INPUT operation.               
C    That is, all the Bull. 9 that come out of ADSUD.                           
                                                                                
      include "gparc.h"
      include "gprad.h"
      include "gpstd.h"
      include "gpstv.h"
                                                                                
      INTEGER  I, J
                                                                                
                                                                                
                                                                                
                                                                                
      NMSTV = 0                                                                 
      DO 10, I=1, TNUM                                                          
        DO 11, J = 1, VAREND                                                    
          IF((TNAME(I) .EQ. VARNAM(J)) .AND. (STAD(J) .EQ. 'STD')) THEN         
             NMSTV=NMSTV+1                                                      
             STDVAR(NMSTV) = VARNAM(J)                                          
          ENDIF                                                                 
  11    CONTINUE                                                                
  10    CONTINUE                                                                
        RETURN                                                                  
        END                                                                     
                                                                                
                                                                                
                                                                                
                  SUBROUTINE OUTGEN(OUTAR,NUM,rlim)                             
C----This subroutine generates a VECVAR for the output operation, it            
C    takes the PRINTS/PLOTS/STATS input array and filters it.  It               
C    then arranges the filtered array in ascending RATE order and               
C    calls VECGEN to generate a VECVAR.  The filtering removes probe            
C    names and differences, and adds HR, MIN, SEC, TPTIME, PTIME.               
C    Adds all Bulletin Nine Variables present in INPUT to OUTPUT                
C    VECVAR, per request from GH.                                               
                                                                                
      include "gparc.h"
      include "gprat.h"
      include "gprad.h"
      include "gpkey.h"
      include "gpstv.h"
                                                                                
      INTEGER LIM,FFLAG,rlim,rtemp,num             
      CHARACTER*8 OAR(600),OUTAR(1)
                                                                                
C-------Common block variables                                                  
      INTEGER  VRATE(600)                                         
C---------Local variables                                                       
      CHARACTER*8 VNAME(600),VNAME2(600)                                        
      INTEGER I,J,K,SKIPS,SKIPS2,VFLAG                                          
                                                                                


      NMKEY=1                                                                   
1002  READ(52,1000,END=1001)KEY(NMKEY)                                          
1000  FORMAT(A8)                                                                
      NMKEY=NMKEY + 1                                                           
      GOTO 1002                                                                 
1001  NMKEY=NMKEY -1                                                            
                                                                                
                                                                                
      DO 30, I = 1, 600                                                         
         VNAME(I) = '      '                                                    
   30 CONTINUE                                                                  
      LIM=NUM                                                                   
      DO 31, I = 1, LIM                                                         
         OAR(I) = OUTAR(I)                                                      
   31 CONTINUE                                                                  
C--------CHECKS FOR PRESENCE OF STANDARD VARIABLES AND ADDS THEM TO             
C          OUTPUT VECVAR, AS PER REQUEST GH 10/88                               
      DO 32, I = 1, NMSTV                                                       
         OAR(I+LIM) = STDVAR(I)                                                 
   32 CONTINUE                                                                  
      LIM=LIM+NMSTV                                                             
      CALL PBCHK(OAR,LIM,0,600)                                                 
      CALL RCLR(OAR,LIM,600)                                                    
      SKIPS = 0                                                                 
      DO 10, I=1, LIM                                                           
C------------Remove probes and differences.                                     
            DO 11, J=1, NMKEY                                                   
               IF (OAR(I) .EQ. KEY(J)) GOTO  10                                 
   11       CONTINUE                                                            
            IF (OAR(I)(1:4) .EQ. 'GUST') GOTO 10                                
            IF (OAR(I)(1:4) .EQ. 'LYAP') GOTO 10                                
            IF (OAR(I)(1:4) .EQ. 'FSSP') GOTO 10                                
            IF (OAR(I)(1:5) .EQ. 'ASAS ') GOTO 10                               
            IF (OAR(I)(1:4) .EQ. 'X200') GOTO 10                                
            IF (OAR(I)(1:4) .EQ. 'X260') GOTO 10                                
            IF (OAR(I)(1:4) .EQ. 'Y200') GOTO 10                                
            IF (OAR(I)(1:2) .EQ. 'DF') GOTO 10                                  
            IF (OAR(I) .EQ. 'HR      ') GOTO 10                                 
            IF (OAR(I) .EQ. 'MIN     ') GOTO 10                                 
            IF (OAR(I) .EQ. 'SEC     ') GOTO 10                                 
            IF (OAR(I) .EQ. 'TPTIME  ') GOTO 10                                 
            IF (OAR(I) .EQ. 'PTIME   ') GOTO 10                                 
            SKIPS = SKIPS +1                                                    
            VNAME(SKIPS) = OAR(I)                                               
   10 CONTINUE                                                                  
                                                                                
      VNAME2(1) = 'HR      '                                                    
      VNAME2(2) = 'MIN     '                                                    
      VNAME2(3) = 'SEC     '                                                    
      VNAME2(4) = 'TPTIME  '                                                    
      VNAME2(5) = 'PTIME   '                                                    
      SKIPS2 = 5                                                                
                                                                                
      RTEMP=1                                                                   
 2222 DO 13, I = 1,SKIPS                                                        
         FFLAG = 0                                                              
         VFLAG = 0                                                              
         DO 14, J= 1, NUMVAR                                                    
            IF (NAME(J) .EQ. VNAME(I)) THEN                                     
               VFLAG = 1                                                        
               IF (RATE(J) .EQ. RTEMP)  THEN                                    
                   SKIPS2 = SKIPS2 + 1                                          
                   VNAME2(SKIPS2) = VNAME(I)                                    
                   FFLAG=1                                                      
               ENDIF                                                            
            ENDIF                                                               
   14    CONTINUE                                                               
         IF ((FFLAG .EQ. 0) .AND. (VFLAG .EQ. 0)) THEN                          
         DO 15, J= 1, TNUM                                                      
            IF (TNAME(J) .EQ. VNAME(I)) THEN                                    
               IF (TRATE(J) .EQ. RTEMP)  THEN                                   
                   SKIPS2 = SKIPS2 + 1                                          
                   VNAME2(SKIPS2) = VNAME(I)                                    
               ENDIF                                                            
            ENDIF                                                               
   15    CONTINUE                                                               
      ENDIF                                                                     
   13 CONTINUE                                                                  
      RTEMP = RTEMP + 1                                                         
      IF ((SKIPS2 .LT. (SKIPS+5)) .AND. (RTEMP .LT. 999))  GOTO 2222            
      CALL VECGEN(VNAME2,SKIPS2)                                                
      WRITE(99) SKIPS2,(VNAME(J),J=1,SKIPS2)                                    
                                                                                
      RETURN                                                                    
      END                                                                       
                  SUBROUTINE PLGEN(ARR,LIM)                                     
C----This subroutine generates a VECFUN for all the variables for               
C    which plots have been requested.  It, therefore, uses the FNAME            
C    array.  The FNAME array is generated in the FSET routine, and FSET         
C    calls PLGEN.                                                               
                                                                                
      include "gparc.h"
      include "gprfl2.h"
                                                                                
C---------Common block variables.                                               
      CHARACTER*8 ARR(1)                             
C------------Local variables.                                                   
      CHARACTER*9 VNAME(600),DUMMY                                              
      INTEGER I,J,K,SKIPS,LIM                                                   
                                                                                
      DO 30, I = 1, LIM                                                         
         VNAME(I)(1:8) = ARR(I)                                                 
         VNAME(I)(9:9) = ','                                                    
   30 CONTINUE                                                                  
                                                                                
      DO 31, I = LIM +1, 600                                                    
         VNAME(I) = '         '                                                 
   31 CONTINUE                                                                  
                                                                                
      VNAME(LIM)(9:9) = ' '                                                     
      K = INT(FLOAT(LIM)/6.0)                                                   
      DO 20, J =0, K                                                            
         M = (6*J) + 1                                                          
         IF (J .EQ. 0) THEN                                                     
            WRITE(97,2001) (VNAME(L), L= M, M+5)                                
         ELSE                                                                   
            WRITE(97,2002) (VNAME(L), L=M, M+5)                                 
      ENDIF                                                                     
   20 CONTINUE                                                                  
 2001 FORMAT(' VECFUN= ',6A9)                                  
 2002 FORMAT(1x,8X,6A9)                            
                                                                                
      RETURN                                                                    
      END                                                                       
