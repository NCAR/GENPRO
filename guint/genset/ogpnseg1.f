      SUBROUTINE NSEG1                                                          
C                                                                               
C  This routine contains most of the interactive setup                          
C                                                                               
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gpio.h"                                                          
C     INCLUDE "gpusrvar.h"                                                      
      INTEGER INTERR                                                            
      COMMON/IOERR/INTERR                                                       
C                                                                               
      CHARACTER*8 FILNAM,NAMIN                                                  
      CHARACTER*8 CNAME,BLANK                                                   
      CHARACTER*4 LNAME,VINPUT,PROBE                                            
      CHARACTER*50 NEXDES                                                       
      CHARACTER*5 IFLT                                                          
      CHARACTER*2 INP,IPM,NINPUT,IPRTC, IOPT                                    
      CHARACTER *1 IEXAM,atemp                                                  
      CHARACTER*20 STRING                                                       
C save space for project QC sensors -- restored as default sensors              
      CHARACTER *3 PRQC(2)                                                      
C  NUMCYC: number of operations involved in GENPRO                              
      INTEGER NUMCYC                                                            
      PARAMETER (NUMCYC=5)                                                      
C  CYCLES: flags for operations enabled/disabled                                
      LOGICAL*1 CYCLES(NUMCYC)                                                  
      EQUIVALENCE (CYCLES(1),OUTCYC)                                            
C  ONOPS, OFFOPS: lists of op'ns that are on and off                            
      CHARACTER *60 ONOPS,OFFOPS                                                
C  OPLIST: string of 'ON' and 'OFF' corresponding to each operation             
      CHARACTER*4 OPLIST(NUMCYC)                                                
C  NAMCYC: names of operations                                                  
      CHARACTER*8 NAMCYC(NUMCYC)                                                
C  NUMDIS: number of disposals possible in GENPRO                               
      INTEGER NUMDIS                                                            
      PARAMETER (NUMDIS=8)                                                      
C  DISPOS: flags for disposals enabled/disabled                                 
      LOGICAL*1 DISPOS(NUMDIS)                                                  
      EQUIVALENCE (DISPOS(1),PR2D1)                                             
C  ONDIS, OFFDIS: lists of disposals that are on and off                        
      CHARACTER *200 ONDIS,OFFDIS                                               
C  DILIST: string of 'ON' and 'OFF' corresponding to each operation             
      CHARACTER*4 DILIST(NUMDIS)                                                
C  NAMDIS:  names of disposals                                                  
      CHARACTER*25 NAMDIS(NUMDIS)                                               
C  Special Plot directive character strings and booleans                        
      CHARACTER*8 NAMFUN,XVAR1,YVAR1,YVAR2                                      
      CHARACTER*8 XBOT,XRANGE,YBOT,YRANGE                                       
      LOGICAL XAXIS,MLTIPL                                                      
C  FOUND: logical sent to NEWFLT, false if flight # chosen not found            
      LOGICAL FOUND                                                             
C  BTIM,ETIM are time interval setup variables                                  
      DIMENSION BTIM(50),ETIM(50)                                               
C  IOPTN is the interactive command list                                        
      CHARACTER*4 BLNK                                                          
C  PMSNMC: PMS1D variables                                                      
      CHARACTER*8 PMSNMC(25)                                                    
C FTIME is adjusted start/end time of flight (adjusted via snapshots)           
      INTEGER FTIME(6)                                                          
C  ISTRNG = scratch array for input,FIN = Temporary Decode output               
      DIMENSION FIN(5)                                                          
      CHARACTER*80 ISTRNG                                                       
C  string to indicate whether all or part of flight will fit on output          
C  physical tape                                                                
      CHARACTER *7 OUTLEN                                                       
C number of IA options in Parameters menu                                       
      INTEGER NOPTN                                                             
      PARAMETER (NOPTN=14)                                                      
      CHARACTER*2 IOPTN(NOPTN)                                                  
      DATA IOPTN/'HE','SE','CA','CC','DI','CH','MC','PD',                       
     1 'SA','SH','EP','DP','EX','US'/                                  
C                                                                               
      DATA PMSNMC( 1)/'CONCF '/                                                 
      DATA PMSNMC( 2)/'DBARF '/                                                 
      DATA PMSNMC( 3)/'DISPF '/                                                 
      DATA PMSNMC( 4)/'PLWCF '/                                                 
      DATA PMSNMC( 5)/'      ' /                                                
      DATA PMSNMC( 6)/'CONCA '/                                                 
      DATA PMSNMC( 7)/'DBARA '/                                                 
      DATA PMSNMC( 8)/'DISPA '/                                                 
      DATA PMSNMC( 9)/'      ' /                                                
      DATA PMSNMC(10)/'      ' /                                                
      DATA PMSNMC(11)/'CONCX '/                                                 
      DATA PMSNMC(12)/'DBARX '/                                                 
      DATA PMSNMC(13)/'DISPX '/                                                 
      DATA PMSNMC(14)/'PLWCX '/                                                 
      DATA PMSNMC(15)/'DBZX  '/                                                 
      DATA PMSNMC(16)/'CONCY '/                                                 
      DATA PMSNMC(17)/'DBARY '/                                                 
      DATA PMSNMC(18)/'DISPY '/                                                 
      DATA PMSNMC(19)/'PLWCY '/                                                 
      DATA PMSNMC(20)/'DBZY  '/                                                 
      DATA PMSNMC(21)/'CONC6 '/                                                 
      DATA PMSNMC(22)/'DBAR6 '/                                                 
      DATA PMSNMC(23)/'DISP6 '/                                                 
      DATA PMSNMC(24)/'PLWC6 '/                                                 
      DATA PMSNMC(25)/'DBZ6  '/                                                 
C                                                                               
      DATA NAMCYC/'Output','Print','Plot','Plot2','Stats'/                      
      DATA NAMDIS/'Print to microfilm   ', 'Print to Mass Store   ',            
     $            'Print to IBM reader   ','Plot to IBM reader   ',             
     $            'Plot to microfilm   ',                                       
     $            'Plot to Mass Store   ', 'Plot to Laser Printer   ',          
     $            'Output to Mass Store   '/                                    
      DATA BTIM(1)/0.0/ ETIM(1)/86399./                                         
      DATA BLANK/'        '/ ,BLNK/'    ' /                                     
      DATA LNAME/'LRNC'/                                                        
C                                                                               
C                                                                               
C***   initialize;  CALCHG is false until calib. coeff's. are changed           
C                                                                               
      CALCHG=.FALSE.                                                            
      IEND=0                                                                    
C  set following to true only when flight is saved or initialized,              
C  respectively                                                                 
      FLTSAV=.FALSE.                                                            
      FLTSET=.FALSE.                                                            
      CALL system("clear")                                                      
      WRITE(LUTO,19709)                                                         
19709 FORMAT(' Select 1: initial project setup ',/,                            
     $       '        2: restore project setup')                              
C  invisible selection # 3 implies restore an old file format, save it          
C  in the new format, restore the resulting new file                            
29709 READ(LUTI,'(Bz,I4)',END=19710,err=70007) IMODE                      
      if (imode .eq. 0) goto 19710
      GOTO 19711                                                                
70007 WRITE(6,'('' Non-integer input; try again'')')                           
      GOTO 29709                                                               
19710 continue                                                                  
      RETURN                                                                    
19711 IF (IMODE.EQ.1) THEN                                                      
       CALL INIPRO                                                              
      ELSEIF (IMODE.EQ.2.OR.IMODE.EQ.3) THEN                                    
       CALL RESPRO(IMODE)                                                       
       IF (IERR.NE.0) THEN                                                      
        IERR=0                                                                  
        IMODE=1                                                                 
       ENDIF                                                                    
      ENDIF                                                                     
      IF (IERR.EQ.1) RETURN                                                     
C  save project QC sensors in case a flight chooses different ones              
      PRQC(1)=QCREF(1)                                                          
      PRQC(2)=QCREF(2)                                                          
C                                                                               
      CALL system("clear")                                                      
C disallow restore of previous flight setup if initial project setup            
      IF (IMODE.EQ.1) THEN                                                      
       CALL INIFLT                                                              
       GOTO 4                                                                   
      ENDIF                                                                     
      WRITE(LUTO,19719)                                                         
19719 FORMAT(' Select 1: initial flight setup ',/,                        
     $       '        2: restore flight setup')                      
29719 READ(LUTI,'(I1)',END=19720,err=70008) IMODE           
      if (imode .eq. 0) goto 19720
      GOTO 19721                                                                
70008 WRITE(6,'('' Non-integer input; try again'')')                           
      GOTO 29719                                                               
19720 continue                                                                  
      RETURN                                                                    
19721 IF (IMODE.EQ.1) THEN                                                      
       CALL INIFLT                                                              
      ELSE                                                                      
       CALL RESFLT                                                              
       IF (.NOT.(FLTSET)) THEN                                                  
        WRITE(LUTO,19991)                                                       
19991   FORMAT(' First you must either initialize or restore a flight s'        
     $  ,'etup, or you may',/                                                   
     $  ' simply return to Setup Menu ',/                                       
     $  ,' Select 1: initialize 2: restore 3: return to Setup Menu')            
29991   READ(LUTI,'(I1)',END=20001,err=70009) IMODE      
        if (mode .eq. 0) goto 20001
        IF(IMODE.NE.3) GOTO 19721                                               
        goto 20001
70009    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 29991                                                             
20001   continue                                                                
        RETURN                                                                  
       ENDIF                                                                    
      ENDIF                                                                     
C default Menu on                                                               
    4 MENUPR=1                                                                  
    5 continue                                                                  
      CALL system("clear")                                                      
      IF(MENUPR.EQ.1)WRITE(LUTO,9910)                                           
 9910 FORMAT(T34,' PARAMETERS MENU',//,                                     
     $ T20,' HE  Help ',/,                                                   
     $ T20,' SE  Select output variables (plot,print,stats) ',/,             
c     3 T20,' CA  Change FSSP calibration values',/,          
c     4 T20,' CC  Change calibration coefficients ',/,                    
     6 T20,' DI  Display Current Setup',/,                                    
     5 T20,' CH  Change Current Setup ',/,                              
c     A T20,' MC  Change miscellaneous values',/,                         
     $ T20,' PD  Change Plot Display Order',/,                          
     3 T20,' US  Add user-defined variables',/,                       
     7 T20,' SA  Save current flight/project setups',/,                 
c     7 T20,' SH  Menu listing on/off switch',/,                    
     8 T20,' EP  Enable Processing',/,                                
c     8 T20,' DP  Disable Processing',/,                              
     8 T20,' EX  Exit, return to Setup Menu')                               
      IF (MENUPR.EQ.1) THEN                                                     
       WRITE(LUTO,'(/,'' Enter name of option_'')')                     
      ELSE                                                                      
       WRITE(LUTO,'(/,'' Enter name of option (SH to show menu)_ '')')       
      ENDIF                                                                     
C Read in the command                                                           
      READ(LUTI,'(A2)',END=9999)IOPT                                            
      if (iopt .eq. "") goto 9999
  666 CALL system("clear")                                                      
C Check for legality of command                                                 
      CALL caps(IOPT,2)                                                        
 5557 DO 6 IO=1,NOPTN                                                           
       IF(IOPT.EQ.IOPTN(IO))GO TO 7                                             
   6  CONTINUE                                                                  
      CALL PAUSER(' Illegal Command ... hit <r> to continue')                   
      GO TO 5                                                                   
C            HE, SE, CA, CC, DI, CH,  MC,                                       
  7   GO TO(150,160,330,400,600,1100,1000,                                      
C        PD, SA, SH, EP,  DP, EX, US                                            
     $  3651,700,898,800,850,9999,1200)IO                       
C                                                                               
 150  CALL PAUSER(' Sorry, no help installed yet ... hit <r>')                  
      GOTO 5                                                                    
C                                                                               
C  Determine output vars for Plot/Print/Stats or for Plot2 operations           
C                                                                               
 160  CALL system("clear")                                                      
      WRITE(LUTO,'('' You may select output variables for either the ''/        
     $'' general output operations (plot, print, stats) or for the''/   
     $'' complete flight plot. '')')        
      WRITE(LUTO,'(/'' Choose: (1) General (2) Complete Flight''/)')            
21160 READ(LUTI,'(I1)',END=5,err=70011) IC     
      if (ic .eq. 0) goto 5
      goto 70010
70011 WRITE(6,'('' Non-integer input; try again'')')                           
      GOTO 21160                                                               
70010 IF (IC.EQ.1) THEN                                                         
       CALL SELECT(VRPOSS,NMPOSS,PSPPTR,PSPOUT)                                 
      ELSE                                                                      
       CALL SELECT(VRPOSS,NMPOSS,PL2PTR,PL2OUT)                                 
      ENDIF                                                                     
      GO TO 160                                                                 

 330  DO 231 IP=1,NUMPMS                                                        
       IF (PNAME(IP).EQ.'FSSP') THEN                                            
 232    CALL system("clear")                                                    
 335    WRITE(LUTO,9332)                                                        
        WRITE(LUTO,9334)DOF,BDIA,TAU1,TAU2,TAU3                                 
 9332   FORMAT(16(/),' Enter change within indicated field',/,                  
     1  ' DOF   BDia    Tau1     Tau2     Tau3',/)                            
9334    FORMAT(1x,2(1X,F5.2),3(1X,F9.7))                                       
9344    FORMAT(2(1X,F5.2),3(1X,F9.7))                                       
        WRITE(LUTO,9333)                                                        
 9333   FORMAT(1x,2(6H -----),3(10H ---------))                              
        READ(LUTI,'(A42)',END=7710) ISTRNG                                      
        if (istrng .eq. '') goto 7710
        GOTO 711                                                                
 7710   continue
        GOTO 5                                                                
  711   READ(ISTRNG,9344,end=70013) FIN       
        goto 70012
70013   WRITE(6,'('' Non-integer input; try again'')')                         
        GOTO 711                                                               
70012   CALL BLNKS(FIN(1),DOF,ISTRNG(2:6),5)                                    
        CALL BLNKS(FIN(2),BDIA,ISTRNG(8:12),5)                                  
        CALL BLNKS(FIN(3),TAU1,ISTRNG(14:22),9)                                 
        CALL BLNKS(FIN(4),TAU2,ISTRNG(24:32),9)                                 
        CALL BLNKS(FIN(5),TAU3,ISTRNG(34:42),9)                                 
        GO TO 232                                                               
       ENDIF                                                                    
  231 CONTINUE                                                                  
      GO TO 5                                                                
C  Write out all available variables for this list                              
C 360 CALL system("clear")                                                      
C     DO 361 I=1,NUMPMS                                                         
C     NP=IPTAB(I)                                                               
C  Accumulations                                                                
C     WRITE(LUTO,9360)(PMSFAX(J),J=PMSTAB(NP),PMSTAB(NP)+                       
C    $ PMSTAB(NP+5)-1)                                                          
C9360 FORMAT(1X,9A8)                                                            
C  Concentrations                                                               
C     WRITE(LUTO,9360)(PCONC(J),J=PMSTAB(NP+10),PMSTAB(NP+10) +                 
C    $ PMSTAB(NP+15)-1)                                                         
C 361 CONTINUE                                                                  
C     WRITE (6,'(//'' Hit <r> to continue'')')                                  
C     READ(LUTI,'(A)',END=6362) I                                               
C362  continue
C     GO TO 301                                                                 
C     Add/delete raw PMS counts to the parameter output list                    
C365  continue        
C     CALL system("clear")                                                      
C     IF (NUMPMS.GT.1) THEN                                                     
C      WRITE(LUTO,1716)                                                         
C1716  FORMAT(/' Select probe by number: '/)                                    
C      DO 21361 J=1,NUMPMS                                                      
C1361   WRITE(LUTO,22361) J,PNAME(J)                                            
C2361  FORMAT(2X,I2,':',2X,A8)                                                  
C     ELSE                                                                      
C      IF (NUMPMS.GT.0) THEN                                                    
C       IPNUM=1                                                                 
C       GOTO 56574                                                              
C      ELSE                                                                     
C       WRITE (6,'(/'' No probes available..hit <r> to continue''/)')           
C       READ(LUTI,'(A)',END=22362) I                                            
C2362   continue  
C       GOTO 301                                                                
C      ENDIF                                                                    
C     ENDIF                                                                     
C2362 READ(LUTI,'(I1)',END=301,err=70014) IPNUM     
C     if (ipnum .eq. 0) goto 301
C     goto 56574
C0014 WRITE(6,'('' Non-integer input; try again'')')                           
C     GOTO 32362                                                               
C adjust IPNUM to reference the actual ordinal position of the probe            
C6574 IPNUM=IPTAB(IPNUM)                                                        
C6573 CALL system("clear")                                                      
C     WRITE(LUTO,'(/'' Probe: '',A5,/)') PNAM(IPNUM)                            
C     WRITE (LUTO,56575)                                                        
C6575 FORMAT( ' Select (1) Accumulations (2) Concentrations ')                  
C6575 READ(LUTI,'(I1)',END=301,err=70015) IPNUM2      
C     if (ipnum2 .eq. 0) goto 301
C     goto 70016
CrC15 WRITE(6,'('' Non-integer input; try again'')')                           
C     GOTO 66575                                                               
C0016 IF (IPNUM2.EQ.2) THEN                                                     
C after a PNAME index has been referenced via IPTAB, use PNAM array             
C      CNAME=PNAM(IPNUM)//'C'                                                   
C concentration array's and its pointer array's starting index                  
C      ISTART=PMSTAB(IPNUM+10)                                                  
C # of words to be considered for selection                                     
C      LENGTH=PMSTAB(IPNUM+15)                                                  
C which derives the end address in the array                                    
C      ISTOP=ISTART+LENGTH-1                                                    
C number selected on entry                                                      
C      NMPICK=NCON(IPNUM)                                                       
C revise selection set                                                          
C      CALL SELECT(PCONC(ISTART),LENGTH,PTRCON(ISTART),NCON(IPNUM))             
C      IF (NCON(IPNUM).EQ.LENGTH.AND.NMPICK.LT.LENGTH) THEN                     
C The selection just completed designated all of this class, which was          
C not the case before the call to SELECT.....                                   
C so, first remove any names in this class from the individual mode array       
C       DO 57766 J=ISTART,ISTOP                                                 
C        DO 57765 K=1,NPMS1D                                                    
C         IF(PCONC(J).EQ.PMS1D(K)) THEN                                         
C          DO 57764 M=K,NPMS1D-1                                                
C7764       PMS1D(M)=PMS1D(M+1)                                                 
C          NPMS1D=NPMS1D-1                                                      
C          GOTO 57766                                                           
C         ENDIF                                                                 
C7765    CONTINUE                                                               
C7766   CONTINUE                                                                
C ...and turn on the keyword for this probe                                     
C       NPVARS=NPVARS+1                                                         
C       PRNAME(NPVARS)=CNAME                                                    
C      ELSEIF (NCON(IPNUM).LT.LENGTH) THEN                                      
C pointer array does not have all indices; use individual var mode              
C if this probe was in the keyword list, remove it                              
C       IF (NMPICK.EQ.LENGTH) THEN                                              
C        DO 56676 J=1,NPVARS                                                    
C         IF(PRNAME(J).EQ.CNAME) THEN                                           
C          DO 56677 M=J,NPVARS-1                                                
C6677       PRNAME(M)=PRNAME(M+1)                                               
C          PRNAME(NPVARS)=' '                                                   
C          NPVARS=NPVARS-1                                                      
C         ENDIF                                                                 
C6676    CONTINUE                                                               
C       ELSE                                                                    
C  adjust NPMS1D                                                                
C        NPMS1D=NPMS1D-NMPICK                                                   
C       ENDIF                                                                   
C ...and reset the individual variable array names as per selection             
C       DO 81360 J=ISTART,ISTART+NCON(IPNUM)-1                                  
C        NPMS1D=NPMS1D+1                                                        
C1360    PMS1D(NPMS1D)=PCONC (PTRCON(J)+ISTART-1)                               
C      ENDIF                                                                    
C     ELSEIF (IPNUM2.EQ.1) THEN                                                 
C  accumulations                                                                
C after a PNAME index has been referenced via IPTAB, use PNAM array             
C      CNAME=PNAM(IPNUM)//'A'                                                   
C accumulation array's and its pointer array's starting index                   
C      ISTART=PMSTAB(IPNUM)                                                     
C # of words to be considered for selection                                     
C      LENGTH=PMSTAB(IPNUM+5)                                                   
C which derives the end address in the array                                    
C      ISTOP=ISTART+LENGTH-1                                                    
C number selected on entry                                                      
C      NMPICK=NACC(IPNUM)                                                       
C revise selection set                                                          
C      CALL SELECT(PMSFAX(ISTART),LENGTH,PTRACC(ISTART),NACC(IPNUM))            
C      IF (NACC(IPNUM).EQ.LENGTH.AND.NMPICK.LT.LENGTH) THEN                     
C The selection just completed designated all of this class, which was          
C not the case before the call to SELECT.....                                   
C so, first remove any names in this class from the individual mode array       
C       DO 67766 J=ISTART,ISTOP                                                 
C        DO 67765 K=1,NPMS1D                                                    
C         IF(PMSFAX(J).EQ.PMS1D(K)) THEN                                        
C          DO 67764 M=K,NPMS1D-1                                                
C7764       PMS1D(M)=PMS1D(M+1)                                                 
C          NPMS1D=NPMS1D-1                                                      
C          GOTO 67766                                                           
C         ENDIF                                                                 
C7765    CONTINUE                                                               
C7766   CONTINUE                                                                
C ...and turn on the keyword for this probe                                     
C       NPVARS=NPVARS+1                                                         
C       PRNAME(NPVARS)=CNAME                                                    
C      ELSEIF (NACC(IPNUM).LT.LENGTH) THEN                                      
C pointer array does not have all indices; use individual var mode              
C if this probe was in the keyword list, remove it                              
C       IF (NMPICK.EQ.LENGTH) THEN                                              
C        DO 66676 J=1,NPVARS                                                    
C         IF(PRNAME(J).EQ.CNAME) THEN                                           
C          DO 66677 M=J,NPVARS-1                                                
C6677       PRNAME(M)=PRNAME(M+1)                                               
C          PRNAME(NPVARS)=' '                                                   
C          NPVARS=NPVARS-1                                                      
C         ENDIF                                                                 
C6676    CONTINUE                                                               
C       ELSE                                                                    
C  adjust NPMS1D                                                                
C        NPMS1D=NPMS1D-NMPICK                                                   
C       ENDIF                                                                   
C ...and reset the individual variable array names as per selection             
C       DO 71360 J=ISTART,ISTART+NACC(IPNUM)-1                                  
C        NPMS1D=NPMS1D+1                                                        
C1360    PMS1D(NPMS1D)=PMSFAX(PTRACC(J)+ISTART-1)                               
C      ENDIF                                                                    
C     ENDIF                                                                     
C     GOTO 56573                                                                
C                                                                               
C                                                                               
C  Change the CALIBRATION COEFFICIENTS                                          
C                                                                               
 400  CALL DISCAL(.FALSE.)                                                      
      GOTO 5                                                                    
C                                                                               
C  Display Current Setup                                                        
C                                                                               
 600  CALL system("clear")                                                      
      IMODE=1                                                                   
      WRITE(LUTO,234)                                                           
 234  FORMAT(' Select  1: Project 2: Flight 3: Miscellany ')                    
22234 READ(LUTI,'(I1)',END=9759,err=70017) IMODE              
      if (imode .eq. 0) goto 9759
      goto 19759
70017 WRITE(6,'('' Non-integer input; try again'')')                           
      GOTO 22234                                                               
 9759 continue                                                             
      GOTO 5                                                                    
19759 LUO=LUTO                                                                  
      CALL system("clear")                                                      
      IF (IMODE.EQ.2) GOTO 1651                                                 
      IF (IMODE.EQ.3) GOTO 2651                                                 
C  get units for all variables                                                  
C     CALL FNDUN                                                                
      WRITE(LUO,9761) NMUSER,                                                   
     1 NOTES,NMPROJ,IPROJ,IARCFT,TURBRT                                         
 9761 FORMAT(' CMS name ',A8,/,                                                 
     $A80,/,' Project ',A18,A4,2X,' Aircraft  ',A4,'  Rate: ',A3,/)             
      WRITE(LUO,9769)QCREF(1),PSX,TTX,DPX                                       
 9769 FORMAT(' TAS computed using MACH(',A8,1H,,A8,') and temperature'          
     1 ,1X,A8,/' Humidity computations use ',A8,/)                              
      WRITE(LUO,9768)AVANE,BVANE                                                
 9768 FORMAT(' Attack angle computed using ',A8,', Sideslip angle',1X,          
     1 'computed using ',A8,/)                                                  
      CALL SERCH('PLWC  ',SDINAM,NFULL,INDX)                                    
      IF(INDX.NE.0)WRITE(LUO,9180)FNUSS,REXP,WIRET                              
 9180  FORMAT(' PLWC: Nusselt = ',F10.4,'*Reynolds**',F10.4,/,                  
     1        '       Wire Temp=',F10.4)                                        
C                                                                               
C  List PMS1D calibration coefficients, if present                              
C                                                                               
      DO 651 J=1,5                                                              
       IF(NPROB(J).EQ.0)GO TO 651                                               
       IF(J.EQ.1) THEN                                                          
        WRITE(LUO,9651)DOF,BDIA,TAU1,TAU2,TAU3                                  
 9651   FORMAT(' FSSP: ',/,                                                     
     2'  DOF=',F6.2,'  BeamDiam=',F5.2,/,'  Tau1=',F9.7,'  Tau2=',F9.7,         
     3'  Tau3=',F6.4)                                                           
       ENDIF                                                                    
  651 CONTINUE                                                                  
      WRITE (6,'(/,'' Hit <r> for display of output variables'')')              
      READ(LUTI,'(A)',END=19899) I                                              
19899 continue
      CALL system("clear")                                                      
      WRITE(LUO,'(1x,I5,'' Variables selected: '',/)') PSPOUT           
      WRITE(LUO,9766)(VRPOSS(PSPPTR(J)),UNITS(PSPPTR(J)),                       
     1  J=1,PSPOUT)                                                             
 9766 FORMAT(5(1x,A8,1H(,A4,1H),1X))                                         
      WRITE (6,'(//,'' ... hit <r> to continue...'')')                          
      READ(LUTI,'(A)',END=29899) I                                              
29899 continue
      GO TO 600                                                                 
C                                                                               
C  display current flight setup                                                 
C                                                                               
 1651 CONTINUE                                                                  
      WRITE(LUO,19761) IFLGHT,DESCRP,IDATEF,ITIMEF,                             
     $ QCREF                                                                    
      WRITE(LUO,19772) (TAPNO(J),J=1,NUMVOL)                                    
      WRITE(LUO,9764)((ITMSEG(I,J),I=1,6),J=1,INMSEG)                           
      WRITE(LUO,'(/,3X,'' Input Snapshot length: '',I5,'' seconds'')')          
     $NSXIN                                                                     
C     CALL SETTIT                                                               
      WRITE(LUO,29761) PRTITL,IVTITL                                            
29761 FORMAT(/,' Project Title: ',A40,/,                                        
     $ ' Independent Variable Title: ',A32)                                     
      IF (FLTSAV) THEN                                                          
       WRITE(LUO,19762)                                                         
      ELSE                                                                      
       WRITE(LUO,19763)                                                         
      ENDIF                                                                     
19772 FORMAT('  Input Tape Volume(s): ',/,                                      
     $(1x,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X)) 
19761 FORMAT(/,' Flight  ',A5,/,                                                
     $' Flight Setup Descriptor: ',A50,/,                                       
     $' Date of Flight  ',3(I2,1X),'  Time of Flight  ',I2,':',I2,':',I2        
     $,' to ',I2,2(':',I2),                                                     
     $/,' Dynamic Pressure Reference Sensors are ',A3,' and ',A3,/)             
 9764 FORMAT(3X,' Time Segments: Start Time   End Time',/,                      
     $(20X,I2,1H:,I2,1H:,I2,3X,I2,1H:,I2,1H:,I2))                               
19762 FORMAT(/,' This setup HAS been saved on disk')                     
19763 FORMAT(/,' This setup has NOT been saved on disk')        
      WRITE (6,'('' Hit <r> to continue...'')')                                 
      READ(LUTI,'(A)',END=29799) I                                              
29799 continue
      GOTO 600                                                                  
C                                                                               
C  display current GENPRO setup                                                 
C                                                                               
 2651 CONTINUE                                                                  
C List enabled and disabled Genpro operations                                   
      ONOPS=' '                                                                 
      OFFOPS=' '                                                                
      ISTON=-7                                                                  
      ISTOF=-7                                                                  
      DO 28888 J=1,NUMCYC                                                       
       IF (CYCLES(J)) THEN                                                      
        ISTON=ISTON+8                                                           
        ONOPS(ISTON:ISTON+7)=NAMCYC(J)                                          
       ELSE                                                                     
        ISTOF=ISTOF+8                                                           
        OFFOPS(ISTOF:ISTOF+7)=NAMCYC(J)                                         
       ENDIF                                                                    
28888 CONTINUE                                                                  
      WRITE(LUO,28766) ONOPS,OFFOPS                                             
28766 FORMAT(' Enabled Operations:  ',A40/' Disabled Operations: ',A40)         
C  repeat for disposes                                                          
      ONDIS=' '                                                                 
      OFFDIS=' '                                                                
      ISTON=-24                                                                 
      ISTOF=-24                                                                 
      DO 38888 J=1,NUMDIS                                                       
       IF (DISPOS(J)) THEN                                                      
        ISTON=ISTON+25                                                          
        ONDIS(ISTON:ISTON+24)=NAMDIS(J)                                         
       ELSE                                                                     
        ISTOF=ISTOF+25                                                          
        OFFDIS(ISTOF:ISTOF+24)=NAMDIS(J)                                        
       ENDIF                                                                    
38888 CONTINUE                                                                  
      WRITE(LUO,38766) ONDIS(1:25)                                              
38766 FORMAT(' Enabled Output Dispositions:  ',A25)                             
      DO 48766 J=2,INT(ISTON/25)+1                                              
       WRITE(LUO,38776) ONDIS((J-1)*25+1:(J-1)*25+25)                           
38776  FORMAT(20X,A25)                                                          
48766 CONTINUE                                                                  
      WRITE(LUO,38767) OFFDIS(1:25)                                             
38767 FORMAT(' Disabled Output Dispositions: ',A25)                             
      DO 58766 J=2,INT(ISTOF/25)+1                                              
       WRITE(LUO,38776) OFFDIS((J-1)*25+1:(J-1)*25+25)                          
58766 CONTINUE                                                                  
      IF (FLTSAV) THEN                                                          
       WRITE(LUO,19762)                                                         
      ELSE                                                                      
       WRITE(LUO,19763)                                                         
      ENDIF                                                                     
      WRITE (6,'(/'' Hit <r> to continue...'')')                                
      READ(LUTI,'(A)',END=39899) I                                              
39899 continue
      GOTO 600                                                                  
C  Modify Output order                                                          
 3651 CALL system("clear")                                                      
      WRITE(LUTO,'('' You may rearrange display order for either the ''/        
     $'' general output operations (plot, print, stats) or for the''/      
     $'' complete flight plot. '')')                
      WRITE(LUTO,'(/'' Choose: (1) General (2) Complete Flight''/)')            
76543 READ(LUTI,'(I1)',END=5,err=70018) IC                
      if (ic .eq. 0) goto 5
      goto 70019
70018  WRITE(6,'('' Non-integer input; try again'')')                           
       GOTO 76543                                                               
70019 IF (IC.EQ.1) THEN                                                         
       CALL CHGORD(PSPPTR,PSPOUT)                                               
      ELSE                                                                      
       CALL CHGORD(PL2PTR,PL2OUT)                                               
      ENDIF                                                                     
      GOTO 3651                                                                 
C                                                                               
C  Switch MENU Listing on or off                                                
C                                                                               
  898 MENUPR= -1 * MENUPR                                                       
      GO TO 5                                                                   
C                                                                               
C enable processing                                                             
C                                                                               
 800  CONTINUE                                                                  
      CALL system("clear")                                                      
      WRITE(LUTO,'('' Enable (1) Project (2) Flight (3) Both  '',               
     $ ''     <r>=Neither'')')                                           
      READ(LUTI,'(I1)',END=34567) IMODE                                         
      if (imode .eq. 0) goto 5
      GOTO 34568                                                                
34567 continue
      IMODE = 3                                                                 
34568 CONTINUE                                                                  
C   Enable either setup: QC's get original values, or updated values            
      QCREF(1)=PRQC(1)                                                          
      QCREF(2)=PRQC(2)                                                          
      IF (IMODE.EQ.1.OR.IMODE.EQ.3) THEN                                        
C save info need for project deck generation                                    
       WRITE(LUTO,'(//'' Processing Project Deck ...'')')                       
       EXEC=.TRUE.                                                              
       CALL DIRECT                                                              
      ENDIF                                                                     
      IF (IMODE.EQ.2.OR.IMODE.EQ.3) THEN                                        
       CALL FLTGEN                                                              
      ENDIF                                                                     
      GOTO 5                                                                    
C                                                                               
C Disable processing                                                            
C                                                                               
  850 CONTINUE                                                                  
      CALL system("clear")                                                      
      WRITE(LUTO,'('' Disable (1) Project (2) Flight (3) Both  '',              
     $ ''     <r>=Both'')')                                                     
      READ(LUTI,'(I1)',END=44567) IMODE                                         
      if (imode .eq. 0) goto 44567
      GOTO 44568                                                                
44567 continue
      IMODE = 3                                                                 
44568 CONTINUE                                                                  
      IF (IMODE.EQ.1.OR.IMODE.EQ.3) THEN                                        
       EXEC=.FALSE.                                                             
       CALL DIRECT                                                              
      ENDIF                                                                     
      IF (IMODE.EQ.2.OR.IMODE.EQ.3) THEN                                        
       REWIND(46)                                                               
       WRITE(46,'('' DISABLE'')')                                          
      ENDIF                                                                     
      GOTO 5                                                                    
C                                                                               
C  Save Current Setup                                                           
C                                                                               
 700  CONTINUE                                                                  
      IF (IEND.EQ.0) WRITE(LUTO,1717)                                           
      IF (IEND.EQ.1) WRITE(LUTO,1817)                                           
 1717 FORMAT(' Select save (1) project setup (2) flight setup ',/,              
     $  '            (3) both  (4) neither  <r> = Parameters Menu ')            
 1817 FORMAT(' Select save (1) project setup (2) flight setup ',/,              
     $  '            (3) both  (4) neither  <r> = exit to Setup Menu ')         
21817 READ(LUTI,'(Bz,I2)',END=1718,err=70020) ISAV    
      if (isav .eq. 0) goto 1718
      goto 1719
70020  WRITE(6,'('' Non-integer input; try again'')')                           
       GOTO 21817                                                               
C 1718 rewind (luto)	
 1718 continue                                                              
      IF (IEND.EQ.0) THEN                                                       
       GOTO 5                                                                   
      ELSE                                                                      
       RETURN                                                                   
      ENDIF                                                                     
 1719 IF (ISAV.EQ.2) GOTO 900                                                   
      IF (ISAV.EQ.4) THEN                                                       
       IF (IEND.EQ.0) GOTO 5                                                    
       RETURN                                                                   
      ENDIF                                                                     
C   Write out project setup: saved project QC's get current flight values       
      PRQC(1)=QCREF(1)                                                          
      PRQC(2)=QCREF(2)                                                          
      CALL FILE(3)                                                              
      IF (ISAV.EQ.1) THEN                                                       
       IF (IEND.EQ.0) GOTO 5                                                    
       RETURN                                                                   
      ENDIF                                                                     
C                                                                               
C  Save flight setup;                                                           
C                                                                               
  900 CONTINUE                                                                  
C ensure that a flight setup has been done at all                               
      IF (.NOT.(FLTSET)) THEN                                                   
       IMODE=3                                                                  
       WRITE(LUTO,1999)                                                         
 1999  FORMAT(' First you must either initialize or restore a flight se'        
     $ ,'tup, or you may',/                                                     
     $  ' simply return to Parameters Menu now. ',/                             
     $ ,' Select 1: initialize 2: restore 3: return to Parameters menu')        
21999  READ(LUTI,'(I1)',END=20000,err=70021) IMODE   
       goto 20000
70021   WRITE(6,'('' Non-integer input; try again'')')                          
        GOTO 21999                                                              
20000  continue
       IF (IMODE.EQ.3) GOTO 5                                                   
       IF (IMODE.EQ.2) THEN                                                     
        CALL RESFLT                                                             
       ELSE                                                                     
        CALL INIFLT                                                             
       ENDIF                                                                    
      ENDIF                                                                     
C Save the flight setup currently in effect                                     
C     OPEN(20,FILE='FLTSAV',IOSTAT=IER, STATUS='UNKNOWN',                       
C    $FORM='UNFORMATTED',ERR=91865)                                             
C-------------open 20
      do 31, i = 1, 80
         fulpth(i:i) = ' '
 31   continue
      lindex = index(arg2,' ') - 1
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/fltsav.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=20,file=fulpth,access='sequential',
     $err=91865,iostat=ier,form='unformatted')
      REWIND(20)                                                                
91865 IF(IER .NE.0) THEN                                                        
       WRITE(LUTO,9656)IER                                                      
 9656  FORMAT(' OPEN error',I5,' on FLTSAV; contact expert')                    
       IER =0                                                                   
       WRITE (6,'('' Hit <r> to continue'')')                                   
       READ(LUTI,'(A)',END=49899) I                                             
49899  continue 
       GOTO 5                                                                   
      ENDIF                                                                     
C  I/O with IBM FORTRAN is a little clumsy...to get to the end of a file        
C  (that may be empty) and extend it, here's the procedure:                     
C                                                                               
C   get to record with this DESCRPtor or to EOF                                 
  101 READ(20,END=6969) NEXDES                                                  
      IF (NEXDES.EQ.DESCRP) THEN                                                
C  user has option of overwriting this record with new setup, or of             
C  designating a new DESCRP for this setup.                                     
       WRITE(LUTO,7676) DESCRP                                                  
 7676  FORMAT(' You have a restorable Flight Setup with description:',/,        
     $ 1x,A50,/,' enter new description to make this one restorable ',         
     $ ' also; or if this '/' one has not changed, enter <r>:')                 
       READ(LUTI,'(A50)',END=76771) DESCRP                                      
       if (descrp .eq. "") goto 76771
       GOTO 101                                                                 
76771  continue
       GOTO 7970                                                                
      ENDIF                                                                     
      GOTO 101                                                                  
C  back up to just before EOF                                                   
 6969 BACKSPACE(20)                                                             
C  if there is a record there, read it and land just before EOF; else           
C  EOF = BOF and the EOF will be read again; if so, backspace again to          
C  get just before the EOF                                                      
      READ(20,END=6970)                                                         
      GOTO 6971                                                                 
C back up one record to prepare for write in correct spot                       
 6970 BACKSPACE(20)                                                             
 6971 WRITE(20,IOSTAT=IOS )DESCRP,IFLGHT,C1,C2,C3,SDINAM,NFULL,                 
     $ ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                               
     $ NUMVOL,TAPNO,TAPE1,OUTPUT,GAP,IDATEF,ITIMEF,QCREF,USRTIT,                
     $ NSXOUT,NSXIN,IVTITL                                                      
      CLOSE(20,IOSTAT=IOS )                                                     
 7970 FLTSAV=.TRUE.                                                             
      IF (IEND.EQ.1) RETURN                                                     
      GOTO 5                                                                    
C                                                                               
C Change miscellaneous default values                                           
C                                                                               
 1000 CALL system("clear")                                                   
      WRITE(LUTO,1901)                                                          
 1901 FORMAT(                                                                   
     1       ' CO  Change Annotation Comment',/                                 
     2       ' TA  Change TAS Calculation Inputs',/                             
     2       ' MA  Change MACH Number Calculation Inputs',/                     
     2       ' DP  Change dew point sensor for humidity calculation',/          
     2       ' LW  Change calibration for PMS LWC probe',/                      
     3       ' AS  Change SENSORS used for Winds',/                             
     4       ' UN  Change UNITS for a Parameter',/)                             
      READ(LUTI,'(A2)',END=679)NINPUT                                           
      if (ninput .eq. "") goto 679
      CALL system("clear")                                                      
      GOTO 680                                                                  
  679 continue                                                             
      GOTO 5                                                                    
  680 IF(INDEX(NINPUT,' ').EQ.1) GO TO 1000                                     
      IARG=2                                                                    
      CALL caps(NINPUT,IARG)                                                   
      IF(NINPUT.EQ.'CO') GO TO 1020                                             
      IF(NINPUT.EQ.'TA') GO TO 1030                                             
      IF(NINPUT.EQ.'MA') GO TO 1040                                             
      IF(NINPUT.EQ.'AS') GO TO 1050                                             
      IF(NINPUT.EQ.'UN') GO TO 1060                                             
      IF(NINPUT.EQ.'DP') GO TO 1070                                             
      IF(NINPUT.EQ.'LW') GO TO 1080                                             
      CALL PAUSER(' Unavailable option ... hit <r> to continue')                
      GO TO 1000                                                                
C  change comment                                                               
 1020 WRITE(LUTO,9704) NOTES                                                    
 9704 FORMAT(1x,A80,/,' Enter up to 65 characters of annotation data ')  
      READ(LUTI,9802,END=681) NOTES(16:80)                                      
 9802 FORMAT(A65)                                                               
  681 continue
  682 GO TO 1000                                                                
C                                                                               
C  TAS CALCULATION INPUTS                                                       
1030  CONTINUE                                                                  
      WRITE(LUTO,9030)QCREF(1),PSX,TTX                                          
 9030 FORMAT(' TAS computed using MACH(',A8,1H,,A8,') and temperature ',        
     1  A8,/,' Please enter new temperature to use return=no change _')         
 1031 READ(LUTI,'(A8)',END=683)FILNAM                                           
      if (filnam .eq. "") goto 683
      GOTO 684                                                                  
  683 continue
      GOTO 1000                                                                 
  684 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1000                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1037                                                  
      WRITE(LUTO,9031) FILNAM                                                   
 9031 FORMAT(' Name ',A8,' not found.   Please reenter _')                      
      GO TO 1031                                                                
 1037 TTX=FILNAM                                                                
      GO TO 1000                                                                
1040  CONTINUE                                                                  
      WRITE(LUTO,9040)QCREF(1),PSX                                              
 9040 FORMAT(' MACH Number computed using ',A8,' and ',A8,/,                    
     1  ' Please enter new QC to be used return=no change _')                   
 1041 READ(LUTI,9801,END=686)FILNAM                                             
      if (filnam .eq. "") goto 686
 9801 FORMAT(A8)                                                                
      GOTO 687                                                                  
  686 continue
      GOTO 1042                                                                 
  687 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1042                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1044                                                  
      WRITE(LUTO,9031) FILNAM                                                   
      GO TO 1041                                                                
 1044 QCREF(1)=FILNAM                                                           
 1042 WRITE(LUTO,9041)                                                          
 9041 FORMAT(' Please enter new static pressure to be used',1X,                 
     1' return=no change _')                                                    
 1045 READ(LUTI,9801,END=688)FILNAM                                             
      if (filnam .eq. "") goto 688
      GOTO 689                                                                  
  688 continue
      GOTO 1000                                                                 
  689 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1000                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1047                                                  
      WRITE(LUTO,9031) FILNAM                                                   
      GO TO 1045                                                                
 1047 PSX=FILNAM                                                                
      GO TO 1000                                                                
C  CHANGE THE INPUTS TO ATTACK AND SIDESLIP                                     
1050  CONTINUE                                                                  
      WRITE(LUTO,9768)AVANE,BVANE                                               
      WRITE(LUTO,9050)                                                          
 9050 FORMAT(' Please enter new ATTACK Sensor to be used',1X,                   
     1' return=no change _')                                                    
 1051 READ(LUTI,9801,END=690)FILNAM                                             
      if (filnam .eq. "") goto 690
      GOTO 691                                                                  
  690 continue
      GOTO 1052                                                                 
  691 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1052                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1054                                                  
      WRITE(LUTO,9031) FILNAM                                                   
      GO TO 1051                                                                
 1054 AVANE=FILNAM                                                              
 1052 WRITE(LUTO,9051)                                                          
 9051 FORMAT(' Please enter new SIDESLIP Sensor to be used',1X,                 
     1' return=no change _')                                                    
 1055 READ(LUTI,9801,END=692)FILNAM                                             
      if (filnam .eq. "") goto 692
      GOTO 693                                                                  
  692 continue
      GOTO 1000                                                                 
  693 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1000                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1057                                                  
      WRITE(LUTO,9031) FILNAM                                                   
      GO TO 1055                                                                
 1057 BVANE=FILNAM                                                              
      GO TO 1000                                                                
C  Allow user to change units                                                   
1060  CONTINUE                                                                  
      WRITE(LUTO,9161)                                                          
 9161 FORMAT(' Enter name of variable _')                                       
      READ(LUTI,9801,END=694)NAMIN                                              
      if (namin .eq. "") goto 694
      GOTO 695                                                                  
  694 continue
      GOTO 1000                                                                 
  695 IF(INDEX(NAMIN,' ').EQ.1)GO TO 1000                                       
      CALL caps(NAMIN,8)                                                       
      CALL SERCH (NAMIN,VRPOSS,NMPOSS,INDX)                                     
      IF(INDX.NE.0)GO TO 1062                                                   
      CALL PAUSER(' Name not found...hit <r> to continue')                      
      GO TO 1060                                                                
 1062 WRITE(LUTO,9163)NAMIN,UNITS(INDX)                                         
 9163 FORMAT(2X,A8,1H(,A4,1H),/,'  Please enter new units <r>: ',1X,            
     1 ' no change  _')                                               
      READ(LUTI,9164,END=696) VINPUT                                            
      if (vinput .eq. "") goto 696
      GOTO 697                                                                  
  696 continue
      GOTO 1060                                                                 
 9164 FORMAT(A4)                                                                
  697 IF(INDEX(VINPUT,' ').EQ.1)GO TO 1060                                      
      UNITS(INDX)=VINPUT                                                        
      CALL SERCH(NAMIN,VNAME,NUMBV,INDX)                                        
      VUNIT(INDX)=VINPUT                                                        
      GO TO 1060                                                                
C  CHANGE DEWPOINTER USED FOR HUMIDITY CALCULATIONS                             
1070  WRITE(LUTO,9071)DPX                                                       
 9071 FORMAT(' Please enter dew pointer to be used return=',A8,' _')            
 1075 READ(LUTI,9801,END=698)FILNAM                                             
      if (filnam .eq. "") goto 698
      GOTO 699                                                                  
  698 continue
      GOTO 1000                                                                 
  699 IF(INDEX(FILNAM,' ').EQ.1) GO TO 1000                                     
      IARG=8                                                                    
      CALL caps(FILNAM,IARG)                                                   
      CALL SERCH2 (FILNAM,SDINAM,MODE2,NMODE2,INDX)                             
      IF (INDX.EQ.0) CALL SERCH2 (FILNAM,DERIV,MODE3,NMODE3,INDX)               
      IF(INDX.NE.0) GO TO 1077                                                  
      WRITE(LUTO,9031)FILNAM                                                    
      GO TO 1075                                                                
 1077 DPX=FILNAM                                                                
      GO TO 1000                                                                
C  CHANGE THE PMS LIQUID WATER PROBE CALIBRATION                                
 1080 WRITE(LUTO,9080)                                                          
 9080 FORMAT(' To change PMS LWC values, please enter new value',1X,            
     1       ' return=no change')      
      WRITE(LUTO,9081) FNUSS                                                    
 9081 FORMAT(' Nusselt = Reynolds * ',F10.4,'] _')                              
29081 READ(LUTI,'(A)',END=7730,err=70023)STRING    
      if (string .eq. "") goto 7730
      IF(INDEX(STRING,' ').GT.1)READ(STRING,'(F10.4)')FNUSS                     
      goto 7730
70023  WRITE(6,'('' Non-numeric input; try again'')')                           
       GOTO 29081                                                               
 7730 continue
      WRITE(LUTO,9083)FNUSS,REXP                                                
 9083 FORMAT(' Nusselt = ',F10.4,'*Reynolds**',F10.4,'] _')                     
29083 READ(LUTI,'(A)',END=731,err=70024)STRING                     
      if (string .eq. "") goto 731
      IF(INDEX(STRING,' ').GT.1)READ(STRING,'(F10.4)')REXP                      
      goto 731
70024  WRITE(6,'('' Non-numeric input; try again'')')                           
       GOTO 29083                                                               
  731 continue
      WRITE(LUTO,9082) WIRET                                                    
 9082 FORMAT(' Wire Temperature =',F10.4,'] _')                                 
29082 READ(LUTI,'(A)',END=732,err=70025)STRING                      
      if (string .eq. "") goto 732
      IF(INDEX(STRING,' ').GT.1)READ(STRING,'(F10.4)')WIRET                     
      goto 732
70025  WRITE(6,'('' Non-numeric input; try again'')')                           
       GOTO 29082                                                               
  732 continue
      GO TO 1000                                                                
C                                                                               
C                                                                               
C  Change project/flight/genpro setups                                          
C                                                                               
 1100 continue
      CALL system("clear")                                                      
      WRITE(LUTO,1110)                                                          
 1110 FORMAT(' Select: (1) Project (2) Flight (3)',
     $' Miscellany <r>=return')       
21110 READ(LUTI,'(I1)',END=1111,err=70026) IMODE        
      if (imode .eq. 0) goto 1111
      goto 1112
70026  WRITE(6,'('' Non-integer input; try again'')')                           
       GOTO 21110                                                               
 1111 continue
      GOTO 5                                                                    
 1112 continue
      CALL system("clear")                                                      
      WRITE(LUTO,1113)                                                          
 1113 FORMAT(' Select item to modify (<r> to exit): ')                          
      IF (IMODE.EQ.1) THEN                                                      
C   Initialized Project parameters                                              
C                                                                               
 1099  continue
       CALL system("clear")                                                    
       WRITE(LUTO,10991) NMUSER,NMSCI,INILAT,INILON,TURBRT,NOTES                
10991  FORMAT(                                                                  
     $ '    (1) User Name  ',A8,/                                               
     $ '    (2) Scientist Number  ',A4,/                                        
     $ '    (3) Initial Latitude ',F9.4,/                                       
     $ '    (4) Initial Longitude ',F9.4,/                                      
     $ '    (5) Turbulence Rate ',A3,/                                          
     $ '    (6) Comment: ',/,A80)                                               
87654  READ(LUTI,'(I1)',END=10199,err=70027) ITEM          
       if (item .eq. 0) goto 10199
       GOTO (10200,10205,10230,10235,10240,10245) ITEM                          
70027   WRITE(6,'('' Non-integer input; try again'')')                          
        GOTO 87654                                                              
10199  continue
       GOTO 1100                                                                
10200  WRITE(LUTO,10201)                                                        
10201  FORMAT(' Name to whose reader output will be sent: ')     
       READ(LUTI,'(A8)',END=1099) NMUSER                                        
       GOTO 1099                                                                
10205  WRITE(LUTO,10206)                                                        
10206  FORMAT(' 4-digit scientist number for accounting: ')                     
       READ(LUTI,'(A4)',END=1099) NMSCI                                         
       if (nmsci .eq. "") goto 1099
       NMACCT(1:4)=NMSCI                                                        
       GOTO 1099                                                                
10230  WRITE(LUTO,10231)                                                        
10231  FORMAT(' Change Initial Latitude: ')                                     
20231  READ(LUTI,'(F9.4)',END=1099,err=70028) INILAT    
       goto 1099
70028   WRITE(6,'('' Non-numeric input; try again'')')                          
        GOTO 20231                                                              
10235  WRITE(LUTO,10236)                                                        
10236  FORMAT(' Change Initial Longitude:')                                     
20236  READ(LUTI,'(F9.4)',END=1099,err=70029) INILON    
       goto 1099
70029   WRITE(6,'('' Non-numeric input; try again'')')                          
        GOTO 20236                                                              
10240  CONTINUE                                                                 
       IF (TURBRT.EQ.'HRT') THEN                                                
        TURBRT = 'LRT'                                                          
        WRITE(LUTO,'(/'' Please verify that turbulence rate in SUMMARY''        
     $  ,'' and COMPFLT files is LRT.''/)')                                     
       ELSE                                                                     
        TURBRT = 'HRT'                                                          
        WRITE(LUTO,'(/'' Please verify that turbulence rate in SUMMARY''        
     $  ,'' and COMPFLT files is HRT.''/)')                                     
       ENDIF                                                                    
       CALL PAUSER(' Hit <r> to continue ... ')                                 
       GOTO 1099                                                                
10245  WRITE(LUTO,10246)                                                        
10246  FORMAT (' Change comment (65 chars max): ')                              
       READ(LUTI,'(A65)',END=1099) NOTES(16:80)                                 
       GOTO 1099                                                                
      ELSEIF (IMODE.EQ.2) THEN                                                  
C determine value of OUTLEN based on comparing total flight length with         
C current value of output tape length in seconds                                
        DO 244 N=1,3                                                            
  244    FTIME(N)=ITMSEG(N,1)                                                   
        DO 245 N=4,6                                                            
  245    FTIME(N)=ITMSEG(N,INMSEG)                                              
       CALL HMS2SX(FTIME(1),NSEX1)                                              
       CALL HMS2SX(FTIME(4),NSEX2)                                              
       NTOTAL = NSEX2 - NSEX1 + 1                                               
       IF (NTOTAL.LE.NSXOUT) THEN                                               
        OUTLEN='FULL'                                                           
       ELSE                                                                     
        OUTLEN='PARTIAL'                                                        
       ENDIF                                                                    
       WRITE(LUTO,1114) DESCRP,IFLGHT,(TAPNO(J),J=1,NUMVOL)                     
 1114  FORMAT(                                                                  
     $ '    (1)   Flight Descriptor ',A50,/                                     
     $ '    (2)   Flight Number ',A5,/                                          
     $ '    (3)   Input Volume Numbers',/,                                      
     $ (1x,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X,A6,2X))         
       WRITE(LUTO,21114)IDATEF,ITIMEF,((ITMSEG(I,J),I=1,6),J=1,INMSEG)          
21114  FORMAT(                                                                  
     $ '    (4)   Flight Date ',3(I2,1X),/                                      
     $ '    (5)   Flight Time ',3(I2,1X),' to ',3(I2,1X),/                      
     $ '    (6)   Input Snapshots: ',/,                                         
     $(15X,I2,1H:,I2,1H:,I2,3X,I2,1H:,I2,1H:,I2))                               
       WRITE(LUTO,21115) NSXIN,OUTLEN,IVTITL,PRTITL,QCREF                       
21115  FORMAT(                                                                  
     $ '    (7)   Input Snapshot length (seconds) ',I5,/                        
     $ '    (8)   Output Snapshot length: ',A7,/,                               
     $ '    (9)   Ivtitle:  ',A32,/                                             
     $ '    (10)  Project Title ',A40,/                                         
     $ '    (11)  QC primary reference ',A3,/                                   
     $ '    (12)  QC secondary reference ',A3,/)                                
98765  READ(LUTI,'(Bz,I2)',END=1100,err=70030) IMODE      
       if (imode .eq. 0) goto 1100
       goto 70031
70030   WRITE(6,'('' Non-integer input; try again'')')                          
        GOTO 98765                                                              
70031  if (IMode .ne. 2) wRITE(LUTO,1115) IMODE                
 1115  FORMAT(' Enter Value for item ',I2,' or <r> for Parameters menu')        
       IF(IMODE.EQ.1) THEN                                                      
        IF(GAP.NE.'0') THEN                                                     
         WRITE(LUTO,'('' read only information -- hit <r> ...'')')              
         READ(LUTI,'(A)',END=11120) I                                           
         goto 11120
        ELSE                                                                    
         READ(LUTI,'(A50)',END=11120) DESCRP                                    
         if (descrp .eq. "") goto 11120
        ENDIF                                                                   
       ELSEIF (IMODE.EQ.2) THEN                                                 
        CALL system("clear")                                                    
        WRITE(LUTO,'('' Previous Setup restores calib. coefficients,'',         
     $'' time segments, '',/,'' QC sensors, and sampled vars as speci'',        
     $''fied therein;'',//,'' Log Entry chooses CURRENT calib. coeff'',         
     $''icients, '',/,'' QC sensors, and sampled vars, w/entire '',      
     $'' flight time in 30-min. intervals '',//,                                
     $'' Your choice:        (1) Previous Setup '',/,                           
     $''                     (2) Log Entry'',/,                                 
     $''                     <r> = back to menu'')')                            
66554   READ(LUTI,'(I1)',END=11120,err=70032) IPICK   
        if (ipick .eq. 0) goto 11120
        goto 70033
70032    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 66554                                                             
70033   IF (IPICK.EQ.1) THEN                                                    
         CALL RESFLT                                                            
        ELSE                                                                    
C save current flight number since NEWFLT changes IFLGHT                        
         IFLT=IFLGHT                                                            
         FOUND=.FALSE.                                                          
         CALL NEWFLT(FOUND)                                                     
         IF (.NOT.(FOUND)) THEN                                                 
          IFLGHT=IFLT                                                           
         ELSE                                                                   
C reset flight/project title/ indicate that a flight setup is done              
          CALL SETTIT                                                           
          FLTSET=.TRUE.                                                         
         ENDIF                                                                  
        ENDIF                                                                   
       ELSEIF (IMODE.EQ.3) THEN                                                 
        WRITE(LUTO,'('' read only information -- hit <r> ...'')')               
        READ(LUTI,'(A)',END=11120) I                                            
        goto 11120
       ELSEIF (IMODE.EQ.4) THEN                                                 
        WRITE(LUTO,'('' read only information -- hit <r> ...'')')               
        READ(LUTI,'(A)',END=11120) I                                            
        goto 11120
       ELSEIF (IMODE.EQ.5) THEN                                                 
        WRITE(LUTO,'('' read only information -- hit <r> ...'')')               
        READ(LUTI,'(A)',END=11120) I                                            
        goto 11120
       ELSEIF (IMODE.EQ.6) THEN                                                 
        CALL TMSEG(BTIM,ETIM,ISTRNG)                                            
       ELSEIF (IMODE.EQ.7) THEN                                                 
21120   READ(LUTI,'(Bz,I5)',END=11120,err=70034) NSXIN               
        if (nsxin .eq. 0) goto 11120
        goto 70035
70034    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 21120                                                             
70035   DO 144 N=1,3                                                            
  144    FTIME(N)=ITMSEG(N,1)                                                   
        DO 145 N=4,6                                                            
  145    FTIME(N)=ITMSEG(N,INMSEG)                                              
        CALL GETSNP(FTIME,NSXIN,.TRUE.,ITMSEG,ITMFLG,INMSEG)                    
C new value ==> this setup not saved on disk anymore                            
        FLTSAV=.FALSE.                                                          
       ELSEIF (IMODE.EQ.8) THEN                                                 
C move this to Project Changes !!!!!                                            
C       READ(LUTI,'(BN,I5)',END=11120) NSXOUT                                   
C       CALL GETSNP(ITIMEF,NSXOUT,.FALSE.,OTMSEG,OTMFLG,ONMSEG)                 
        WRITE(LUTO,'('' read only information -- hit <r> ...'')')               
        READ(LUTI,'(A)',END=11120) I                                            
        goto 11120
       ELSEIF(IMODE.EQ.9) THEN                                                  
         READ(LUTI,'(A32)',END=11120) IVTITL                                    
        if (ivtitl .eq. "") goto 11120
C new value ==> this setup not saved on disk anymore                            
         FLTSAV=.FALSE.                                                         
       ELSEIF (IMODE.EQ.10) THEN                                                
C  Project Title                                                                
        READ(LUTI,'(A)',END=12113) USRTIT                                       
        if (USRTIT .eq. "") goto 12113
        CALL SETTIT                                                             
12113   continue
       ELSEIF (IMODE.EQ.11) THEN                                                
        READ(LUTI,'(A3)',END=11120) QCREF(1)                                    
        if (qcref(1) .eq. "") goto 11120
C new value ==> this setup not saved on disk anymore                            
         FLTSAV=.FALSE.                                                         
       ELSEIF (IMODE.EQ.12) THEN                                                
        READ(LUTI,'(A3)',END=11120) QCREF(2)                                    
        if (qcref(2) .eq. "") goto 11120
C new value ==> this setup not saved on disk anymore                            
         FLTSAV=.FALSE.                                                         
       ELSE                                                                     
C Illegal entry                                                                 
        WRITE (6,'('' Illegal entry. <r> to continue'')')                       
        READ(LUTI,'(A)',END=59899) I                                            
59899   continue
        GOTO 11120                                                              
       ENDIF                                                                    
11120  continue
       IMODE=2                                                                  
       GOTO 1112                                                                
      ELSEIF (IMODE.EQ.3) THEN                                                  
C  Miscellany setup                                                             
 2113  continue
       CALL system("clear")                                                     
       WRITE(LUTO,2114)                                                         
 2114  FORMAT( ' Select menu (<r> to exit): ',                                  
     $/'    (1) Genpro Operations'                                              
     $/'    (2) Output Dispositions'                                            
     $/'    (3) Special Plots'                                                  
     $)                                                                         
22115  READ(LUTI,'(I1)',END=1100,err=70036) IMODE                    
       if (imode .eq. 0) goto 1100
       goto 2115
70036   WRITE(6,'('' Non-integer input; try again'')')                          
        GOTO 22115                                                              
 2115  continue
       CALL system("clear")                                                     
       IF(IMODE.EQ.1) THEN                                                      
C  set indicator strings for each operation                                     
        DO 3338 J=1,NUMCYC                                                      
         IF (CYCLES(J)) THEN                                                    
          OPLIST(J)='ON'                                                        
         ELSE                                                                   
          OPLIST(J)='OFF'                                                       
         ENDIF                                                                  
 3338   CONTINUE                                                                
C  Operations setup                                                             
        WRITE(LUTO,3114) OPLIST                                                 
 3114   FORMAT( ' Select # of Genpro Operation to toggle: '/                    
     $ '    (1) Output ',A3,/                                                   
     $ '    (2) Print ',A3,/                                                    
     $ '    (3) Plot ',A3,/                                                     
     $ '    (4) Plot2 ',A3,/                                                    
     $ '    (5) Statistics ',A3,/                                               
     $ '    <r> = Parameters Menu'/)                                            
23114   READ(LUTI,'(I1)',END=2113,err=70037) IMODE         
        if (imode .eq. 0) goto 2113
        goto 70038
70037    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 23114                                                             
70038   IF (IMODE.GE.1.AND.IMODE.LE.NUMCYC) THEN                                
         CYCLES(IMODE)=.NOT.(CYCLES(IMODE))                                     
        ELSE                                                                    
         WRITE (6,'('' Illegal entry. <r> to continue'')')                      
         READ(LUTI,'(A)',END=69899) I                                           
69899    continue
         IMODE=1                                                                
         GOTO 2115                                                              
        ENDIF                                                                   
        IMODE=1                                                                 
        GOTO 2115                                                               
       ELSEIF(IMODE.EQ.2) THEN                                                  
C  set indicator strings for each dispose                                       
        DO 4338 J=1,NUMDIS                                                      
         IF (DISPOS(J)) THEN                                                    
          DILIST(J)='ON'                                                        
         ELSE                                                                   
          DILIST(J)='OFF'                                                       
         ENDIF                                                                  
 4338   CONTINUE                                                                
C  Dispose setup                                                                
        WRITE(LUTO,5114) DILIST                                                 
 5114   FORMAT( ' Select # of Output device to toggle: '/                       
     $ '    (1) Print to Microfilm ',A3,/                                       
     $ '    (2) Print to Mass Store ',A3,/                                      
     $ '    (3) Print to IBM reader ',A3,/                                      
     $ '    (4) Plot to IBM reader ',A3,/                                       
     $ '    (5) Plot to Microfilm ',A3,/                                        
     $ '    (6) Plot to Mass Store ',A3,/                                       
     $ '    (7) Plot to Laser Print ',A3,/                                      
     $ '    (8) Output to Mass Store ',A3,/                                     
     $ '    <r> = Parameters Menu'/)                                            
25114   READ(LUTI,'(I1)',END=2113,err=70039) IMODE         
        if (imode .eq. 0) goto 2113
        goto 70040
70039    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 25114                                                             
70040   IF (IMODE.GE.1.AND.IMODE.LE.NUMDIS) THEN                                
         DISPOS(IMODE)=.NOT.(DISPOS(IMODE))                                     
        ELSE                                                                    
         WRITE (6,'('' Illegal entry. <r> to continue'')')                      
         READ(LUTI,'(A)',END=79899) I                                           
79899    continue
         IMODE=2                                                                
         GOTO 2115                                                              
        ENDIF                                                                   
        IMODE=2                                                                 
        GOTO 2115                                                               
       ELSEIF (IMODE.EQ.3) THEN                                                 
C Select special plots                                                          
        CALL EXTEND(49)                                                         
        CALL EXTEND(50)                                                         
C set defaults                                                                  
        LUPLT=49                                                                
 7112   continue
        YVAR1='NONE'                                                            
71121   XAXIS=.FALSE.                                                           
        MLTIPL=.FALSE.                                                          
        XVAR1='Time'                                                            
        YVAR2='NONE'                                                            
C plot operations menu                                                          
 7113   CALL system("clear")                                                    
        WRITE(LUTO,7114)                                                        
 7114   FORMAT( ' Select Plot Operation: '/                                     
     $ '    (1) Select 1st or 2nd Plot operation (default 1st)',/               
     $ '    (2) Select Dependent Variable (Y-axis)',/                           
     $ '    (3) Set X-axis Variable  ',/                                        
     $ '    (4) Set Multiple Curve (2nd Y-axis variable) ',/                    
     $ '    (5) Select Further Plot Options ',/                                 
     $ '    <r> = Genpro Menu'/)                                                
27114   READ(LUTI,'(I1)',END=2113,err=70041) IOPER   
        if (ioper .eq. 0) goto 2113
        goto 70042
70041    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 27114                                                             
70042   CALL system("clear")                                                   
        IF (IOPER.EQ.1) THEN                                                    
         IF (LUPLT.EQ.49) THEN                                                  
          WRITE(LUTO,'('' Current Plot Operation is the 1st'')')                
          WRITE(LUTO,'('' To switch to the 2nd, enter any character;'',         
     $    /,'' else, enter <r>'')')                                             
         ELSE                                                                   
          WRITE(LUTO,'('' Current Plot Operation is the 2nd'')')                
          WRITE(LUTO,'('' To switch to the 1st, enter any character;'',         
     $    /,'' else, enter <r>'')')                                             
         ENDIF                                                                  
         READ(LUTI,'(A1)',END=7113)  atemp                                      
         if (atemp .eq. "") goto 7113
         IF(LUPLT.EQ.49) THEN                                                   
          LUPLT=50                                                              
         ELSE                                                                   
          LUPLT=49                                                              
         ENDIF                                                                  
         GOTO 7113                                                              
        ELSEIF (IOPER.EQ.2) THEN                                                
C Set directive for last dependent var, if any                                  
         IF(YVAR1.NE.'NONE') THEN                                               
          IF ((XAXIS).AND.(MLTIPL)) THEN                                        
           NAMFUN=YVAR1(1:2)//YVAR2(1:2)//XVAR1(1:4)                            
           WRITE(LUPLT,'('' YAXIS=['',A8,'','',A8,''],%FOR, '',A8)')      
     $     YVAR1,YVAR2,NAMFUN                                                   
           WRITE(LUPLT,'('' XAXIS='',A8,'',%FOR, '',A8)') XVAR1,NAMFUN    
          ELSEIF (XAXIS) THEN                                                   
           NAMFUN=YVAR1(1:4)//XVAR1(1:4)                                        
           WRITE(LUPLT,'('' YAXIS='',A8,'',%FOR, '',A8)')   
     $     YVAR1,NAMFUN                                                         
           WRITE(LUPLT,'('' XAXIS='',A8,'',%FOR, '',A8)') XVAR1,NAMFUN   
          ELSEIF (MLTIPL) THEN                                                  
           NAMFUN=YVAR1(1:4)//YVAR2(1:4)                                        
           WRITE(LUPLT,'('' YAXIS=['',A8,'','',A8,''],%FOR, '',A8)')    
     $     YVAR1,YVAR2,NAMFUN                                                   
          ENDIF                                                                 
         ENDIF                                                                  
         WRITE(LUTO,'(''   Enter Dependent Variable Name (Y-axis): '')')        
         READ(LUTI,'(A8)',END=7113) YVAR1                                       
         if (yvar1 .eq. "") goto 7113
         GOTO 71121                                                             
        ELSEIF (IOPER.EQ.3) THEN                                                
C Set X-axis Variable                                                           
         WRITE(LUTO,'(''   Enter X-axis Variable Name: '')')                    
         READ(LUTI,'(A8)',END=7113) XVAR1                                       
         if (xvar1 .eq. "") goto 7113
         XAXIS=.TRUE.                                                           
         GOTO 7113                                                              
        ELSEIF (IOPER.EQ.4) THEN                                                
C Multiple Curves, Y-axis                                                       
         WRITE(LUTO,'(''   Enter 2nd Y-axis Variable Name: '')')                
         READ(LUTI,'(A8)',END=7113) YVAR2                                       
         if (yvar2 .eq. "") goto 7113
         MLTIPL=.TRUE.                                                          
         GOTO 7113                                                              
        ELSEIF (IOPER.EQ.5) THEN                                                
C Select Further Options                                                        
         IF(YVAR1.NE.'NONE') THEN                                               
          IF ((XAXIS).AND.(MLTIPL)) THEN                                        
           NAMFUN=YVAR1(1:2)//YVAR2(1:2)//XVAR1(1:4)                            
          ELSEIF (XAXIS) THEN                                                   
           NAMFUN=YVAR1(1:4)//XVAR1(1:4)                                        
          ELSEIF (MLTIPL) THEN                                                  
           NAMFUN=YVAR1(1:4)//YVAR2(1:4)                                        
          ELSE                                                                  
           NAMFUN=YVAR1                                                         
          ENDIF                                                                 
          GOTO 71130                                                            
         ELSE                                                                   
          CALL PAUSER(' Select Dependent Variable first ... hit <r> ')          
          GOTO 7113                                                             
         ENDIF                                                                  
        ELSE                                                                    
         GOTO 7113                                                              
        ENDIF                                                                   
C start options                                                                 
71130   continue
        CALL system("clear")                                                    
        IF(MLTIPL) THEN                                                         
         WRITE(LUTO,'('' Set Options for '',A8,'' and '',A8,'' vs. '',          
     $   A8,'' : '',/)') YVAR1,YVAR2,XVAR1                                      
        ELSE                                                                    
         WRITE(LUTO,'('' Set Options for '',A8,'' vs. '',                       
     $   A8,'' : '',/)') YVAR1,XVAR1                                            
        ENDIF                                                                   
        WRITE(LUTO,7115)                                                        
7115    FORMAT(                                                                 
     $ '    (1) Set Minimum/Range ',/                                           
     $ '    (2) Scatter Plot ',/                                                
     $ '    (3) Replot (if data exceeds bounds)',/                              
     $ '    (4) Adiabats ',/                                                    
     $ '    <r> = Plot Operations Menu'/)                                       
27115   READ(LUTI,'(I1)',END=7777,err=70043) IMODE         
        if (imode .eq. 0) goto 7777
        goto 70044
70043    WRITE(6,'('' Non-integer input; try again'')')                         
         GOTO 27115                                                             
70044   CALL system("clear")                                                    
        IF (IMODE.LT.1.OR.IMODE.GT.4) GOTO 71130                                
        IF (IMODE.EQ.1) THEN                                                    
C  Set bottom and range                                                         
         XBOT='-99.0'                                                           
         XRANGE='-99.0'                                                         
         IF(XAXIS) THEN                                                         
          WRITE(LUTO,'('' Set Minimum for '',A8,'', <r>=float: '')')XVAR1  
          READ(LUTI,'(A8)',END=71131) XBOT                                      
          if (xbot .eq. "") goto 71131
          GOTO 71132                                                            
71131     continue
          XBOT='-99.0'                                                          
71132     WRITE(LUTO,'('' Set Range for '',A8,'', <r>=float: '')')XVAR1        
          READ(LUTI,'(A8)',END=71133) XRANGE                                    
          if (xrange .eq. "") goto 71133
          GOTO 71134                                                            
71133     continue
          XRANGE='-99.0'                                                        
         ENDIF                                                                  
71134    WRITE(LUTO,'('' Set Minimum for '',A8,'', <r>=float: '')')YVAR1     
         READ(LUTI,'(A8)',END=71135) YBOT                                       
          if (ybot .eq. "") goto 71135
         GOTO 71136                                                             
71135    continue
         YBOT='-99.0'                                                           
71136    WRITE(LUTO,'('' Set Range for '',A8,'', <r>=float: '')')YVAR1       
         READ(LUTI,'(A8)',END=71137) YRANGE                                     
          if (yrange .eq. "") goto 71137
         GOTO 71138                                                             
71137    continue
         YRANGE='-99.0'                                                         
71138    CONTINUE                                                               
         WRITE(LUPLT,'(                                                         
     $   '' BOTRNG=[('',A8,'','',A8,''),('',A8,'','',A8,'',)],%FOR, '',  
     $   A8)') XBOT,XRANGE,YBOT,YRANGE,NAMFUN                                   
         GOTO 71130                                                             
        ELSEIF (IMODE.EQ.2) THEN                                                
C Scatter Plots                                                                 
         WRITE(LUPLT,'('' PLTYPE = PT, %FOR, '',A8)') NAMFUN             
         GOTO 71130                                                             
        ELSEIF (IMODE.EQ.3) THEN                                                
C Replots                                                                       
         WRITE(LUPLT,'('' REPLOT = YES, %FOR, '',A8)') NAMFUN         
         GOTO 71130                                                             
        ELSEIF (IMODE.EQ.4) THEN                                                
C Adiabats                                                                      
         WRITE(LUTO,'('' Select Interval (float, default 1.0): '')')            
37113    READ(LUTI,'(F8.2)',END=71130,err=70045) ADIBAT    
         if (adibat .eq. 0.0) goto 71130
         goto 70046
70045     WRITE(6,'('' Non-numeric input; try again'')')                        
          GOTO 37113                                                            
70046    WRITE(LUPLT,'('' ADIBAT = '',F8.2,'',%FOR, '',A8)')ADIBAT,NAMFUN     
         GOTO 71130                                                             
        ENDIF                                                                   
C done -- go back to Plot operations menu for next Dependent variable           
 7777   continue
        GOTO 7112                                                               
       ENDIF                                                                    
       IMODE=3                                                                  
       GOTO 1112                                                                
      ENDIF                                                                     
      GOTO 5                                                                    
C  get user defined variables                                                   
 1200 CALL USRVAR                                                               
      GOTO 5                                                                    
C                                                                               
C   exit                                                                        
C                                                                               
 9999 continue
      CALL system("clear")                                                      
C  offer reminder to SAve setups                                                
      WRITE(LUTO,'(//                                                           
     $'' If you have made changes to either the Project or Flight ''/           
     $'' Setups, the new setups are NOT restorable until you SAve ''/           
     $'' them. Enter any character for the SAve menu; else, to exit''/          
     $'' to Setup Menu without SAving, enter <r>'')')                           
      READ(LUTI,'(A)',END=19998) IOPT                                           
      if (iopt .eq. "") goto 19998
      IEND=1                                                                    
      GOTO 700                                                                  
19998 continue
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SETTIT                                                         
      INCLUDE "gpifile.h"                                                       
C  character strings for months of year                                         
      CHARACTER*3 MONTHS(12)                                                    
      CHARACTER*2 DATE(3)                                                       
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',        
     $            'OCT','NOV','DEC'/                                            
      WRITE(DATE,'(I2)') IDATEF                                 
      DO 19799 M=20,1,-1                                                        
       IF (USRTIT(M:M).NE.' ') THEN                                             
        IPOS=M                                                                  
        GOTO 19800                                                              
       ENDIF                                                                    
19799 CONTINUE                                                                  
C PRTITL will be composed as follows whenever it is involved in display         
C or termination is about to happen                                             
19800 PRTITL=IPROJ//'-'//IFLGHT//'  '//USRTIT(1:IPOS)                           
     $ //'  '//DATE(2)//MONTHS(IDATEF(1))//DATE(3)                              
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DISCAL(RDONLY)                                                 
      INCLUDE "gpio.h"                                                          
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INTEGER INTERR                                                            
      COMMON/IOERR/INTERR                                                       
C                                                                               
C  Display calibration coeff.'s, offer changes if RDONLY is false               
C                                                                               
C  NROWS -- number of lines on screen                                           
C  ISTART -- start word in SDINAM for display                                   
C  NMLEFT -- number of vars in 1st NFULL words of SDINAM left for display       
C                                                                               
      CHARACTER*8 CNAME                                                         
      CHARACTER*40 ISTRNG                                                       
      LOGICAL RDONLY                                                            
      NROWS=16                                                                  
      ISTART=1                                                                  
      NMLEFT=NFULL                                                              
 400  CALL system("clear")                                                      
      WRITE(LUTO,9741)(SDINAM(J),C1(J),C2(J),C3(J),J=ISTART,MIN0(NFULL,         
     $ ISTART+NROWS-1))                                                         
 9741 FORMAT(1x,A8,3X,A13,1X,A13,1X,A13)        
      DO 19744 J=1,(NROWS - NMLEFT)                                             
19744  WRITE(LUTO,'('' '')')                                                    
      IF(RDONLY) THEN                                                           
       WRITE(LUTO,'(///,'' Hit <r> for next page, X to exit'')')                
       GOTO 405                                                                 
      ENDIF                                                                     
C                                                                               
7401  WRITE(LUTO,'(//                                                           
     $'' Enter new coeff. under correct field; blanks=no chg;'',            
     1'' <r>=next page; X=exit''/                                               
     1'' VARxxxxxC0xxxxxxxxxxxC1xxxxxxxxxxxC2xxxxxxxxxxx'')')         
      ISTRNG='                                                     '            
 9804 FORMAT(A8,A13,A13,A13)                                                    
  405 CNAME='UNCHGD'                                                            
29804 READ(LUTI,9804,END=673)CNAME,ISTRNG(1:13),ISTRNG(14:26),                  
     $ ISTRNG(27:39)                                                            
      if (cname .eq. "") goto 673
      GOTO 674                                                                  
  673 cname = 'UNCHGD'
C input a carriage return                                                       
      IF(CNAME.EQ.'UNCHGD') THEN                                                
C adjust number left to display and starting position in SDINAM                 
       NMLEFT=NMLEFT-NROWS                                                      
       ISTART=ISTART+NROWS                                                      
C if any more left, go to next page of display                                  
       IF(ISTART.LE.NFULL) GOTO 400                                             
       RETURN                                                                   
      ENDIF                                                                     
C input an 'X' ==> done                                                         
674   call caps(cname,8)
      IF(CNAME.EQ.'X') RETURN                                                   
      IF (RDONLY) GOTO 400                                                      
      CALL SERCH(CNAME,SDINAM,NFULL,INDX)                                       
      IF(INDX.NE.0) GO TO 402                                                   
      WRITE(LUTO,9806)CNAME                                                     
 9806 FORMAT(' Name',2X,A8,'  not found')                                       
      GO TO 7401                                                                
C  Change only those coefficients which are entered explicitly                  
  402 CALCHG=.TRUE.                                                             
      IF(ISTRNG(1:13).NE.'             ')READ(ISTRNG,'(A13)',err=81112) 
     $C1(INDX)          
      goto 81113
81112  WRITE(6,'('' Non-integer input; try again'')')                           
       goto 29804
81113 continue
      IF(ISTRNG(14:26).NE.'             ')READ(ISTRNG,'(13X,A13)',
     $err=81112) C2(INDX)    
      IF(ISTRNG(27:39).NE.'             ')READ(ISTRNG,'(26X,A13)',              
     $err=81112) C3(INDX)    
C  redisplay current page with updated coefficients                             
      GO TO 400                                                                 
      END                                                                       
