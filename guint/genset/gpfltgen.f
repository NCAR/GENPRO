      SUBROUTINE FLTGEN                                                        
                                                                                
C--------Subroutine FLTGEN generates a flight deck.                             
C                                                                               
C  Here's the procedure definition in GPPROD DECK that the generated            
C  deck must address:                                                           
C                                                                               
C                                                                               
C LGO,USER=:,PROJECT=:,PROJNO=:,FLTNO=:,TITLE=:,AIRCRAFT=:,IVTITLE=:,^          
C PRDAY=:,PRMON=:,PRYEAR=:,PRTIME=:,^                                           
C BEGIV=:,ENDIV=:,BEGSNP=:,ENDSNP=:,OUTBI=:,OUTEI=:,^                           
C INILAT=:,INILON=:,STSHFT1=:,STSHFT2=:,QCREF1=:,QCREF2=:,^                     
C VOLUMES='VOLUME1':,UNITS=11:,KPOS=((0,0)):,KEOF=1:0,INPUT=MS:MT,^             
C SEG=:,OUTPUT=:,TAPE=NONE:,TURBRT=HRT:HRT,^                                    
C TAPEA=NONE:,TAPEB=NONE:,TAPEC=NONE:,TAPED=NONE:,TAPEE=NONE:,^                 
C TAPEF=NONE:,TAPEG=NONE:,TAPEH=NONE:,TAPEI=NONE:,TAPEJ=NONE:,^                 
C TAPEK=NONE:,TAPEL=NONE:,TAPEM=NONE:,TAPEN=NONE:,TAPEO=NONE:,^                 
C TAPEP=NONE:,TAPEQ=NONE:,TAPER=NONE:,TAPES=NONE:,TAPET=NONE:,^                 
C FLUSHP=1800:1800,^                                                            
C CALCYC=0:-1,OUTCYC=0:-1,STATCYC=0:-1,^                                        
C PRTCYC=0:-1,PLTCYC=0:-1,PLTCYC2=0:-1,^                                        
C PR2D1=NO:YES,PR2MS=NO:YES,PR2IO=NO:YES,PL2IO=NO:YES,PL2D1=NO:YES,^            
C PL2MS=NO:YES,PL2MP=NO:YES,OUT2MS=NO:YES,^                                     
C PMS=:YES,WINDS=:R,^                                                           
C BRX=(,.75):(,.75),^                                                           
C BRY=(,.75):(,.75),^                                                           
C TAU1=(0.0000063):,TAU2=(0.0000038):,TAU3=(0.00):,^                            
C DOF=(2.74):(2.74),BDIA=(.180):(.180),^                                        
C TWIRE=(403.16):(403.16),CN1=(0.22):(0.22),EN1=(0.62):(0.62).                  
C                                                                               
C                                                                               
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpio.h"                                                          
      INCLUDE "gpifile.h"                                                      
C  Maximum number of output snapshots allowed for an output file with           
C  record length of 100                                                         
      PARAMETER (MAXOUT = 6)                                                    
      INTEGER J, I                                                              
      CHARACTER *1 TLET                                                         
      CHARACTER * 5 STAT1,tflght                                               
      CHARACTER *255 FMT                                                        
      CALL EXTEND (46)                                                          
      tflght = iflght
      WRITE(46,'('' FILENAME: '',A3,1X,A5)') IPROJ,tFLGHT          
      WRITE(46,9001) NMUSER                                            
 9001 FORMAT(' LGO,USER=',A8,',^')                                              
      WRITE(46,9002) NMPROJ                                                     
 9002 FORMAT(' PROJECT=',A16,',^')                                            
      WRITE(46,9003) IPROJ                                                     
 9003 FORMAT(' PROJNO=',A3,',^')                                                
         DO 9998,J=40, 1, -1                                                   
            IF (PRTITL(J:J) .NE. ' ') THEN                                     
               LOOP = J                                                        
               GOTO 9997                                                       
            ENDIF                                                              
 9998 CONTINUE                                                                 
 9997 WRITE(46,9005) '(''',(PRTITL(J:J),J=1,LOOP),'''),^',(' ',                
     $J=LOOP+1,40)                                                             
 9005 FORMAT(' TITLE=',42A)                                                     
      WRITE(46,9006)IARCFT                                                     
 9006 FORMAT(' AIRCRAFT=',A3,',^')                                              
      WRITE(46,9031)OUTPUT                                                     
 9031 FORMAT(' OUTPUT=',A6,',^')                                                
         DO 9996,J=32, 1, -1                                                   
            IF (IVTITL(J:J) .NE. ' ') THEN                                     
               LOOP = J                                                        
               GOTO 9995                                                       
            ENDIF                                                              
 9996    CONTINUE                                                              
 9995 WRITE(46,9007) '(''',(IVTITL(J:J),J=1,LOOP),'''),^',(' ',                
     $J=LOOP+1,32)                                                             
 9007 FORMAT(' IVTITLE=',34A)                                                   
      WRITE(46,8010) (ITMSEG(J,1),J=1,3)                                       
 8010 FORMAT(' PRTIME=(','''',I2,'H','''',',','''',I2,'M','''',',',             
     $                  '''',I2,'S','''','),^')                                
      WRITE(46,9008) IDATEF(2)                                                 
 9008 FORMAT(' PRDAY=',I2,',^')                                                 
      WRITE(46,9009) IDATEF(1)                                                 
 9009 FORMAT(' PRMON=',I2,',^')                                                 
      WRITE(46,9010) IDATEF(3)                                                 
 9010 FORMAT(' PRYEAR=',I2,',^')                                                
      WRITE(46,9011) 'BEGIV',(ITMSEG(J,1),J=1,3)                               
      WRITE(46,9011) 'ENDIV',(ITMSEG(J,INMSEG),J=4,6)                          
 9011 FORMAT(1x,A5,'=(',I2,'.,',I2,'.,',I2,'.),^')    
      IF (INMSEG .EQ. 1) WRITE(46,9012) 'BEGSNP',(ITMsEG(J,1),J=1,3)            
      IF (INMSEG .EQ. 2) WRITE(46,9013) 'BEGSNP',(ITMSEG(J,1),J=1,3),          
     $(ITMSEG(J,2),J=1,3)                                                      
 9012 FORMAT(1x,A6,'=((',I2,'.,',I2,'.,',I2,'.)),^')   
 9013 FORMAT(1x,A6,'=((',I2,'.,',I2,'.,',I2,'.)'     
     $,',(',I2,'.,',I2,'.,',I2,'.)),^')                                        
      IF (INMSEG .EQ. 1) WRITE(46,9012) 'ENDSNP',(ITMSEG(J,1),J=4,6)           
      IF (INMSEG .EQ. 2) WRITE(46,9013) 'ENDSNP',(ITMSEG(J,1),J=4,6),          
     $(ITMSEG(J,2),J=4,6)                                                      
      IF (ONMSEG .EQ. 1) THEN                                                  
       WRITE(46,9012) 'OUTBI ',(OTMSEG(J,1),J=1,3)                             
       WRITE(46,9012) 'OUTEI ',(OTMSEG(J,1),J=4,6)                             
      ELSE                                                                     
       IF(ONMSEG.GT.MAXOUT) THEN                                               
        CALL system("clear")                                                   
        WRITE(LUTO,'('' FATAL: Too many output snaps. Contact expert''         
     $  )')                                                                    
        CALL PAUSER(' Hit <r> to continue ... ')                               
        RETURN                                                                 
       ENDIF                                                                   
       LENG=28                                                                 
       IGO=11                                                                   
       IEND=IGO+LENG-1                                                          
       WRITE(FMT(IGO:IEND),200)                                                 
  200  FORMAT(''' ((''',',I2,','''.,''',',I2,','''.,''',',I2,','''.)''')  
       LENG=LENG+1                                                              
       DO 100 J=2,ONMSEG                                                        
        IGO=IEND+1                                                              
        IEND=IGO+LENG-1                                                         
        WRITE(FMT(IGO:IEND),201)                                                
  201   FORMAT(',',''',(''',',I2,','''.,''',',I2,',        
     $  '''.,''',',I2,','''.)''')                                               
  100  CONTINUE                                                                 
       WRITE(FMT(IEND+1:IEND+7),202)                                            
  202  FORMAT (',','''),^''',')')             
       WRITE(FMT(1:10),198)                                                     
  198  FORMAT ('(','''OUTBI=''',',')                                            
       WRITE(46,FMT)((OTMSEG(J,K),J=1,3),K=1,ONMSEG)                            
       WRITE(FMT(1:10),199)                                                     
  199  FORMAT (' (','''OUTEI=''',',')                              
       WRITE(46,FMT)((OTMSEG(J,K),J=4,6),K=1,ONMSEG)                            
      ENDIF                                                                     
      WRITE(46,9014) INILAT,INILON                                              
 9014 FORMAT(' INILAT=(',F9.4,'),INILON=(',F9.4,'),^')                          
      IF (PHDG .EQ. 1) STAT1 = '0.000'                                          
      IF (PHDG .EQ. 5) STAT1 = '-0.40'                                          
      IF (PHDG .EQ. 25) STAT1 = '-.040'                                         
      IF (PHDG .EQ. 50) STAT1 = '-.040'                                         
      IF (PHDG .EQ. 250) STAT1 = '-.008'                                        
      WRITE(46,9015) STAT1,STAT1                                                
 9015 FORMAT(' STSHFT1=(',A5,'),STSHFT2=(',A5,'),^')                            
      IF  (QCREF(2)(3:3) .EQ. 'x') QCREF(2) = QCREF(1)                          
      WRITE(46,9016) QCREF(1),QCREF(2)                                          
 9016 FORMAT(' QCREF1=',A3,',QCREF2=',A3,',^')                                  
      IF (GAP .EQ. '0') THEN                                                    
       IF (NUMVOL .EQ. 1) THEN                                                  
         WRITE(46,9027) TAPNO(1)                                                
 9027 FORMAT(' TAPE=',A6,',^')                                                  
       ELSE                                                                     
         TLET = 'A'                                                             
         DO 8388, J=1, NUMVOL                                                   
            WRITE(46,9028)TLET,TAPNO(J)                                         
 9028       FORMAT(' TAPE',A1,'=',A6,',^')                                      
            TLET = CHAR(ICHAR(TLET)+1)                                          
 8388    CONTINUE                                                               
       ENDIF                                                                    
      ELSE                                                                      
         WRITE(46,9030)GAP                                                     
         tFLGHT(5:5) = ' '                                                      
         IF ((NUMVOL .EQ. 1) .AND. (TAPE1 .EQ. '0')) THEN                       
            WRITE(46,9027) TAPNO(1)                                             
         ELSE                                                                   
           TLET = 'A'                                                           
           IF (TAPE1 .NE. '0') TLET = TAPE1                                     
           DO 8389, J=1, NUMVOL                                                 
              WRITE(46,9028)TLET,TAPNO(J)                                       
              TLET = CHAR(ICHAR(TLET)+1)                                        
 8389      CONTINUE                                                             
         ENDIF                                                                  
      ENDIF                                                                     
 9030 FORMAT(' SEG=',1A,',^')                                                   
      WRITE(46,9004) tFLGHT                                                     
 9004 FORMAT(' FLTNO=',A5,',^')                                                 
      WRITE(46,'('' TURBRT='',A3,'',^'')') TURBRT                              
 9017 FORMAT(' UNITS,KPOS,KEOF,^')                                              
      WRITE(46,9018) NSXIN                                                      
 9018 FORMAT (' FLUSHP=',I6,',^')                                               
      IF (OUTCYC) WRITE(46,'('' OUTCYC,^'')')                                  
      IF (STACYC) WRITE(46,'('' STATCYC,^'')')                                  
      IF (PRTCYC) WRITE(46,'('' PRTCYC,^'')')                                   
      IF (PLTCYC) WRITE(46,'('' PLTCYC,^'')')                                   
      IF (PL2CYC) WRITE(46,'('' PLTCYC2,^'')')                                  
      IF (PR2D1) WRITE(46,'('' PR2D1=YES,^'')')                                 
      IF (PR2MS) WRITE(46,'('' PR2MS=YES,^'')')                                 
      IF (PR2IO) WRITE(46,'('' PR2IO=YES,^'')')                                 
      IF (PL2IO) WRITE(46,'('' PL2IO=YES,^'')')                                 
      IF (PL2D1) WRITE(46,'('' PL2D1=YES,^'')')                                 
      IF (PL2MS) WRITE(46,'('' PL2MS=YES,^'')')                                 
      IF (PL2MP) WRITE(46,'('' PL2MP=YES,^'')')                                 
      IF (OUT2MS) WRITE(46,'('' OUT2MS=YES,^'')')                               
C                                                                              
C  determine if Radome winds requested                                          
C                                                                               
      DO 16 J=1,PSPOUT                                                          
C  assuming UIR implies that all radome wind vars are present                   
       IF (VRPOSS(PSPPTR(J)).EQ.'UIR') THEN                                     
        WRITE(46,'('' WINDS=R,^'')')                                            
        GOTO 21                                                                 
       ENDIF                                                                    
   16 CONTINUE                                                                  
   21 CONTINUE                                                                  
C                                                                               
       WRITE(46,'('' INPUT=MS,^'')')                                            
C     ENDIF                                                                     
      IF (NUMPMS .GT. 0)  WRITE(46,'('' PMS,^'')')                              
      WRITE(46,9019)TAU1,TAU2,TAU3                                             
 9019 FORMAT(' TAU1=(',F11.8,'),TAU2=(',F11.8,'),TAU3=(',F11.8,'),^')           
      WRITE(46,9020)DOF                                                         
 9020 FORMAT(' DOF=(',F6.3,'),^')                                               
      WRITE(46,9021)BDIA                                                        
 9021 FORMAT(' BDIA=(',F6.4,'),^')                                              
      WRITE(46,9022)WIRET                                                       
 9022 FORMAT(' TWIRE=(',F9.4,'),^')                                             
      WRITE(46,9023)FNUSS                                                       
 9023 FORMAT(' CN1=(',F9.4,'),^')                                               
      WRITE(46,9024)REXP                                                        
 9024 FORMAT(' EN1=(',F9.4,').')                                                
      RETURN                                                                    
      END                                                                      
