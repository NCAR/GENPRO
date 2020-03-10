
        SUBROUTINE MEMRD(OUT,ACC,THR,PMK,DME,PLT2,TURB)                         
                                                                                
C--------Subroutine MEMRD  reads an input DIRECT file and puts the              
C        variables into the appropriate arrays.                                 
                                                                                
      include "gprfl.h"
      include "gpref.h"
      include "gppms.h"
      include "gpplrar.h"
      include "gparc.h"
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
C     INCLUDE "gpusrvar.h"                                                      
                                                                                
      INTEGER TNUM,TRATE                                                  
                                                                                
      CHARACTER*12 ACC                                                          
      CHARACTER*6 TUNITS                                                        
      CHARACTER*3 TURB                                                          
      character*80 genpth
                                                                                
C----------PARAMETER VARIABLES                                                  
      INTEGER OUT,RDME,THR,DME,PLT2,PMK             
                                                                                
C-----------SUBROUTINE VARIABLES                                                
      CHARACTER*8  TMP,RQC(5),CQC(5),RPS(4),CPS(4),RDP(4),CDP(4)                
      CHARACTER*3  TUR, AIR                                                     
      INTEGER*2 COUNT,TCOUNT                                                    
      INTEGER  J,K,LOOP,NQC,NPS,NDP                                             
                                                                                
      DATA CQC/'QCFC    ','QCBC    ','QCGC    ','QCWC    ','QCRC    '/          
      DATA RQC/'QCF     ','QCB     ','QCG     ','QCW     ','QCR     '/          
      DATA RPS/'PSFD    ','PSF     ','PSB     ','PSW     '/                     
      DATA CPS/'PSFDC   ','PSFC    ','PSBC    ','PSWC    '/                     
      DATA RDP/'DPT     ','DPB     ','DPG     ','        '/                     
      DATA CDP/'DPTC    ','DPBC    ','DPGEO   ','        '/                     
                                                                                
      NQC = 5                                                                   
      NPS = 4                                                                   
      NDP = 3                                                                   
      NUMPAR = 20                                                               
      
      REWIND (21)                                                               
      READ(21) EXEC                                                             
C---------EXEC is true if there is data to process.                             
      IF  ( EXEC) THEN                                                          
         OUT=0                                                                  
      ELSE                                                                      
C-----------Otherwise set Exit flag(OUT) and skip routine.                      
         OUT=1                                                                  
         GOTO 3132                                                              
      ENDIF                                                                     
                                                                                
C----------------------Clear array                                              
      DO 15,J=1,600                                                             
         NAME(J) = '        '                                                   
   15 CONTINUE                                                                  
                                                                                
      NUMVAR = 0                                                                
                                                                                
                                                                                
      READ(21) NUMVAR,(NAME(J),J=1,NUMVAR)                                      
      READ(21) NUMPR,(PRARR(J),J=1,NUMPR)                                       
      READ(21) NUMPL,(PLARR(J),J=1,NUMPL)                                       
                                                                                
                                                                                
C------------------Check for implied KEYWORDs                                   
      CALL GLCHK                                                                
      CALL RCLR(NAME,NUMVAR,600)                                                
                                                                                
                                                                                
      READ(21) (RFSEN(J),J=1,6)                                                 
                                                                                
C------SORT REF SENSORS                                                         
      TMP= RFSEN(1)                                                             
      RFSEN(1)=RFSEN(3)                                                         
      RFSEN(3) = RFSEN(4)                                                       
      RFSEN(4) = RFSEN(2)                                                       
      RFSEN(2) =TMP                                                             
      call caps(rfsen(1),8)
      call caps(rfsen(2),8)
      call caps(rfsen(3),8)
      call caps(rfsen(4),8)
                                                                                
      DO 40, J= 1, NQC                                                          
         IF (RFSEN(2) .EQ. CQC(J)) RFSEN(2) = RQC(J)                            
   40 CONTINUE                                                                  
      DO 41, J= 1, NDP                                                          
         IF (RFSEN(3) .EQ. CDP(J)) RFSEN(3) = RDP(J)                            
   41 CONTINUE                                                                  
      DO 42, J= 1, NPS                                                          
         IF (RFSEN(4) .EQ. CPS(J)) RFSEN(4) = RPS(J)                            
   42 CONTINUE                                                                  
                                                                                
                                                                                
                                                                                
       READ(21) NMACCT,NMUSER,NMPROJ,IPROJ,IFLGHT,PRTITL,IARCFT,IVTITL,         
     $ IDATEF,ITIMEF,ITMSEG,INMSEG,ITMFLG,OTMSEG,ONMSEG,OTMFLG,                 
     $ NSXIN,NSXOUT,                                                            
     $ INILAT,INILON,QCREF,                                                     
     $ NUMVOL,TAPNO,OUTPUT,GAP,TAPE1,TURBRT,                                    
     $ OUTCYC,STACYC,PRTCYC,PLTCYC,PL2CYC,                                      
     $ PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS,                        
     $ TAU1,TAU2,TAU3,DOF,BDIA,WIRET,FNUSS,REXP                                 
C     READ(21)NMUSRV,USRVEC                                                     
C     READ(21)NUMKEY,KEYWRD                                                     
                                                                                
      TURB = TURBRT                                                             
      call caps(turb,3)
      IF (.NOT. PL2CYC) NUMPL = 0                                               
                                                                                
C--------SETS FLAG INDICATING WHICH PLANE IS BEING FLOWN                        
      KFLAG = IARCFT(3:3)                                                       
      IF (PL2CYC) PLT2=1                                                        
      ACC = NMACCT                                                              
      ACC(10:12) = IARCFT                                                       
                                                                                
                                                                                
 3131 continue                                                                 
C-----Pick up PMS & GUST affiliated VARIABLES.                                  
      CALL PBCHK(NAME,NUMVAR,1,600)                                             
 3132 RETURN                                                                    
      END                                                                       
