C  include the file that defines MAXNAM for reference below                     
      INCLUDE "gpdictn.h"                                                       
C** Project setup variables                                                     
      common/cproj/phdg,nwords,nvar,nvolt,ideltt,
     1             INILAT,INILON
      COMMON / PROJ / NMUSER,NMPROJ,TURBRT,                           
     2                NMACCT,NAMES(maxnam) ,                                   
     3                QCX,PSX,TTX,AVANE,BVANE,DPX,WINDS,
     4                UNITS(maxnam),NMSCI,IARCFT,IPROJ,NOTES        
C                                                                               
      REAL INILAT,INILON                                                        
C  account number digits 5 - 9                                                  
      CHARACTER *5 ACCTNO                                                       
      PARAMETER (ACCTNO = '41113')                                              
      INTEGER NWORDS,NVAR,NVOLT,PHDG                                            
      CHARACTER*16 NMPROJ                                                       
      CHARACTER*8  NMUSER                                                       
      CHARACTER*3  TURBRT                                                       
      CHARACTER*12 NMACCT                                                       
      CHARACTER*6 NAMES,QCX,PSX,TTX,AVANE,BVANE,DPX                     
      CHARACTER*4 UNITS,NMSCI                                           
      CHARACTER*3 IARCFT,IPROJ                                                  
      CHARACTER*1 WINDS                                                         
      CHARACTER*80 NOTES                                                        
C                                                                               
C     PHDG     => RATE OF INPUT VARIABLE PHDG                                   
C     INILAT, INILON  => INITIAL LATITUDE, LONGITUDE                            
C     NMRECS   => NUMBER OF RECORDS IN DATA FILE  (2 WORD INTEGER)              
C     NVAR     => TOTAL NUMBER OF VARIABLES                                     
C     NVOLT    => NUMBER OF MODE 1 (VOLTAGES) VARIABLES IN THE FILE             
C     NMUSER   => NAME OF THE USER (ASCII) TO WHOSE READER OUTPUT IS SEN        
C     TURBRT   => TURBULENCE RATE (EITHER HRT OR LRT)                           
C     NMPROJ   => NAME OF PROJECT                                               
C     NMACCT   => ACCOUNT NUMBER ( NMSCI // '41113' // IARCFT )                 
C     NAMES    => 6 CHARACTER ARRAY OF VARIABLE NAMES                           
C     QCX      => 6 CHARACTER NAME OF QC FOR COMPUTING TAS,WINDS                
C     PSX      => 6 CHARACTER NAME OF STATIC PRESS. FOR TAS,WINDS               
C     TTX      => 6 CHARACTER NAME OF TEMPERATURE FOR TAS,WINDS                 
C     AVANE    => 6 CHARACTER NAME OF ATTACK VANE FOR WINDS                     
C     BVANE    => 6 CHARACTER NAME OF SIDESLIP VANE FOR WINDS                   
C     DPX      => 6 CHARACTER NAME OF DEW POINT USED FOR HUMIDITY               
C     WINDS    => 'R' IF RADOME WINDS REQUESTED; ELSE ' '                       
C     UNITS    => 4 CHAR ARRAY CONTAINING ASCII UNITS OF EACH VARIABLE          
C     NMSCI    => USER'S SCIENTIST NUMBER (ASCII)                               
C     IARCFT   => AIRCRAFT NUMBER (ASCII)                                       
C     IPROJ    => PROJECT NUMBER                                                
C     NOTES    => USER NOTE SPACE                                               
C                                                                               
C   flight setup information + add'l GENPRO info...these correspond to          
C   major portion of arguments used in PRODUCT APPEND file                      
C                                                                               
      COMMON /cFLT / IDATEF,ITIMEF,NSXIN,NSXOUT,
     2               OTMSEG,OTMFLG,ONMSEG,                                      
     3               ITMSEG,ITMFLG,INMSEG,NUMVOL                               
      COMMON /fFLT / EXEC,CALCHG,FLTSET,FLTSAV,INPUT,                           
     2               OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC,                        
     3               PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS           
      COMMON / FLT / DESCRP,PRTITL,USRTIT,IVTITL,                               
     2               IFLGHT,TAPNO,OUTPUT,GAP,TAPE1,                             
     3               QCREF                                                      
C                                                                               
C capable of handling 50 time segments                                          
      INTEGER MAXSEG                                                            
      PARAMETER (MAXSEG=50)                                                     
C capable of handling 20 input tapes per flight                                 
      INTEGER MAXTAP                                                            
      PARAMETER (MAXTAP=20)                                                     
      INTEGER NUMVOL                                                            
      INTEGER NSXOUT,NSXIN,IDATEF(3),ITIMEF(6)                                  
      INTEGER ITMSEG(6,MAXSEG),ITMFLG(MAXSEG),INMSEG                            
      INTEGER OTMSEG(6,MAXSEG),OTMFLG(MAXSEG),ONMSEG                            
      LOGICAL*1 EXEC,CALCHG,INPUT                                               
      LOGICAL*1 FLTSET,FLTSAV                                                   
      LOGICAL*1 OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC                              
      LOGICAL*1 PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS                
      CHARACTER*50 DESCRP                                                       
      CHARACTER*40 PRTITL                                                       
      CHARACTER*32 IVTITL                                                       
      CHARACTER*20 USRTIT                                                       
      CHARACTER*6  TAPNO(MAXTAP),OUTPUT                                         
      CHARACTER*5  IFLGHT                                                       
      CHARACTER*3  QCREF(2)                                                     
      CHARACTER*1  GAP,TAPE1                                                    
C                                                                               
C     MAXSEG   => MAXIMUM NUMBER OF TIME SEGMENTS, INPUT AND OUTPUT             
C     MAXTAP   => MAXIMUM NUMBER OF INPUT TAPES                                 
C     IDATEF   => DATE OF FLIGHT                                                
C     ITIMEF   => BEGINNING/ENDING TIME OF FLIGHT                               
C     ITMSEG,OTMSEG   => ARRAY OF ROWS OF BEGIN, END SNAPSHOTS                  
C     INMTIM,ONMTIM   => ARRAY OF FLAGS FOR VALID ROWS IN ABOVE                 
C     INMSEG,ONMSEG   => TOTAL NUMBER OF VALID ROWS IN ABOVE                    
C     NSXIN    => INPUT SNAPSHOT PERIOD IN SECONDS                              
C     NSXOUT   => OUTPUT SNAPSHOT PERIOD IN SECONDS                             
C     INPUT    => TRUE: INPUT DATASET IS ON MASS STORE; FALSE: MAG TAPE         
C     EXEC     => TRUE IF EXECUTION IS TO CONTINUE                              
C     CALCHG   => TRUE ONLY IF CALIBRATION CONSTANTS CHANGED                    
C     FLTSET   => TRUE ONLY IF FLIGHT SETUP HAS BEEN DONE BY USER               
C     FLTSAV   => TRUE ONLY IF CURRENT FLIGHT SETUP IS SAVED ON DISK            
C     OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC  => TRUE IF OPERATION                  
C      IS TO BE ENABLED (INPUT,OUTPUT,PRINT,PLOT,2nd PLOT,STATS)                
C     PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS => TRUE IF DISPOSE       
C      OF GIVEN OPERATION'S OUTPUT TO GIVEN DEVICE SHOULD OCCUR                 
C      (PR2xx ==> print operation, PL2xx ==> plot operation,                    
C       OUT2xx ==> output operation; MP ==> Xerox Laser printer,                
C       MS ==> mass store, D1 ==> Dicomed film, IO ==> IBM reader)              
C     DESCRP   => DESCRIPTION OF CURRENT FLIGHT SETUP                           
C     PRTITL   => PROJECT TITLE (VARIABLE 'PROJECT' IN GENPRO)                  
C     USRTIT   => OPTIONAL USER SPECIFIED TITLE SECTION (DEFAULT=NMPROJ)        
C     IVTITL   => INDEPENDENT VARIABLE TITLE                                    
C     IFLGHT   => FLIGHT NUMBER (CHARCETER * 5)                                 
C     TAPNO    => TAPE NUMBERS FOR INPUT FOR ONE FLIGHT                         
C     NUMVOL   => NUMBER OF INPUT TAPES FOR THIS FLIGHT                         
C     OUTPUT   => NAME OF MS OUTPUT FILE (E.G., G57077)                         
C     GAP      => LETTER TO APPEND TO FLIGHT NUMBER IN MS OUTPUT FILE IF        
C                 THIS FLIGHT IS A SEGMENT DUE TO TIME GAPS; IF NOT A           
C                 GAPPED FLIGHT, GAP = '0'                                      
C     TAPE1    => IF GAP <> '0', TAPE1 = LETTER SUFFIX OF FIRST TAPE TO         
C                 ACQUIRE FOR PROCESSING THIS SEGMENT; IF TAPE1='0', THEN       
C                 THERE IS ONLY ONE TAPE FOR ENTIRE FLIGHT AND IT HAS NO        
C                 SUFFIX; IF GAP = '0', TAPE1 IS IRRELEVANT                     
C     QCREF    => DYNAMIC PRESSURE REFERENCE SENSORS                            
C                                                                               
      COMMON/ EXEC2 / DISK                                       
      CHARACTER *1 DISK                                                         
