C                                                                               
C---------------------------------------------------------------------          
C                    A D S U D R A W     VERSION 2.0                            
C---------------------------------------------------------------------          
C                                                                               
C    PROGRAM ADSU5 IS A PROGRAM THAT CAN BE USED TO DECODE A RAW TAPE           
C  HEADER AND PROVIDE A FACILITY TO DUMP THAT TAPE. DEPENDING ON THE            
C  OPTIONS SET AT THE BOTTOM OF THE PROGRAM (INPUT FILE ON THIS DECK),          
C  TAPE INFORMATION CAN BE DUMPED EITHER BY RECORD OR TIME, HORIZON-            
C  TALLY OR VERTICALLY.                                                         
C                                                                               
C  THREE SEPARATE OUTPUT FILES ARE GENERATED FROM THIS CODE AND THEY            
C  ARE AS FOLLOWS :                                                             
C                                                                               
C        -  A CALIBRATION TABLE SUITABLE FOR INSERTION INTO A                   
C           CALIB USER DIRECTIVE OF A GENPROII DECK.                            
C                                                                               
C        -  A TAPE HEADER FILE SUITABLE FOR INSERTION INTO AN                   
C           INPUT USER DIRECTIVE OF A GENPROII DECK.                            
C                                                                               
C        -  A LISTING OF THE PROGRAM, LOAD TABLE, AND, IF SELECTED,             
C           A RAW TAPE DUMP.                                                    
C                                                                               
C                                                                               
C                                                                               
C                   - HISTORY  -                                                
C                                                                               
C   VERSION 1.0   OCTOBER 6, 1982 ADS DECODER (7600) AND INITIAL CON-           
C                        VERTION TO CRAY--CELIA CHEN                            
C   VERSION 1.1   JANUARY 5, 1983  OUTPUT GENERATION--KENT KRUMVIEDA            
C   VERSION 1.2   FEBURARY 9, 1983 KING AIR FEATURES ADDED-CELIA CHEN           
C   VERSION 1.3   JANUARY 12, 1984 REWRITE FOR 63 & 120 SDI BLOCK               
C                        SIZE POSSIBITY, AND RAWDUMP FEATURE ADDED-KK           
C   VERSION 1.4   JUNE 6, 1984 BLOCKS DME AND LORANC ADDED-KRUMVIEDA *          
C   VERSION 1.5   JUNE 5, 1985 LORANC BLOCK MODIFIED TO ACCEPT DATA             
C                              RATE OF 182. CODE INTERNALLY DOCU-               
C                              MENTED                     -BW                   
C   VERSION 2.0   OCT. 16,1986 LORANC BLOCK SIZE CHANGED TO 91 WORDS.           
C                              VARIABLES DECODED FROM LORANC BLOCK:             
C                                                                               
C                                 VARNAM 1STBIT #BITS RATE KONKEY               
C                              1) CSTAT  12+XL      4    1      0               
C                              2) CSEC   16+XL     16    1      1               
C                              3) CFSEC  32+XL     16    1      1               
C                              4) LORN   48+XL      8   64      0               
C                                                                               
C                                 WHERE XL IS THE FIRST BIT LOCATION            
C                                 OF THE LORANC BLOCK (LRNC)  -CC               
C   VERSION 2.1   MAY  27,1987 BUGFIX, INCREASED GENPRO2 COMPATABILITY          
C                              JEFF BERRY X1645, ADSUD REWRITE                  
C   VERSION 2.2   JUL 1, 1987  OUTPUT TEXT FILE (UNIT 9) WITH INFO USED         
C                              DOWNSTREAM BY GENSET ROUTINE                     
C                 OCT 2, 1987  BUGFIX ON PMS1 FIRST BIT LOCATION -CC            
C   VERSION 3.0   DEC 7, 1987  COMBINES BOTH VERSIONS OF ADSRAW, GARY'S         
C                              AND CELIA'S.  HOPEFULLY PROVIDING A VER-         
C                              SION EVERYONE CAN USE.  REVISES OUTPUT           
C                              FORMAT FOR LORAN-C AND DME        -JB            
C                                                                               
C---------------------------------------------------------------------          
C     QUESTIONS ABOUT THE CODE OR THE EXECUTION SHOULD BE DIRECTED              
C     TO  JEFF BERRY  X-1645  RM-19E, OR CELIA CHEN X-1648                      
C---------------------------------------------------------------------          
C  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *          
C  CODE MODIFIED TO GET 16-BIT WORD FOR EV1 INSTEAD OF JUST 8 LOWER             
C  BITS  7MAY85 ESS PROJ.235                                                    
C*********************************************************************          
C                                                                               
C                                                                               
      PROGRAM ADSU5                                                             
                                                                                
      DIMENSION NFREQ(20),NCHANL(20),ITIME(3),NDX(120),LINK(122)                
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),                   
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      INTEGER DUMMY,CONKEY,XFLAG,DFLAG                                          
 1000 FORMAT(2X,'PARITY ERROR ON HEADER RECORD ')                               
 1010 FORMAT(//,'  DATE ',3I3,'    START TIME ',3I3,4X,'J    FREQ   CHAN        
     1NELS ',/,(42X,I3,2I8  ))                                                  
 1020 FORMAT(10A8)                                                              
 1030 FORMAT(I5)                                                                
 1040 FORMAT(2X,'END OF FILE ')                                                 
 1050 FORMAT(2X,'END OF TAPE ')                                                 
                                                                                
C-----READ IN LOGICAL UNIT NUMBER                                               
      READ(5,1020) DUMMY                                                        
      READ(5,1030) IUNIT                                                        
                                                                                
C-----INITIALIZE NEW HEADER                                                     
      CALL RDTAPE(IUNIT,1,2,NBUF,1200)                                          
      CALL IOWAIT(IUNIT,NSTATE,LEN)                                             
                                                                                
C-----CHECK QUALITY OF READ                                                     
      NSTATE=NSTATE+1                                                           
      GO TO(20,80,10,90)NSTATE                                                  
   10 WRITE(6,1000)                                                             
      GOTO 100                                                                  
C                                                                               
C-----THE INPUT TAPE HAS BEEN WRITTEN BY A 16 BIT MACHINE, THEREFORE            
C     THE ACTUAL NUMBER OF WORDS READ IN NEEDS TO BE MODIFIED TO                
C     ACCOMMODATE THE 16 BIT SIZE.                                              
   20 NWDS=64*LEN/16                                                            
                                                                                
C-----NOW CALL THE HEADER ROUTINE WITH THE APPROPRIATE # WORDS                  
      CALL HEADER(NWDS)                                                         
C-----EXTRACT THE SDI SAMPLING RATES AND SORT THEM LOWEST TO HIGHEST            
      NCH=0                                                                     
      DO 30 I=1,MAXSDI                                                          
   30 NDX(I)=ISDI(I)                                                            
                                                                                
C-----FSORTN IS A CRAYLIB ROUTINE. DOCUMENTATION CAN BE FOUND ON FICHE          
C       NEXT TO THE CONSULTING OFFICE AT NCAR                                   
      CALL FSORTN(1,MAXSDI,1,NDX,MAXSDI,LINK)                                   
C     CALL HDRDMP                                                               
      ISTRT=0                                                                   
   40 ISTRT=ISTRT+1                                                             
      IF(NDX(ISTRT).LT.1) GOTO 40                                               
      NUM=1                                                                     
      NCH=1                                                                     
      NFREQ(NCH)=NDX(ISTRT)                                                     
      NCHANL(NCH)=NUM                                                           
      ISTRT=ISTRT+1                                                             
      DO 50 I=ISTRT,MAXSDI                                                      
         IF(NDX(I).GT.NFREQ(NCH))THEN                                           
            NCH=NCH+1                                                           
            NFREQ(NCH)=NDX(I)                                                   
            NUM=1                                                               
         ELSE                                                                   
            NUM=NUM+1                                                           
         END IF                                                                 
   50    NCHANL(NCH)=NUM                                                        
      DO 60 NN=1,NCH                                                            
         DO 60 I=1,MAXSDI                                                       
            IF(ISDI(I).EQ.NFREQ(NN))THEN                                        
               NDX(I)=NCHANL(NN)                                                
            END IF                                                              
   60 CONTINUE                                                                  
      PRINT 1010,NYEAR,NMON,NDAY,NHR,NMIN,NSEC,(J,NFREQ(J),NCHANL(J),           
     1J=1,NCH)                                                                  
                                                                                
C-----PRINT OUT ENTIRE VECVAR                                                   
      JJ=0                                                                      
      DO 70 I=1,MAXSDI                                                          
         IF (ISDI(I).LT.1) GOTO 70                                              
         JJ=JJ+1                                                                
   70 CONTINUE                                                                  
      CALL ADSUD(NFREQ,NCHANL,NCH,JJ)                                           
C     CALL HDRDMP                                                               
C-----NOW CHECK TO SEE IF A RAW TAPE DUMP IS DESIRED                            
      READ(5,1020) DUMMY                                                        
      READ(5,1030) IRDUMP                                                       
      IF(IRDUMP.NE.1) GOTO 100                                                  
                                                                                
C-----IF SO, CALL THE TAPE DUMP ROUTINE TO GENERATE THE DUMP                    
      CALL RWDPRT(IUNIT)                                                        
      GOTO 100                                                                  
   80 PRINT 1040                                                                
      GOTO 100                                                                  
   90 PRINT 1050                                                                
C                                                                               
C  dump header info to disk for reference by GENSET routine                     
C                                                                               
  100 CONTINUE                                                                  
      END                                                                       
                                                                                
C--------------------------------------------------------------------           
                                                                                
      SUBROUTINE HEADER(NWDS)                                                   
C                                                                               
C     THIS SUBROUTINE WILL DECODE THE HEADER RECORD OF THE INPUT                
C     TAPE AND GENERATE AN OUTPUT FILE TO BE USED AS PART OF A                  
C     GENPROII CALIB  UD.                                                       
C                                                                               
C     HEADER INFORMATION IS DECODED USING THE FOLLOWING HEADER                  
C     CONVENTIONS:                                                              
C                                                                               
C  ARIS5 HEADER INFORMATION   AS OF DEC 15, 1981                                
C                                                                               
C  WORDS ARE 16 BITS, X'S REPRESENT BIT POSTIONS IN WORD                        
C           DASHES ARE INCLUDED FOR READABILITY                                 
C                                                                               
C  WORD     DESCRIPTION                                                         
C    1      NHEAD(1) = -1 (FLAG WORD)                                           
C    2      NHEAD(2) = TAPE LABEL(NUMBER)                                       
C    3      NHEAD(3) = FLT NO.                                                  
C    4      NHEAD(4) = XXX-YEAR00000000   YEAR        = IHEAD(3)                
C    4      NHEAD(4) = 00000000XX-MONTH   MO          = IHEAD(1)                
C    5      NHEAD(5) = XXXX-DAY00000000   DAY         = IHEAD(2)                
C    5      NHEAD(5) = 00000000XXX-HOUR   HR          = IHEAD(4)                
C    6      NHEAD(6) = X-MINUTE00000000   MIN         = IHEAD(5)                
C    6      NHEAD(6) = 00000000X-SECOND   SEC         = IHEAD(6)                
C   7&8     NHEAD(7) , NHEAD(8) = 4 8-BIT ASCII AIRCRAFT NAME                   
C  9&10     NHEAD(9) , NHEAD(10)--4 CHARACTER PROJECT NAME                      
C   11      NHEAD(11)= LENGTH OF FILE HEADER  = 1051                            
C   12      NHEAD(12)= NUMBER OF LOGICAL RECS PER PHYSICAL RECORD               
C   13           (13)= LOGICAL RECORD LENGTH                                    
C 14-20        (14-20)   NOT USED                                               
C 21-36     BLKNAM(1-8)  4 CHAR DATA BLOCK NAMES                                
C 37-44     ISIZE(1-8)   LENGTH OF EACH BLOCK                                   
C 45-52     IFIRST(1-8)  FIRST WORD NUMBER                                      
C 53-60     ILAST(1-8)   LAST WORD NUMBER                                       
C 61        MPHYSCL      MAXIMUM NUMBER OF PHYSICAL CHANNELS (100)              
C 62-261    LOCATE(2,100)                                                       
C                   (1,N) PHYSICAL CHANNEL NUMBER                               
C                   (2,N) RELATIVE ADDRESS OF FIRST SAMPLE                      
C 262       MAXBAS        NUMBER OF BUFFERS (LENGTH OF IBUFBAS)                 
C 263-272   IBUFBAS(1-10) RELATIVE ADDRESSES OF EACH BUFFER START               
C 273       MAXSDI        NUMBER OF SAMPLED CHANNELS POSSIBLE                   
C 274-336   ISDI(1-63)    SAMPLE RATE FOR EACH CHANNEL                          
C 337-588   SDINAM(1-63)  8 CHAR CHANNEL NAME                                   
C 589       MAXANA        MAX NUMBER OF ANALOG CHANNELS                         
C 590-715   C1(1-63)      ZEROTH ORDER CALIBRATION COEFFICIENTS                 
C 716-841   C2(1-63)      FIRST ORDER CALIBRATION COEFFICIENTS                  
C 842-967   C3(1-63)      SECOND ORDER CALIBRATION COEFFICIENTS                 
C 968       NFULL         NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM            
C 969-1031  ITYPE(1-63)   2 CHARACTER SPECIFICATION OF  EITHER                  
C                          ANALOG (AN) OR DIGITAL (DI)                          
C                                                                               
C                                                                               
C  ADS HEADER INFORMATION   AS OF NOV 11, 1983                                  
C                                                                               
C   WORD        DESCRIPTION                                                     
C      1-10  SAME AS ARIS5 HEADER OF DEC 15, 1981                               
C        11  LENGTH OF FILE HEADER = 1655                                       
C     12-61  SAME AS ARIS5 HEADER OF DEC 15, 1981                               
C    62-301  LOCATE(2,120)                                                      
C               -  (1,N) PHYSICAL CHANNEL NUMBER                                
C               -  (2,N) RELATIVE ADDRESS OF FIRST SAMPLE                       
C       302  MAXBAS        NUMBER OF BUFFERS (LENGTH OF IBUFBAS)                
C   303-312  IBUFBAS(1-10) RELATIVE ADDRESSES OF EACH BUFFER START              
C       313  MAXSDI        NUMBER OF SAMPLED CHANNELS POSSIBLE                  
C   314-433  ISDI(1-120)   SAMPLE RATE FOR EACH CHANNEL                         
C   434-793  SDINAM(1-120) 8 CHAR CHANNEL NAME                                  
C       794  MAXANA        MAX NUMBER OF ANALOG CHANNELS                        
C  795-1034  C1(1-120)     ZEROTH ORDER CAL COEFFICIENT                         
C 1035-1274  C2(1-120)     FIRST ORDER CAL COEFFICIENT                          
C 1275-1514  C3(1-120)     SECOND ORDER CAL COEFFICIENT                         
C      1515  NFULL         NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM           
C 1516-1635 ITYPE(1-120)   2 CHARACTER SPECIFICATION OF EITHER                  
C                           ANALOG (AN) OR DIGITAL (DI)                         
C                                                                               
                                                                                
                                                                                
                                                                                
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),                   
     3              NHDR(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,           
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
      DIMENSION NBIT8(720),NHEAD(3000),NCHAR(8),IBUF(10),MONTH(12)              
                                                                                
      DATA MONTH/3HJAN,3HFEB,3HMAR,3HAPR,3HMAY,3HJUN,3HJUL,3HAUG,3HSEP,         
     1           3HOCT,3HNOV,3HDEC/                                             
C                                                                               
C     DESCRIPTION OF VARIABLES:                                                 
C                                                                               
C     C1(120)  -- THE ZEROTH ORDER CALIBRATION COEFFICIENTS                     
C     C2(120)  -- THE FIRST ORDER CALIBRATION COEFFICIENTS                      
C     C3(120)  -- THE SECOND ORDER CALIBRATION COEFFICIENTS                     
C     IBUF(10) -- RELATIVE ADDRESSES OF EACH BUFFER START                       
C     IFIRST   -- FIRST WORD NUMBER                                             
C     ILAST    -- LAST WORD NUMBER                                              
C     ISDI     -- SAMPLE RATE FOR EACH CHANNEL                                  
C     ISIZE    -- LENGTH OF EACH BLOCK                                          
C     ITYPE    -- 2 CHAR SPEC OF ANALOG(AN) OR DIGITAL(DI)                      
C     KPROB    -- NUMBER OF PROBES  (PROBE COUNTER)                             
C     LOCATE(2,120)    (1,N)-- PHYSICAL CHANNEL NUMBERS                         
C                      (2,N)-- RELATIVE ADDRESS OF 1ST SAMPLE                   
C     MAXANA   -- MAX NUMBER OF ANALOG CHANNELS                                 
C     MAXBAS   -- NUMBER OF BUFFERS (LENGTH OF IBUFBAS)                         
C     MAXSDI   -- NUMBER OF SAMPLED CHANNELS POSSIBLE                           
C     MXCHAN   -- MAXIMUM NUMBER OF PHYSICAL CHANNELS (100)                     
C     NFULL    -- NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM                    
C     SDINAM   -- 8 CHAR CHANNEL NAME (DOUBLE PREC)                             
C                                                                               
C                                                                               
C     INITIALIZE NEW HEADER                                                     
C                                                                               
C-----USE GBYTES TO UNPACK THE 16-BIT INFORMATION STORED 4 PER WORD             
C     IN NBUF TO NHEAD (1 PER WORD). NWDS WAS SET BY THE MAIN PROGRAM.          
      CALL GBYTES(NBUF,NHEAD,0 ,16,0,NWDS)                                      
C-----DETERMINE NUMBER OF CHANNELS AND INITILIZE SOME CONSTANTS                 
      IF(NHEAD(273).NE.63) GOTO 10                                              
      ISDISZ=252                                                                
      ILCTSZ=100                                                                
      NBTGB=8                                                                   
      GOTO 30                                                                   
  10  IF(NHEAD(313).NE.120) GOTO 20                                             
      ISDISZ=360                                                                
      ILCTSZ=120                                                                
      NBTGB=6                                                                   
      GOTO 30                                                                   
  20  WRITE(6,1000)                                                             
 1000 FORMAT(2X,'AN ERROR HAS OCCURED IN DETECTING THE SDI BLOCK SIZE!')        
      RETURN                                                                    
                                                                                
C-----GET FLAG WORD = -1                                                        
  30  LOC = 1                                                                   
      ISIGN = 100000B .AND.NHEAD(1)                                             
      IF(ISIGN.NE.0) NHEAD(1)=NHEAD(1)-200000B                                  
      NFLAG = NHEAD(LOC)                                                        
                                                                                
C-----GET TAPE NUMBER                                                           
      LOC = 2                                                                   
      NTAPE = NHEAD(LOC) +9000                                                  
                                                                                
C-----GET FLIGHT NUMBER                                                         
      LOC = 3                                                                   
      NFLT  = NHEAD(LOC )                                                       
C                                                                               
C     TIME OF CREATION  OF THE TAPE                                             
C     WORD 4     XXX-YEAR00000000     -- YEAR                                   
C                00000000XX-MONTH     -- MONTH                                  
C     WORD 5     XXXX-DAY00000000     -- DAY                                    
C                00000000XXX-HOUR     -- HOUR                                   
C     WORD 6     X-MINUTE00000000     -- MINUTE                                 
C                00000000X-SECOND     -- SECOND                                 
C                                                                               
C     4 CHAR AIRCRAFT NAME                                                      
C     WORD 7     XX-CHAR100000000     -- CHAR 1                                 
C                00000000XX-CHAR2     -- CHAR 2                                 
C     WORD 8     XX-CHAR300000000     -- CHAR 3                                 
C                00000000XX-CHAR4     -- CHAR 4                                 
C                                                                               
C     4 CHAR PROJECT NUMBER                                                     
C     WORD 9     XX-CHAR100000000     -- CHAR 1                                 
C                00000000XX-CHAR2     -- CHAR 2                                 
C     WORD 10    XX-CHAR300000000     -- CHAR 3                                 
C                00000000XX-CHAR4     -- CHAR 4                                 
C                                                                               
      LOC = 4                                                                   
      NBSKIP = 16*(LOC-1)                                                       
                                                                                
C-----EXTRACT DATE AND TIME FROM 16-BIT NBUF WORDS INTO 8-BITS PER WORD         
C      NBIT8 AND THEN PRINT THEM                                                
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,14)                                     
      NYEAR = NBIT8(1)                                                          
      NMON  = NBIT8(2)                                                          
      NDAY  = NBIT8(3)                                                          
      NHR   = NBIT8(4)                                                          
      NMIN  = NBIT8(5)                                                          
      NSEC  = NBIT8(6)                                                          
      PRINT 1020,NFLAG,NTAPE,NFLT,NYEAR,NMON,NDAY,NHR,NMIN,NSEC                 
 1020 FORMAT(1H1,'      NFLAG  NTAPE       NFLT    NYEAR    NMON     NDA        
     1Y    NHR      NMIN     NSEC',/,1X,9I9)                                    
                                                                                
C-----EXTRACT AIRCRAFT NAME  AND PRINT IT                                       
      CALL SBYTES(NAIR,NBIT8(7),0,8,0,4)                                        
      PRINT 1030, NAIR                                                          
 1030 FORMAT(/,'  AIRCRAFT NAME  ', A4)                                         
                                                                                
C-----EXTRACT PROJECT NAME AND PRINT IT                                         
      CALL SBYTES(NPRO,NBIT8(11),0,8,0,4)                                       
      PRINT 1040, NPRO                                                          
 1040 FORMAT(/,'  PROJECT NAME (NUMBER) ',A4)                                   
      WRITE(9,1045) NYEAR,NDAY,NMON,NPRO,NAIR,NFLT                              
 1045 FORMAT(3(I2,1X),A4,A4,I4)                                                 
C-----GET LENGTH OF FILE HEADER                                                 
      LENHD = NHEAD(11)                                                         
                                                                                
C-----GET NUMBER LOGICAL RECS PER PHYSICAL REC                                  
      NLREC = NHEAD(12)                                                         
                                                                                
C-----GET LOGICAL RECORD LENGTH                                                 
      LENLOG= NHEAD(13)                                                         
      PRINT 1050,LENHD,NLREC,LENLOG                                             
 1050 FORMAT(/,'  LENGTH OF FILE HEADER IS ',I5,' NO. LOGICAL RECS PER          
     - PHYSICAL REC = ',I5,' LOGICAL RECORD LENGTH = ',I5)                      
                                                                                
C-----EXTRACT 4 CHARACTER DATA BLOCK NAMES AND PAD WITH SPACES                  
      LOC = 21                                                                  
      NBSKIP = 16*(LOC-1)                                                       
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,32)                                     
      IN=0                                                                      
      DO 60 I=1,8                                                               
         DO 40 N=1,4                                                            
           IN=IN+1                                                              
  40     NCHAR(N)=NBIT8(IN)                                                     
                                                                                
C-----FILL REST OF NCHAR WITH SPACES                                            
         DO 50 M=5,8                                                            
  50     NCHAR(M)=40B                                                           
         CALL SBYTES(NAMBLK(I),NCHAR,0,8,0,8)                                   
  60  CONTINUE                                                                  
                                                                                
C-----LENGTH OF EACH BLOCK                                                      
C-----FIRST WORD NUMBER(LOCATION/ADDRESS) OF EACH BLOCK                         
C-----LAST WORD NUMBER(LOCATION/ADDRESS) OF EACH BLOCK                          
      LOCS = 37                                                                 
      LOCF = 45                                                                 
      LOCL = 53                                                                 
      PRINT 1070                                                                
 1070 FORMAT(/,'  BLOCK NAME             ISIZE',7X,'IFIRST',8X,'ILAST')         
      DO 70 I=1,8                                                               
         ISIZE(I) = NHEAD(LOCS+I-1)                                             
         IFIRST(I) = NHEAD(LOCF+I-1)                                            
         ILAST(I) = NHEAD (LOCL+I-1)                                            
         PRINT 1080,NAMBLK(I),ISIZE(I),IFIRST(I),ILAST(I)                       
         WRITE(9,1080) NAMBLK(I),ISIZE(I),IFIRST(I),ILAST(I)                    
 1080 FORMAT( 3X,A8,12X,I5,8X,I5,8X,I5)                                         
  70  CONTINUE                                                                  
                                                                                
C-----MAXIMUM NUMBER OF PHYSICAL CHANNELS                                       
      MXCHAN = NHEAD (61)                                                       
      WRITE(9,1082) MXCHAN                                                      
 1082 FORMAT(I6)                                                                
C-----LOCATE(1,N) ADS OCTAL ADDRESS                                             
C-----      (2,N) OFFSET INTO TAPE BUFFER OF FIRST SAMPLE                       
      LOC = 62                                                                  
      K= 0                                                                      
      KGO=ILCTSZ*2                                                              
      DO 80 I=1,KGO,2                                                           
         K= K+1                                                                 
         LOCATE(1,K)= NHEAD(LOC+I-1)                                            
         LOCATE(2,K) = NHEAD(LOC+I)                                             
  80  CONTINUE                                                                  
 1090 FORMAT(/,' LOCATE(1,K)  ',/,(15I8))                                       
 1100 FORMAT(/,' LOCATE(2,K)  ',/,(15I8))                                       
                                                                                
C-----GET MAXIMUM NUMBER OF BUFFERS                                             
      LOC=LOC+KGO                                                               
      MAXBAS = NHEAD (LOC)                                                      
      WRITE(9,1105) MAXBAS                                                      
 1105 FORMAT(I6)                                                                
                                                                                
C-----GET OFFSET INTO TAPE BUFFER OF EACH LOGICAL RECORD                        
      LOC=LOC+1                                                                 
      DO 90 I=1,10                                                              
         IBUF(I) = NHEAD (LOC+I-1)                                              
  90  CONTINUE                                                                  
      WRITE(9,1106)(IBUF(I),I=1,10)                                             
 1106 FORMAT(10(I6))                                                            
C-----GET NUMBER OF SAMPLED CHANNELS POSSIBLE                                   
      LOC=LOC+10                                                                
      MAXSDI = NHEAD (LOC)                                                      
      WRITE(9,1106) MAXSDI                                                      
C-----GET SAMPLE RATE FOR EACH CHANNEL                                          
      LOC=LOC+1                                                                 
      DO 100 I=1,MAXSDI                                                         
         ISDI(I) = NHEAD(LOC+I-1)                                               
  100 CONTINUE                                                                  
                                                                                
C-----GET 6 CHARACTER CHANNEL NAME                                              
      LOC=LOC+MAXSDI                                                            
      NBSKIP = 16*(LOC-1)                                                       
      ITTS=ISDISZ*2                                                             
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,ITTS)                                   
      IN=0                                                                      
      DO 130 I=1,MAXSDI                                                         
         DO 110 N=1,NBTGB                                                       
            IN=IN+1                                                             
  110    NCHAR(N)=NBIT8(IN)                                                     
                                                                                
C-----NOW PAD REST IF NECESSARY WITH SPACES                                     
         DO 120 M=NBTGB+1,8                                                     
  120    NCHAR(M)=40B                                                           
         CALL SBYTES(NAMSDI(I),NCHAR,0,8,0,8)                                   
  130 CONTINUE                                                                  
                                                                                
C-----THE MAXIMUM NUMBER OF ANALOG CHANNELS = 120                               
      LOC=LOC+ISDISZ                                                            
      MAXANA = NHEAD(LOC)                                                       
      WRITE(9,1106) MAXANA                                                      
C-----GET THE ZEROTH ORDER CALIBRATION COEFFICIENT                              
      LOC=LOC+1                                                                 
      ITTS=MAXSDI*2                                                             
      CALL LSI(NHEAD(LOC),C1,ITTS)                                              
                                                                                
C-----GET THE FIRST ORDER CALIBRATION COEFFICENT                                
      LOC=LOC+ITTS                                                              
      CALL LSI(NHEAD(LOC),C2,ITTS)                                              
                                                                                
C-----GET THE SECOND ORDER CALIBRATION COEFFICIENT                              
      LOC=LOC+ITTS                                                              
      CALL LSI(NHEAD(LOC),C3,ITTS)                                              
                                                                                
C-----READ THE NUMBER OF SAMPLED CHANNELS IN THIS PROGRAM                       
      LOC=LOC+ITTS                                                              
      NFULL = NHEAD(LOC)                                                        
      WRITE(9,135) NFULL                                                        
  135 FORMAT(I6)                                                                
                                                                                
C-----GET A 2 CHARACTER SPECIFICATION OF EITHER ANALOG(AN) OR DIGITAL(DI)       
      LOC=LOC+1                                                                 
      NBSKIP = 16*(LOC-1)                                                       
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,ITTS)                                   
      IN=0                                                                      
      DO 160 I=1,MAXSDI                                                         
         DO 140 N=1,2                                                           
            IN=IN+1                                                             
  140    NCHAR(N)=NBIT8(IN)                                                     
                                                                                
C-----NOW FILL REST OF NCHAR WITH SPACES                                        
         DO 150 M=3,8                                                           
  150    NCHAR(M)=40B                                                           
         CALL SBYTES(ITYPE(I),NCHAR,0,8,0,8)                                    
  160 CONTINUE                                                                  
                                                                                
C-----GET FOUR CHAR PMS-1D PROBE NAME                                           
      LOC=LOC+MAXSDI                                                            
      NBSKIP = 16*(LOC-1)                                                       
      CALL GBYTES(NBUF,NBIT8,NBSKIP,8,0,16)                                     
      IN=0                                                                      
      DO 190 I=1,4                                                              
         DO 170 N=1,4                                                           
            IN=IN+1                                                             
  170    NCHAR(N)=NBIT8(IN)                                                     
                                                                                
C-----AND PAD THE REST OF NCHAR WITH SPACES                                     
         DO 180 M=5,8                                                           
  180    NCHAR(M)=40B                                                           
         CALL SBYTES(NAMPMS(I),NCHAR,0,8,0,8)                                   
  190 CONTINUE                                                                  
                                                                                
C-----GET BLOCK SIZE FOR INDIVIDUAL PROBES                                      
      KPROB=0                                                                   
      LOC=LOC+8                                                                 
                                                                                
C-----LOOP OVER CURRENT MAXIMUM POSSIBLE PMS1 PROBES                            
      DO 200 I=1,4                                                              
         ISZPMS(I) = NHEAD(LOC+I-1)                                             
         IF(ISZPMS(I) .GT. 0) KPROB=KPROB+1                                     
  200 CONTINUE                                                                  
                                                                                
C-----SAMPLE RATE FOR EACH INDIVIDUAL PROBE                                     
      LOC=LOC+4                                                                 
      DO 210 I=1,KPROB                                                          
  210 IRTPMS(I) = NHEAD(LOC+I-1)                                                
                                                                                
C-----OFFSET INTO PMS BLOCK OF FIRST SAMPLE OF EACH INDIVIDUAL PROBE            
      LOC=LOC+4                                                                 
      DO 220 I=1,KPROB                                                          
  220 LOCPMS(I) = NHEAD(LOC+I-1)                                                
      PRINT 1110,KPROB                                                          
 1110 FORMAT(/,' TOTAL NUMBER OF PMS PROBES FOR THIS PROJECT IS',I4,//)         
      PRINT 1120                                                                
 1120 FORMAT(/,' PROBE NO.   PMSNAME   BLKSIZE   SMPL RATE   1ST LOC  ')        
      DO 230 I=1,KPROB                                                          
         PRINT 1130,I,NAMPMS(I),ISZPMS(I),IRTPMS(I),LOCPMS(I)                   
  230 CONTINUE                                                                  
 1130 FORMAT(6X,I4,4X,A4,6X,I4,10X,I4,6X,I4)                                    
      PRINT 1140                                                                
 1140 FORMAT(/,'  CHANNEL NO      INDEX   SMPL RATE      NAME        C1         
     -     C2      C3      ITYPE')                                              
                                                                                
C-----FIX UP PROBE NAMES FOR GENPRO                                             
      DO 92, IJ = 1, KPROB                                                      
         IF (NAMPMS(IJ) .EQ. 8H260X    ) NAMPMS(IJ) = 8HX260                    
         IF (NAMPMS(IJ) .EQ. 8H200Y    ) NAMPMS(IJ) = 8HY200                    
         IF (NAMPMS(IJ) .EQ. 8H200X    ) NAMPMS(IJ) = 8HX200                    
   92 CONTINUE                                                                  
                                                                                
      WRITE(9,1132) KPROB                                                       
      DO 231 I=1,KPROB                                                          
         WRITE(9,1131) NAMPMS(I),ISZPMS(I),IRTPMS(I),LOCPMS(I)                  
  231 CONTINUE                                                                  
 1132 FORMAT(I6)                                                                
 1131 FORMAT(A4,6X,I4,10X,I4,6X,I4)                                             
      WRITE(7,1160)                                                             
      DO 240 I=1,MAXSDI                                                         
      IF(ISDI(I).LE.0) GOTO 240                                                 
      PRINT 1150, LOCATE(1,I),LOCATE(2,I),ISDI(I),NAMSDI(I),C1(I),C2(I),        
     -C3(I),ITYPE(I)                                                            
      WRITE(9,1151)ISDI(I),NAMSDI(I),C1(I),C2(I)                                
     -,C3(I),ITYPE(I)                                                           
C-----ELIMINATE IDENTITY COEFFICIENTS(0,1,0)                                    
      IF ((ABS(C1(I)) .LE. .000001) .AND.(ABS(C2(I) - 1.0) .LE. .000001)        
     -.AND. (ABS(C3(I)) .LE. .000001)) GOTO 240                                 
      WRITE(7,1170) C1(I),C2(I),C3(I),NDAY,MONTH(NMON),NYEAR,NAMSDI(I)          
 1150 FORMAT(I10,I11,9X,I3,4X,A8,3(2X,F8.3),4X,A2)                              
 1151 FORMAT(I3,4X,A8,3(2X,F13.5),4X,A2)                                        
 1160 FORMAT(1X,'ORDVAR =NCFABV ,          COEFS              ,CALDATE')        
 1170 FORMAT('LETVAR=3,[',F13.5,',',F13.5,',',F13.5,'],''',I2,A3,I2,            
     1''',%FOR,',A8)                                                            
  240 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
C--------------------------------------------------------------------           
                                                                                
      SUBROUTINE ADSUD(NFREQ,NCHANL,NCH,NUMSDI)                                 
C                                                                               
C        SUBROUTINE ADSUD WILL GENERATE  A FILE THAT CAN BE INSERTED            
C    INTO A GENNPRO-II INPUT UD TO READ IN THIS TAPE.                           
C                                                                               
C                                                                               
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),DUMBK1(120),DUMBK2(120),          
     2              DUMBK3(120),NFULL,ITYPE(120),LENLOG,NLREC,                  
     2              KDUM2(2000),                                                
     3              NHEAD(20),KDUM4,KDUM5,KDUM6(10),MAXSDI,KDUM7,               
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
C                                                                               
      DIMENSION NAMHDR(11),NAMINS(11),NPLUS(11),NFREQ(1),NCHANL(1),             
     1NMPMS2(4),SCALE(120),NAMHSK(50),NMCPIT(5),COM(6)                          
C                                                                               
      CHARACTER COM                                                             
C                                                                               
      DATA NMLORN/8HLRNC    /                                                   
      DATA NAMDME/8HDME     /                                                   
      DATA NAMHDR/8HIDWD    ,8HHR      ,8HMIN     ,8HSEC     ,                  
     1            8HSTEF    ,8HFTER    ,8HINST    ,8HRSWD    ,                  
     2            8HBECT    ,8HTECT    ,8HSYST    /                             
      DATA NAMINS/8HTSEC    ,8HTHNDS   ,8HALAT    ,8HALON    ,                  
     1            8HTHI     ,8HALPHA   ,8HXVI1    ,8HXVI2    ,                  
     2            8HYVI1    ,8HYVI2    ,8HGSI     /                             
      DATA NPLUS/0,16,34,930,162,130,66,194,98,226,706  /                       
      DATA NMPMS2/8HAUX1    ,8HAUX2    ,8HAUX3    ,8HAUX4    /                  
      DATA NAMHSK/8HV10     ,8HV10R    ,8HTADS    ,8HTV10    ,                  
     1     8HFLOADS  ,8HFZV     ,8HFZVR    ,8HVDREF   ,8HXIDICE  ,              
     2     8HXI400U  ,8HXI60U   ,8HXI28U   ,8HXI400D  ,8HXI60D   ,              
     3     8HXI28D   ,8HXILGEN  ,8HXIRGEN  ,8HXIEXT   ,8HVP15D   ,              
     4     8HV28     ,8HVP15A   ,8HVM15A   ,8HTCBADS  ,8HFCBADS  ,              
     5     8HSP1A    ,8HSP2A    ,8HSP3A    ,8HSP4A    ,8HSP5A    ,              
     6     8HSP6A    ,8HCCKPIT  ,8HHCPY1   ,8HEV1     ,8HHPCY2   ,              
     7     8HEV2     ,8HHCPY3   ,8HEV3     ,8HHCPY4   ,8HEV4     ,              
     8     8HHCPY5   ,8HTA2D    ,8HTS2D    ,8HTLSI    ,8HSP1D    ,              
     9     8HSP2D    ,8HSP3D    ,8HSP4D    ,8HSP5D    ,8HSP6D    ,              
     A     8HSP7D    /                                                          
      DATA NMCPIT/8HCMFMC   ,8HDISW    ,8HSQSW    ,8HCAMESW  ,                  
     1            8HCKEVP1  /                                                   
      INTEGER FSTBIT,BITS,BTSKIP,RATES,DUMMY,CONKEY                             
      INTEGER TNAME(396)                                                        
 1000 FORMAT(/,' ORDVAR = FSTBIT,BITS,SKIP, SAMPLE,CONKEY,TERM,   FACTOR        
     1 ')                                                                       
 1010 FORMAT(' ORDGEN = LOGBIT,  DATLOG,  DATSIZ,  NAMKEY,  BITKEY')            
 1020 FORMAT(' LETGEN = ',I6,',',3X,I4,',',3X,I6,',    IDWD ,  34433',/)        
 1070 FORMAT(' VECVAR = ',6(1X,A8,','))                                         
 1075 FORMAT(11X,'LORN    , CSTAT   ,')                                         
 1080 FORMAT((10X,6(1X,A8,',')))                                                
 1085 FORMAT((10X,6(1X,A8,A1)))                                                 
 1090 FORMAT(' LETVAR = ',I6,',',I4,',',I5,',',I4,',',I3,',   0.0,',            
     1F13.7,', %FOR,',2X,A8)                                                    
C                                                                               
C     VARIABLE DESCRIPTIONS USED IN ADSUD                                       
C                                                                               
C     BITS(215)   --NUMBER OF BITS FOR EACH VARIABLE                            
C     BTSKIP(215) --NUMBER OF BITS TO SKIP FOR EACH VARIABLE                    
C     CONKEY(215)  CONVERSION KEY FOR INPUT OPERATION UD                        
C           = 1  NO CONVERSION REQUIRED                                         
C           = 2  CONVERT THE BIT STRING WITH THE MOST SIGNIFICANT BIT           
C                AS A SIGN BIT ( 0 = +, 1 = -)                                  
C           = 6  INVERT THE 1'S COMPLEMENT BIT STRING WITH .NOT..               
C           = 7  INVERT THE 1'S COMPLEMENT BIT STRING WITH .NOT., THEN          
C                CONVERT IT AS CONKEY=2.                                        
C     DUMBK  --(1-3) UNUSED VARIABLES USED TO HOLD POSITION IN COMMON BLK       
C     FACTOR(215) -- SCALING FACTOR FOR ALL VARIABLES                           
C     FSTBIT(215) -- FIRST BIT LOCATION FOR ALL VARIABLES                       
C     IFIRST(8)   -- FIRST WORD NUMBER                                          
C     II,I,ITER,IJ,NO,NCH,IL -- INDEX'S FOR ARRAYS                              
C     INDEX       -- BEGINNING BIT LOCATION FOR INS & PMS BLOCK                 
C     ISDI(120)   -- SAMPLE RATE FOR EACH CHANNEL                               
C     ITYPE(120)  -- 2 CHAR SPEC OF ANALOG(AN) OR DIGITAL(DI)                   
C     KDUM  --(0-7) UNUSED VARIABLES USED TO HOLD POSITION IN COMMON BLK        
C     LENLOG      -- LOGICAL RECORD LENGTH                                      
C     LOCATE(2,120) -- (1,N) PHYSICAL CHANNEL NUMBER                            
C                      (2,N) RELATIVE ADDRESS OF FIRST SAMPLE                   
C     LOCIN        -- BEGINNING LOCATION IN BITS OF A BLOCK (I.E.SDI,INS)       
C     MAXSDI       -- MAXIMUM NUMBER OF SAMPLED CHANNELS POSSIBLE               
C     NAMBLK(8)    -- 4 CHARACTER DATA BLOCK NAMES                              
C     NAMES(215)   -- NAMES FOR ALL VARIABLES                                   
C     NAMHDR(11)   -- 8H CHARACTER HEADER NAME                                  
C     NAMINS(11)   -- 8H CHARACTER INS NAME                                     
C     NAMSDI(120)  -- 8 CHAR CHANNEL NAME(DOUBLE PREC-4 4 CHAR NAME EACH)       
C     NMPMS2(4)    -- 8H CHARACTER PMS2D NAME                                   
C     NCHANL(20)   -- NUMBER OF CHANNELS AT A SAMPLING RATE                     
C     NFREQ(20)    -- SAMPLING RATE                                             
C     NLREC        -- LOGICAL RECORDS PER PHYSICAL RECORD                       
C     NPLUS(11)    -- ADJUSTED STARTING BIT LOCATION FOR INS BLOCK              
C     RATES(215)   -- SAMPLE RATES FOR ALL VARIABLES                            
C     SCALE(120)   -- SCALE FACTORS FOR SDI BLOCK                               
C                                                                               
C------THE FOLLOWING SETS GAP INFO                                              
      CONKEY(1) = 1                                                             
      FSTBIT(1) = 1                                                             
      BITS(1) = 1                                                               
      BTSKIP(1) = 0                                                             
      RATES(1) = 1                                                              
      FACTOR(1) = 1.0                                                           
      NAMES(1) = 8HGAP                                                          
                                                                                
                                                                                
C-----THE FOLLOWING PRINTS OUT ALL PERTINANT HEADER INFORMATION.                
      DO 10 II=2,12                                                             
         CONKEY(II)=1                                                           
         FSTBIT(II)=((II-2)*16)+1                                               
         BITS(II)=16                                                            
         BTSKIP(II)=0                                                           
         RATES(II)=1                                                            
         FACTOR(II)=1.0                                                         
10       NAMES(II)=NAMHDR(II-1)                                                 
      II=II-1                                                                   
                                                                                
C-----SET SDI SCALING                                                           
      DO 20 I=1,MAXSDI                                                          
                                                                                
C-----CHECK IF DATA IS DIGITAL OR ANALOG, THEN SET SCALE ACCORDINGLY.           
         IF(ITYPE(I).EQ.8HAN      ) SCALE(I)=0.001223                   ANALOG  
  20     IF(ITYPE(I).EQ.2HDI      ) SCALE(I)=0.0219726                  DIGITAL 
C-----THE FOLLOWING CALCULATES OUT ALL PERTINANT INS UD DATA.                   
      DO 260 ITER=2,8                                                           
         IF(NAMBLK(ITER).NE.8HINS     ) GOTO 50                                 
         LOCIN=IFIRST(ITER)                                                     
         DO 40 IJ = 1,11                                                        
            II=II+1                                                             
            FACTOR(II)=1.0                                                      
            BITS(II)=16                                                         
            BTSKIP(II)=0                                                        
            RATES(II)=1                                                         
            FSTBIT(II)=LOCIN*16-15+NPLUS(IJ)                                    
            IF(IJ .GT. 4) THEN                                                  
               BTSKIP(II)=206                                                   
               RATES(II)=5                                                      
            ENDIF                                                               
            CONKEY(II)=1                                                        
            IF(IJ .GT. 2) THEN                                                  
               CONKEY(II)=2                                                     
               BITS(II)=18                                                      
                                                                                
C-----CONVERSION FACTOR FOR XVI1,XVI2,YVI1,YVI2                                 
               FACTOR(II)=26.2567192                                            
               IF( IJ.LE.6) FACTOR(II)=728.1777861                              
               IF(IJ .EQ. 11) THEN                                              
                  BTSKIP(II)=0                                                  
                  RATES(II)=1                                                   
                  FACTOR(II)=1.9425019                                          
               ENDIF                                                            
            ENDIF                                                               
            NAMES(II)=NAMINS(IJ)                                                
  40     CONTINUE                                                               
         GO TO 260                                                              
                                                                                
C-----THIS SECTION CALCULATES SDI INFORMATION                                   
  50     IF(NAMBLK(ITER).NE.8HSDI     ) GOTO 90                                 
         DO 80 I=1,NUMSDI                                                       
            II=II+1                                                             
            NAMES(II)=NAMSDI(I)                                                 
            RATES(II)=ISDI(I)                                                   
            IF(ISDI(I).GE.1)THEN                                                
               CONKEY(II)=2                                                     
               BITS(II)=16                                                      
               FACTOR(II)=1./SCALE(I)                                           
               FSTBIT(II)=LOCATE(2,I)*16-15                                     
            IF(ISDI(I).GT.1) THEN                                               
               DO 60 NO = 1,NCH                                                 
                IF(NFREQ(NO).EQ.ISDI(I))BTSKIP(II)=NCHANL(NO)*16-16             
  60           CONTINUE                                                         
            ELSE                                                                
               BTSKIP(II)=0                                                     
            END IF                                                              
            IF((NAMSDI(I).EQ.8HPITCH   ).OR.(NAMSDI(I).EQ.8HROLL    ).OR        
     1      .(NAMSDI(I).EQ.8HPHDG    )) THEN                                    
               FACTOR(II)=FACTOR(II)*8.                                         
               CONKEY(II)=7                                                     
            END IF                                                              
            IF((NAMSDI(I).EQ.8HCROLL   ).OR.(NAMSDI(I).EQ.8HCHGME   ).OR        
     1      .(NAMSDI(I).EQ.8HHGME    )) CONKEY(II)=7                            
            IF(NAMSDI(I).EQ.8HHGME    ) FACTOR(II)=FACTOR(II)*25.               
            IF(NAMSDI(I) .EQ.8HPSFD    ) THEN                                   
               FACTOR(II)=1./0.033864                                           
               CONKEY(II)=7                                                     
C              CONKEY(II)=6                                                     
            END IF                                                              
            IF(NAMSDI(I) .EQ.8HVZI     ) FACTOR(II)=82.020997                   
         ENDIF                                                                  
  80     CONTINUE                                                               
         GO TO 260                                                              
C                                                                               
C----------------------HOUSE KEEPING BLOCK------------------------              
C                                                                               
  90     IF(NAMBLK(ITER).NE.8HHSKP    ) GOTO 120                                
         INDEX=IFIRST(ITER)                                                     
         FSTBIT(II+1)=INDEX*16-15                                               
         DO 110 IL=1,50                                                         
            II=II+1                                                             
            BTSKIP(II)=0                                                        
            RATES(II)=1                                                         
            NAMES(II)=NAMHSK(IL)                                                
C-----HOUSE KEEPING BLOCK HAS THE SAME ANALOG CONVERSION FACTOR AS SDI          
            FACTOR(II)=1.0                                                      
            CONKEY(II)=2                                                        
            BITS(II)=16                                                         
            IF(IL.LE.30) FACTOR(II)=1./0.001223                                 
            IF(IL.GE.3 .AND. IL.LE.5) FACTOR(II)=FACTOR(II)/5.                  
            IF(IL.EQ.23 .OR. IL.EQ.24) FACTOR(II)=FACTOR(II)/5.                 
            IF(IL.EQ.19 .OR. IL.EQ.21 .OR. IL.EQ.22) FACTOR(II)=                
     1      FACTOR(II)/3                                                        
            IF(IL.EQ.20) FACTOR(II)=FACTOR(II)/6.2                              
C-----EV1 CORRECTION                                                            
C           IF(IL.EQ.33) FSTBIT(II)=FSTBIT(II)+8                                
C           IF(IL.EQ.33) BITS(II)=8                                             
C           IF(IL.EQ.34) FSTBIT(II)=FSTBIT(II)-8                                
C                                                                               
C** HOUSE KEEPING BLOCK   ---------- WORD 31 CCKPIT                             
C        /   /   /   /   /   /   /   /   /   /   /   /   /   /   /   /          
C BIT-- 15  14  13  12  11  10  09  08  07  06  05  04  03  02  01  00          
C ORDER  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16          
C                    *   *   *               *  **********************          
C                         COCKPIT EVENT (BITS 0-5)                              
C                         CAMERA SWITCH (BITS 6)                                
C                         SQUAT SWITCH  (BIT 10)                                
C                         DEICE SWITCH  (BIT 11)                                
C                         CAMERA INTERVAL (BIT 12)                              
C                                                                               
            IF( IL.EQ. 31) THEN                                                 
               CONKEY(II)=1                                                     
               FACTOR(II)=1.0                                                   
C-----TO PRINT THE BIT LOCATION FOR THE WHOLE WORD                              
C-----TO PRINT OUT BIT 12, 11, 10, 6, AND BITS 0-5                              
               DO 100 I=1,5                                                     
                 II=II+1                                                        
                 BITS(II)=1                                                     
                 IF(I.EQ.5) BITS(II)=6                                          
                 IF(I.EQ.1) FSTBIT(II)=FSTBIT(II-1) +3                          
                 IF(I.EQ.2.OR.I.EQ.3.OR.I.EQ.5) FSTBIT(II)=FSTBIT(II-1)+        
     1           1                                                              
                 IF(I.EQ.4) FSTBIT(II)=FSTBIT(II-1)+4                           
                 BTSKIP(II)=0                                                   
                 RATES(II)=1                                                    
                 CONKEY(II)=1                                                   
                 FACTOR(II)=1.0                                                 
                 NAMES(II)=NMCPIT(I)                                            
 100           CONTINUE                                                         
               FSTBIT(II+1)=FSTBIT(II)+6                                        
               GOTO 110                                                         
            END IF                                                              
            IF(IL.NE.31) THEN                                                   
            END IF                                                              
            FSTBIT(II+1)=FSTBIT(II)+16                                          
 110     CONTINUE                                                               
         GO TO 260                                                              
C                                                                               
C-----PMS BLOCK       (PMS1)                                                    
C     PICKING UP THE WHOLE BLOCK AS ONE PARAMETER NAMED PMSBLOCK                
C                                                                               
 120     IF(NAMBLK(ITER).NE.8HPMS1    ) GOTO 140                                
         INDEX=IFIRST(ITER)                                                     
         DO 130, IP=1,KPROB                                                     
            IF (IP .GT. 1) INDEX = INDEX + ISZPMS(IP-1)                         
            II=II+1                                                             
            CONKEY(II)=2                                                        
            BITS(II)=16                                                         
            FSTBIT(II)=INDEX*16-15                                              
            FACTOR(II)=1.0                                                      
            BTSKIP(II)=0                                                        
            RATES(II)=ISZPMS(IP)                                                
            NAMES(II)=NAMPMS(IP)                                                
 130     CONTINUE                                                               
         GO TO 260                                                              
C                                                                               
C-----PMS BLOCK       (PMS2D AUXILARY WORDS)                                    
C                                                                               
 140     IF(NAMBLK(ITER).NE.8HPMS2    ) GOTO 160                                
         INDEX=IFIRST(ITER)                                                     
         FSTBIT(II+1)=INDEX*16-15                                               
         DO 150 IL=1,4                                                          
            II=II+1                                                             
            CONKEY(II)=2                                                        
            BITS(II)=16                                                         
            FACTOR(II)=1.0                                                      
            BTSKIP(II)=0                                                        
            RATES(II)=1                                                         
            NAMES(II)=NMPMS2(IL)                                                
            FSTBIT(II+1)=FSTBIT(II)+16                                          
 150     CONTINUE                                                               
C                                                                               
C-----DME BLOCK                                                                 
C                                                                               
 160     IF(NAMBLK(ITER).NE.8HDME     ) GOTO 180                                
         INDEX=IFIRST(ITER)                                                     
         FSTBIT(II+1)=INDEX*16-15                                               
         II=II+1                                                                
         CONKEY(II)=0                                                           
         BITS(II)=16                                                            
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
         RATES(II)=3                                                            
         NAMES(II)=NAMDME                                                       
C                                                                               
C-----LORAN-C BLOCK                                                             
C     1) CSTAT  12+XL      4    1      0                                        
C     2) CSEC   16+XL     16    1      1                                        
C     3) CFSEC  32+XL     16    1      1                                        
C     4) LORN   48+XL      8   64      0                                        
C----- WHERE XL IS THE FIRST BIT LOCATION OF LORAN-C BLOCK                      
C                                                                               
 180     IF(NAMBLK(ITER).NE.NMLORN    ) GOTO 260                                
         INDEX=IFIRST(ITER)                                                     
         FSTBIT(II+1)=INDEX*16-15                                               
         II=II+1                                                                
         CONKEY(II)=0                                                           
         BITS(II)=16                                                            
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
C        RATES(II)= 91                                                          
         NAMES(II)=NAMBLK(ITER)                                                 
C                                                                               
C---- 1) CSTAT  12+XL      4    1      0                                        
         II=II+1                                                                
         FSTBIT(II)=(FSTBIT(II-1)+12)                                           
         CONKEY(II)=0                                                           
         BITS(II)=4                                                             
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
         RATES(II)= 1                                                           
         NAMES(II)= 8HCSTAT                                                     
C---- 2) CSEC   16+XL     16    1      1                                        
         II=II+1                                                                
         FSTBIT(II)=(FSTBIT(II-1)+4 )                                           
         CONKEY(II)=1                                                           
         BITS(II)= 16                                                           
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
         RATES(II)= 1                                                           
         NAMES(II)= 8HCSEC                                                      
C---- 3) CFSEC  32+XL     16    1      1                                        
         II=II+1                                                                
         FSTBIT(II)=(FSTBIT(II-1)+16)                                           
         CONKEY(II)=1                                                           
         BITS(II)= 16                                                           
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
         RATES(II)= 1                                                           
         NAMES(II)= 8HCFSEC                                                     
C---- 4) LORN   48+XL      8   64      0                                        
         II=II+1                                                                
         FSTBIT(II)=(FSTBIT(II-1)+16)                                           
         CONKEY(II)=0                                                           
         BITS(II)= 8                                                            
         FACTOR(II)=1.0                                                         
         BTSKIP(II)=0                                                           
         RATES(II)= 64                                                          
         NAMES(II)= 8HLORN                                                      
 260  CONTINUE                                                                  
                                                                                
       IJJ = 0                                                                  
       DO 101, JJI = 1, II                                                      
         IF (NAMES(JJI) .NE. 8HLRNC    ) THEN                                   
            IJJ = IJJ + 1                                                       
            TNAME(IJJ) = NAMES(JJI)                                             
         ENDIF                                                                  
  101  CONTINUE                                                                 
                                                                                
      DO 11,KJ = 1,6                                                            
          COM(KJ) = ','                                                         
   11 CONTINUE                                                                  
      JK = INT(FLOAT(IJJ)/6.0)                                                  
      JK = JK * 6                                                               
      IF (ABS(IJJ-JK) .LE. .00001) JK = IJJ - 6                                 
      COM(IJJ-JK) = ' '                                                         
      LGBT=LENLOG*16                                                            
      IDTSZ=LGBT*NLREC                                                          
      PRINT 1010                                                                
      PRINT 1020,LGBT,NLREC,IDTSZ                                               
      PRINT 1070,(TNAME(J),J=1,6)                                               
      PRINT 1080,(TNAME(J),J=7,JK)                                              
      PRINT 1085,(TNAME(J),COM(J-JK),J= JK + 1,IJJ)                             
      PRINT 1000                                                                
      WRITE(8,1010)                                                             
      WRITE(8,1020) LGBT,NLREC,IDTSZ                                            
      WRITE(8,1070)(TNAME(J),J=1,6)                                             
      WRITE(8,1080)(TNAME(J),J=7,JK)                                            
      WRITE(8,1085)(TNAME(J),COM(J-JK),J=JK+1,IJJ)                              
      WRITE(8,1000)                                                             
      DO 21,J=1,II                                                              
         PRINT 1090,FSTBIT(J),BITS(J),BTSKIP(J),RATES(J),CONKEY(J),             
     1   FACTOR(J),NAMES(J)                                                     
         WRITE(8,1090)FSTBIT(J),BITS(J),BTSKIP(J),RATES(J),CONKEY(J),           
     1   FACTOR(J),NAMES(J)                                                     
   21 CONTINUE                                                                  
      KR=II                                                                     
      CALL PTRATE(RATES,NAMES,KR)                                               
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------          
                                                                                
      SUBROUTINE PTRATE(IRATES,NAMES,II)                                        
                                                                                
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),                   
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
                                                                                
      DIMENSION IRATES(215),NAMES(215),IRATS(120),NAMRAT(120),BOB(120),         
     1          COM(4),NAMPMT(4)                                                
      CHARACTER BOB,COM                                                         
                                                                                
                                                                                
 1000 FORMAT(1X,'RATE = ',I3,',%FOR,')                                          
 1010 FORMAT((9X,6(A8,A1)))                                                     
 1020 FORMAT(1X,'SCLKEY = 2')                                                   
 1030 FORMAT(1X,'SCLKEY = 1,%FOR,',4(A8,A1))                                    
 1040 FORMAT(11X,'LORN    ,CSTAT',A1)                                           
 1050 FORMAT (11X,'DME     ')                                                   
                                                                                
      INDEX=0                                                                   
      DO 10 I=1,II                                                              
         IF(IRATES(I).LE.1) GOTO 10                                             
         INDEX=INDEX+1                                                          
         IRATS(INDEX)=IRATES(I)                                                 
         NAMRAT(INDEX)=NAMES(I)                                                 
  10  CONTINUE                                                                  
      IEND=INDEX-1                                                              
C-----SORT BY RATE                                                              
      DO 30 I=1,IEND                                                            
         JJ=I+1                                                                 
         DO 20 J=JJ,INDEX                                                       
            IF(IRATS(I).LE.IRATS(J)) GOTO 20                                    
            ISWAP=IRATS(I)                                                      
            ISWPNM=NAMRAT(I)                                                    
            IRATS(I)=IRATS(J)                                                   
            NAMRAT(I)=NAMRAT(J)                                                 
            IRATS(J)=ISWAP                                                      
            NAMRAT(J)=ISWPNM                                                    
  20     CONTINUE                                                               
  30  CONTINUE                                                                  
      IBEG=1                                                                    
      DO 40 I=1,INDEX                                                           
         IF(IRATS(IBEG).EQ.IRATS(I+1)) GOTO 40                                  
         IEND=I                                                                 
         DO 60 K=IBEG,IEND                                                      
            BOB(K) = ','                                                        
   60    CONTINUE                                                               
         BOB(IEND) = ' '                                                        
         BOB(IBEG+35) = ' '                                                     
 1234    PRINT 1000,IRATS(IBEG)                                                 
         WRITE(8,1000) IRATS(IBEG)                                              
         IF ((IEND-IBEG) .LT. 37) THEN                                          
            PRINT 1010,(NAMRAT(K),BOB(K),K=IBEG,IEND)                           
            WRITE(8,1010) (NAMRAT(K),BOB(K),K=IBEG,IEND)                        
            IBEG=I+1                                                            
         ELSE                                                                   
            PRINT 1010,(NAMRAT(K),BOB(K),K=IBEG,IBEG+35)                        
            WRITE(8,1010) (NAMRAT(K),BOB(K),K=IBEG,IBEG+35)                     
            IBEG=IBEG+36                                                        
            GOTO 1234                                                           
         ENDIF                                                                  
  40  CONTINUE                                                                  
      PRINT 1020                                                                
      WRITE(8,1020)                                                             
      JK = 0                                                                    
      DO 70, IJ = 1, 4                                                          
         COM(IJ) = ','                                                          
   70 CONTINUE                                                                  
C-----SET FLAGS IF THERE IS LORAN OR DME DATA                                   
      DFLAG = 0                                                                 
      LFLAG = 0                                                                 
      XFLAG = 0                                                                 
      DO 82, IJ = 1, 215                                                        
         IF (NAMES(IJ) .EQ. 8HLORN    ) LFLAG = 1                               
         IF (NAMES(IJ) .EQ. 8HDME     ) DFLAG = 1                               
   82 CONTINUE                                                                  
      IF ((LFLAG .EQ. 1) .OR. DFLAG .EQ. 1) XFLAG = 1                           
      IF (XFLAG .EQ. 0) COM(KPROB) = ' '                                        
      PRINT 1030,(NAMPMS(IJ),COM(IJ),IJ=1,KPROB)                                
      WRITE(8,1030) (NAMPMS(IJ),COM(IJ),IJ=1,KPROB)                             
      COM(1) = ' '                                                              
      IF (LFLAG .EQ. 1) THEN                                                    
         IF (DFLAG .EQ. 1) COM(1) = ','                                         
         PRINT 1040,COM(1)                                                      
         WRITE(8,1040)COM(1)                                                    
      ENDIF                                                                     
      IF (DFLAG .EQ. 1) THEN                                                    
         PRINT 1050                                                             
         WRITE(8,1050)                                                          
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
C--------------------------------------------------------------------           
      SUBROUTINE LSI(NPAC,A,NUM)                                                
C                                                                               
C     SUBROUTINE LSI CONVERTS 32 BIT FLOATING POINT NUMBERS ENCODED             
C     BY THE HP GROUND STATION INTO WORDS SUITABLE FOR THE CRAY                 
                                                                                
C----------------- ** AS OF AUGUST 26,1985 (CC) ** --------------------         
C                                                                               
C**     H P - 1 0 0 0    R E A L   F O R M A T  ( 32 BITS )                     
C                                                                               
C----------                                                                     
C BIT N0.    15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00                    
C WORD 1      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |                    
C                                                                               
C   WHERE  BIT    15  IS SIGN OF FRACTION (0-POSITIVE;1-NEGATIVE)               
C          BITS 0-14 ARE FRACTION BITS                                          
C NOTE: 1'S COMPLIMENT FRACTION FOR NEGATIVE SIGN OF FRACTION                   
C----------                                                                     
C BIT N0.    15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00                    
C WORD 2      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |                    
C                                                                               
C   WHERE  BIT   00   IS SIGN OF EXPONENT (0-POSITIVE;1-NEGATIVE)               
C          BITS 1-7  ARE EXPONENT BITS                                          
C          BITS 8-15 ARE FRACTION BITS                                          
C NOTE: 2'S COMPLIMENT EXPONENT FOR NEGATIVE SIGN OF EXPONENT                   
C----------------------------------------------------------------------         
C                                                                               
      DIMENSION NPAC(1),A(1)                                                    
      I=0                                                                       
      DO 100 N=1,NUM,2                                                          
         I= I +1                                                                
C** PICK SIGN OF FRACTION BIT                                                   
         ISIGN = 100000B .AND.NPAC(N)                                           
         IWD1 = NPAC(N)                                                         
         IWD2 = NPAC(N+1)                                                       
C** SAVE ORIGINAL WORD 2 FOR DECODING EXPONENT WORD                             
         IWD2S=IWD2                                                             
         IF(ISIGN.NE.0) THEN                                                    
C** NEGATIVE SIGN FRACTION -- 1'S COMPLIMENT                                    
            IWD1 = .NOT. IWD1                                                   
            IWD2 = .NOT. IWD2                                                   
         END IF                                                                 
         NFRAC1 = IWD1 .AND. 77777B                                             
         NFRAC2 = NFRAC1*256                                                    
         NFRAC3 = IWD2 .AND.177400B                                             
         NFRAC3 = NFRAC3/256                                                    
         NFRAC = NFRAC2 .OR. NFRAC3                                             
C** PICK UP SIGN OF EXPONENT BIT FROM THE SAVED WORD 2                          
         NXSIGN = IWD2S .AND. 1B                                                
         IF(NXSIGN.EQ.0) THEN                                                   
C** POSITIVE SIGN OF EXPONENT                                                   
            NEXP = IWD2S .AND. 376B                                             
            NEXP = NEXP/2                                                       
         END IF                                                                 
         IF(NXSIGN.NE.0 ) THEN                                                  
C** NEGATIVE SIGN OF EXPONENT -- 2'S COMPLIMENT                                 
            IWD2 = .NOT. IWD2S                                                  
            NEXP = IWD2 .AND. 376B                                              
            NEXP =(NEXP/2) +1                                                   
            NEXP = -NEXP                                                        
         END IF                                                                 
         FRAC = NFRAC* 1.192092896E-7                                           
         A(I) = 2.**NEXP *(FRAC)                                                
         IF(ISIGN.NE.0) A(I)=-A(I)                                              
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
C--------------------------------------------------------------------           
                                                                                
      SUBROUTINE RWDPRT(IUNIT)                                                  
                                                                                
C     SUBROUTINE RWDPRT GENERATES A PRINTOUT OF THE RAW DATA TAPE               
C     ACCORDING TO THE SPECIFICATIONS SET BY THE USER AT THE BOTTOM             
C     OF THIS DECK.                                                             
C                                                                               
C                                                                               
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              NFULL,ITYPE(120),LENLOG,NLREC,NBUF(2000),                   
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),        
     1              BEGREC(60)                                                  
      DIMENSION NAMDMP(200),IDATA(250),ITIME(5)                                 
      INTEGER DUMMY,NAMDMP,SPREC,STREC,TIMEST,TIMESP,FSTBIT,BTSKIP,             
     1        BITS,RATES,TIME,CONKEY,BEGREC                                     
      REAL MINUTE                                                               
      DATA NMLORN/8HLRNC    /                                                   
C                                                                               
C-----BECAUSE THE LORAN-C DATA IS ENCODED AS 8 BIT ASCII WHILE THE              
C     REST OF THE DATA IS IN PACKED INTEGER, IT CANNOT BE STORED                
C     IN THE MINUTE ARRAY AS A FLOATING POINT NUMBER. INSTEAD, A                
C     SEPARATE INTEGER ARRAY IS SET UP AND EQUIVALENCED TO THE                  
C     MINUTE ARRAY SO THAT THE INFORMATION IS STORED IN ITS                     
C     ORIGINAL FORM.                                                            
      INTEGER IMINUTE(60,5000)                                                  
      EQUIVALENCE (MINUTE, IMINUTE )                                            
                                                                                
      LOGICAL INULL                                                             
                                                                                
 1000 FORMAT(' EOF SENSED ON DATA FILE REC. NO. ',I8)                           
 1010 FORMAT(' EOT SENSED ON DATA FILE REC. NO. ',I8)                           
 1020 FORMAT(' PARITY ERROR SENSED ON DATA FILE, REC. NO.',I8)                  
 1030 FORMAT(' NO MATCH WAS FOUND FOR RAWDUMP INPUT FILE VARIABLES')            
 1040 FORMAT(1H1)                                                               
 1050 FORMAT(10A8)                                                              
 1070 FORMAT(8I5)                                                               
 1080 FORMAT(8(1X,A8))                                                          
 1090 FORMAT(1X,' CONKEY=',I2,' IS NOT DEFINED IN CODE!')                       
C                                                                               
C     VARIABLE DESCRIPTIONS USED IN SUBROUTINE TAPE DUMP PRINT OUT              
C                                                                               
C     IDATA       -- ARRAY WHICH HOLD GBYTED NBUF ARRAY                         
C     IHRATE      -- LARGET RATE IN CURRENT PRINTING SEQUENCE                   
C     IPAGE       -- PAGE OF FORMATED DUMP DONE                                 
C     IPFLAG      -- PRINT FLAG TO PRINT HEADER ON TOP OF EACH PAGE             
C     ITRNPG      -- COUNTER; PRINT 60 LINES PER PAGE                           
C     KR          -- NUMBER OF PARAMETERS                                       
C     LOCPOS(200) -- HOLDS POSITION OF PARAMETER IN MINUTE                      
C     LOGBEG -- LOGICAL DATA RECORD TO BE PROCESSED IN PHYSICAL DATA REC        
C     MINUTE(60,10000) -- HOLDS ONE MINUTE OF DATA                              
C     NDREC       -- NUMBER OF RECORD CURRENTLY BEING PROCESSED                 
C     NLREC       -- NUMBER OF LOGICAL RECORDS PER PHYSICAL RECORD              
C     NUMSEC      -- INDEX USED IN MINUTE  TO INDICATE SECONDS                  
C     TIMESP      -- STOP TIME IN SECONDS                                       
C     TIMEST      -- START TIME IN SECONDS                                      
C                                                                               
                                                                                
C-----READ IN SPECIFICATION DATA ON RAW DUMP                                    
      READ(5,1050) DUMMY                                                        
      READ(5,1070) IHOWTO                                                       
      READ(5,1050) DUMMY                                                        
      READ(5,1070) IDMPBY                                                       
                                                                                
C-----READ IN START AND STOP TIME                                               
      READ(5,1050) DUMMY                                                        
      READ(5,1070) ISTHH,ISTMM,ISTSS                                            
      READ(5,1070) ISPHH,ISPMM,ISPSS                                            
                                                                                
C-----READ IN START AND STOP RECORD                                             
      READ(5,1050) DUMMY                                                        
      READ(5,1070) STREC,SPREC                                                  
                                                                                
C-----PRINT OUT BY COLUMNS (0) OR HORIZONTALLY(1)                               
      READ(5,1050) DUMMY                                                        
      READ(5,1070) KPRT                                                         
                                                                                
C-----READ IN THE NUMBER, AND VARIABLES TO BE DUMPED                            
      READ(5,1050) DUMMY                                                        
      READ(5,1070) INUM                                                         
      READ(5,1050) DUMMY                                                        
      READ(5,1080) (NAMDMP(I),I=1,INUM)                                         
      TIMEST=ISTHH*3600+ISTMM*60+ISTSS                                          
      TIMESP=ISPHH*3600+ISPMM*60+ISPSS                                          
                                                                                
C-----INITILIZE SOME VARIABLES                                                  
      IPOS=0                                                                    
      IPAGE=0                                                                   
      IPFLAG=1                                                                  
      NDREC=0                                                                   
                                                                                
  10  NUMSEC=0                                                                  
                                                                                
  20  NDREC=NDREC+1                                                             
                                                                                
C-----READ IN A PHYSICAL RECORD OF DATA FROM RAW TAPE                           
      CALL RDTAPE(IUNIT,1,2,NBUF,2000)                                          
      CALL IOWAIT(IUNIT,NSTATE,LEN)                                             
C     PRINT 1201,LEN,(NBUF(N),N=1,5)                                            
 1201 FORMAT(' LEN,NBUF ',I8,/,(5(1X,O22)))                                     
      NSTATE=NSTATE+1                                                           
      GO TO(60,210,50,40)NSTATE                                                 
  40  WRITE (6,1010) NDREC                                                      
      RETURN                                                                    
  50  WRITE(6,1020) NDREC                                                       
      RETURN                                                                    
                                                                                
C-----GOOD READ                                                                 
C-----FILL UP MINUTE ARRAY WITH ONE MINUTE OF DATA                              
  60  IF(IDMPBY.EQ.0) THEN                                                      
          IF(NDREC.GT.SPREC) THEN                                               
              IF(NUMSEC.GE.1) GOTO 210                                          
              RETURN                                                            
          ENDIF                                                                 
          IF(NDREC.LT.STREC) GOTO 10                                            
      ENDIF                                                                     
      LOGBEG=1                                                                  
                                                                                
C     PRINT 1227,NDREC,NLREC,LENLOG,NUMSEC                                      
 1227 FORMAT(' NDREC,NLREC,LENLOG,NUMSEC ',4I8)                                 
                                                                                
C-----LOOP OVER NUMBER OF LOGICAL RECORDS WITHIN EACH PHYSICAL RECORD           
  70  DO 200 KL=LOGBEG,NLREC                                                    
         NUMSEC=NUMSEC+1                                                        
         BEGREC(NUMSEC)=NDREC                                                   
         JJ=0                                                                   
         IOFFS=(KL-1)*LENLOG*16                                                 
         CALL GBYTES(NBUF,ITIME,IOFFS,BITS(2),0,4)                              
C        PRINT 1229,LENLOG,IOFFS,NLREC,NDREC,KL,ITIME                           
 1229    FORMAT(' LENLOG,IOFFS,NLREC,NDREC,KL,ITIME(1-5)',/,10I8)               
         TIME(NUMSEC,1)=ITIME(2)                                                
         TIME(NUMSEC,2)=ITIME(3)                                                
         TIME(NUMSEC,3)=ITIME(4)                                                
                                                                                
C-----FIND STARTING TIME IF NECESSARY                                           
         IF(ITIME(1).NE.103201B) THEN                                           
             NUMSEC=NUMSEC-1                                                    
             GOTO 20                                                            
         ENDIF                                                                  
         ITIME(5)=ITIME(2)*3600+ITIME(3)*60+ITIME(4)                            
         IF(IDMPBY.EQ.0) GOTO 80                                                
         IF(ITIME(5).GT.TIMESP) THEN                                            
            NUMSEC=NUMSEC-1                                                     
            IF(NUMSEC.GT.0) GOTO 210                                            
            RETURN                                                              
         ENDIF                                                                  
         IF(ITIME(5).LT.TIMEST) THEN                                            
            NUMSEC=0                                                            
            GOTO 200                                                            
         ENDIF                                                                  
                                                                                
C-----START RAW DUMP HERE                                                       
  80     IF(IPOS.GT.0) GOTO 110                                                 
                                                                                
C-----FIND THE LOCATION OF PARAMETERS TO BE DUMPED                              
         DO 100 J=1,KR                                                          
            DO 90 I=1,INUM                                                      
               IF(NAMES(J).NE.NAMDMP(I)) GOTO 90                                
               IPOS=IPOS+1                                                      
               LOCPOS(IPOS)=J                                                   
                                                                                
C-----FIND STARTING LOCATIONS FOR PARAMETERS IN THE MINUTE ARRAY                
               IF(IPOS.EQ.1) THEN                                               
                   LOCMIN(IPOS)=1                                               
               ELSE                                                             
                   LOCMIN(IPOS)=LOCMIN(IPOS-1)+RATES(LOCPOS(IPOS-1))            
               ENDIF                                                            
  90        CONTINUE                                                            
 100     CONTINUE                                                               
                                                                                
C-----CHECK FOR NULL MATCH ON INPUT FILE                                        
         IF(IPOS.EQ.0)  THEN                                                    
            PRINT 1040                                                          
            PRINT 1030                                                          
            RETURN                                                              
         ENDIF                                                                  
                                                                                
C-----NOW FILL UP THE MINUTE ARRAY                                              
 110     DO 190 I=1,IPOS                                                        
            IK=LOCPOS(I)                                                        
            IOFFS=((KL-1)*LENLOG*16)+FSTBIT(IK)-1                               
            CALL GBYTES(NBUF,IDATA,IOFFS,BITS(IK),BTSKIP(IK),RATES(IK))         
            DO 190 J=1,RATES(IK)                                                
               JJ=JJ+1                                                          
                                                                                
C-----MAKE CONVERSIONS OF THE DATA AS SPECIFIED BY CONKEY                       
C     SEE EXPLANATION OF CONKEY CONVERSIONS IN ADSUD                            
                                                                                
               GO TO(155,180,130,120,120,120,150,150) (CONKEY(IK) + 1)          
                                                                                
                                                                                
 155           IF (NAMES(IK) .EQ. 'DME     ') THEN                              
               CALL SKEY8 (IDATA(1),IDATA(2),IDATA(3),MINUTE(NUMSEC,JJ),        
     $         MINUTE(NUMSEC,JJ+1),MINUTE(NUMSEC,JJ+2))                         
                  JJ=JJ+2                                                       
                  GOTO 190                                                      
               ELSE                                                             
                  GOTO 180                                                      
               ENDIF                                                            
                                                                                
 120           PRINT 1090,CONKEY(IK)                                            
               RETURN                                                           
                                                                                
C-----CONKEY=2                                                                  
 130           MASK=2**(BITS(IK))-1                                             
               IDATA(J)=IDATA(J).AND.MASK                                       
               MAX1=2**(BITS(IK)-1)-1                                           
               IF(IDATA(J).GT.MAX1) IDATA(J)=IDATA(J)-2**(BITS(IK))             
               GOTO 180                                                         
                                                                                
C-----CONKEY=6 & 7                                                              
 150           IDATA(J)=.NOT.IDATA(J)                                           
               IF(CONKEY(IK).EQ.6) GOTO 180                                     
               MASK=2**(BITS(IK))-1                                             
               IDATA(J)=IDATA(J).AND.MASK                                       
               MAX1=2**(BITS(IK)-1)-1                                           
               IF(IDATA(J).GT.MAX1) IDATA(J)=IDATA(J)-2**(BITS(IK))             
 180           MINUTE(NUMSEC,JJ)=FLOAT(IDATA(J))/FACTOR(IK)                     
               IF(IHOWTO.EQ.0) MINUTE(NUMSEC,JJ)=IDATA(J)                       
               IF(NAMES(LOCPOS(I)).EQ.8HNMLORN  ) IMINUTE(NUMSEC,JJ)  =         
     1         IDATA(J)                                                         
               IF(NAMES(LOCPOS(I)).EQ.'LORN    ') IMINUTE(NUMSEC,JJ)  =         
     1         IDATA(J)                                                         
 190     CONTINUE                                                               
         IF(NUMSEC.EQ.60) THEN                                                  
            LOGBEG=KL+1                                                         
            IF(LOGBEG.GT.NLREC) LOGBEG=1                                        
            GOTO 210                                                            
         ENDIF                                                                  
 200  CONTINUE                                                                  
      GOTO 20                                                                   
                                                                                
C-----NOW PRINT OUT THE DATA                                                    
 210  IF(KPRT.EQ.0) CALL KKPRNT(NUMSEC,ITRNPG,IPFLAG,IPOS)                      
      IF(KPRT.EQ.1) CALL CCPRNT(NUMSEC,IPOS)                                    
      IF(IDMPBY.EQ.0.AND.NDREC.GT.SPREC) RETURN                                 
      IF(IDMPBY.EQ.1.AND.ITIME(5).GE.TIMESP) RETURN                             
      IF(NSTATE.EQ.2) GOTO 30                                                   
      NUMSEC=0                                                                  
      IF(LOGBEG.GT.1) GOTO 70                                                   
      GOTO 10                                                                   
      RETURN                                                                    
  30  WRITE (6,1000) NDREC                                                      
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------          
                                                                                
      SUBROUTINE KKPRNT(NUMSEC,ITRNPG,IPFLAG,IPOS)                              
C                                                                               
C     SUBROUTINE KKPRINT PRINTS OUT THE RAW TAPE DATA IN COLUMN FORMAT          
C                                                                               
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              ITYPE(120),LENLOG,NLREC,NBUF(2000),                         
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),        
     1BEGREC(60)                                                                
      DIMENSION PRTROW(10),LOCBYRT(200)                                         
      INTEGER DUMMY,FSTBIT,BTSKIP,BITS,RATES,TIME,CONKEY,BEGREC                 
      REAL MINUTE                                                               
      DATA NMLORN/8HLRNC    /                                                   
 1000 FORMAT(1H1)                                                               
 1010 FORMAT(23X,A4,'-',I3,10X,I2,'/',I2,'/',I4,10X,'BEGINNING RECORD ='        
     1,I5,31X,'PAGE',2X,I5)                                                     
 1020 FORMAT('  HR MI SEC       ',10(3X,A8))                                    
 1030 FORMAT(2X,I2,1X,I2,1X,F6.3,2X,10(1X,F10.3))                               
                                                                                
      DO 34, KJI=1 , IPOS                                                       
         LOCBYRT(KJI) = LOCPOS(KJI)                                             
   34 CONTINUE                                                                  
                                                                                
      IF (IPOS .GE. 2) THEN                                                     
       DO 36, KIJ = 1,IPOS                                                      
         DO 35, KJI =2,IPOS                                                     
            IF (RATES(LOCBYRT(KJI)) .LT. RATES(LOCBYRT(KJI-1))) THEN            
               LTEMP = LOCBYRT(KJI)                                             
               LOCBYRT(KJI) = LOCBYRT(KJI-1)                                    
               LOCBYRT(KJI-1) = LTEMP                                           
            ENDIF                                                               
   35    CONTINUE                                                               
   36  CONTINUE                                                                 
      ENDIF                                                                     
                                                                                
                                                                                
      DO 50 I=1,IPOS,10                                                         
         II=I+9                                                                 
         IF(II.GT.IPOS) II=IPOS                                                 
                                                                                
C-----FIND THE LARGEST RATE IN THIS PRINTING SEQUENCE                           
         IHRATE=RATES(LOCBYRT(II))                                              
                                                                                
C-----LOOP OVER 60 SECONDS OR ENDING TIME SEGMENT                               
            DO 40 J=1,NUMSEC                                                    
                                                                                
C-----PRINT PAGE HEADER                                                         
               DO 30 JJ=1,IHRATE                                                
                  IF((ITRNPG.EQ.60.OR.IPFLAG.EQ.1).AND.JJ.NE.182)  THEN         
                     IPAGE=IPAGE+1                                              
                     PRINT 1000                                                 
                     PRINT 1010,NPRO,NFLT,NMON,NDAY,NYEAR,BEGREC(J),IPAG        
                     PRINT 1020,(NAMES(LOCBYRT(K)),K=I,II)                      
                     ITRNPG=0                                                   
                     IPFLAG=0                                                   
                  ENDIF                                                         
                  SECD=TIME(J,3)+FLOAT(JJ-1)/FLOAT(IHRATE)                      
                                                                                
C-----SET UP ROW OF DATA TO BE PRINTED                                          
                  KK=0                                                          
                  DO 20 K=I,II                                                  
                  IF(NAMES(LOCBYRT(K)).EQ.NMLORN    ) THEN                      
                     CALL LORANC(J,K)                                           
                     GOTO 20                                                    
                  ENDIF                                                         
                  KK=KK+1                                                       
                                                                                
C-----SET PRTROW TO OVERFLOW PRINT BUFFER IF PARAM IS NONEXISTANT               
C-----DURING THIS TIME INCREMENT.                                               
                  PRTROW(KK)=99999999.9999                                      
                  TIMECK=TIME(J,3)+FLOAT(JJ-1)/FLOAT(RATES(LOCBYRT(K)))         
                  LOCROW=LOCMIN(K)+INT(RATES(LOCBYRT(K))*(SECD-FLOAT            
     1            (TIME(J,3))))                                                 
                  IF(TIMECK.EQ.SECD) PRTROW(KK)=MINUTE(J,LOCROW)                
  20           CONTINUE                                                         
               PRINT 1030,TIME(J,1),TIME(J,2),SECD,(PRTROW(L),L=1,KK)           
               ITRNPG=ITRNPG+1                                                  
  30        CONTINUE                                                            
  40     CONTINUE                                                               
         IPFLAG=1                                                               
  50  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------          
                                                                                
      SUBROUTINE CCPRNT(NUMSEC,IPOS)                                            
C                                                                               
C     SUBROUTINE CCPRINT PRINTS OUT THE RAW TAPE DATA IN HORIZONTAL             
C     FORMAT                                                                    
                                                                                
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              ITYPE(120),LENLOG,NLREC,NBUF(2000),                         
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),        
     1BEGREC(60)                                                                
      INTEGER DUMMY,FSTBIT,BTSKIP,BITS,RATES,TIME,CONKEY,BEGREC                 
      REAL MINUTE                                                               
      DATA NMLORN/8HLRNC    /                                                   
 1010 FORMAT(//,' -----------------  RECORD# =',I5,5X,'TIME = ',I2,':',         
     1I2,':',I2,'  -----------------')                                          
 1020 FORMAT(/,15X,'PARAMETER = ',A8,5X,'RATE = ',I4)                           
 1030 FORMAT(10(1X,F10.3,1X))                                                   
 1040 FORMAT(21(F6.1))                                                          
 1050 FORMAT(14(F7.1))                                                          
      DO 20 I=1,NUMSEC                                                          
         IF(NAMES(LOCPOS(1)).NE.NMLORN    ) THEN                                
            PRINT 1010,BEGREC(I),TIME(I,1),TIME(I,2),TIME(I,3)                  
         ENDIF                                                                  
         DO 10 J=1,IPOS                                                         
         IF(NAMES(LOCPOS(J)).EQ.'LORN    ') THEN                                
            CALL SLORANC(I,LOCMIN(J))                                           
            GOTO 10                                                             
         ENDIF                                                                  
         PRINT 1020,NAMES(LOCPOS(J)),RATES(LOCPOS(J))                           
         IST=LOCMIN(J)                                                          
         ISP=LOCMIN(J)-1+RATES(LOCPOS(J))                                       
         IF ((NAMES(LOCPOS(J)) .EQ. 'FSSP    ')  .OR.                           
     $    (NAMES(LOCPOS(J)) .EQ. 'Y200    ')  .OR.                              
     $    (NAMES(LOCPOS(J)) .EQ. 'X200    ')  .OR.                              
     $    (NAMES(LOCPOS(J)) .EQ. 'ASAS    ')) THEN                              
          PRINT 1040,(MINUTE(I,K),K=IST,ISP)                                    
         ELSEIF (NAMES(LOCPOS(J)) .EQ. 'X260    ') THEN                         
          PRINT 1050,(MINUTE(I,K),K=IST,ISP)                                    
         ELSE                                                                   
          PRINT 1030,(MINUTE(I,K),K=IST,ISP)                                    
         ENDIF                                                                  
  10     CONTINUE                                                               
  20  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
C---------------------------------------------------------------------          
                                                                                
      SUBROUTINE LORANC(NUMSEC,J)                                               
                                                                                
C     SUBROUTINE LORANC PRINTS OUT THE DATA IN THE LORANC BLOCK.  SINCE         
C     IT IS IN ASCII FORMAT, IT IS PRINTED OUT AS CHARACTER DATA.               
                                                                                
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              ITYPE(120),LENLOG,NLREC,NBUF(2000),                         
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),        
     1BEGREC(60)                                                                
      INTEGER DUMMY,FSTBIT,BTSKIP,BITS,RATES,TIME,CONKEY,BEGREC                 
      REAL MINUTE                                                               
      INTEGER IWORD(6)                                                          
 1010 FORMAT(/,15X,'PARAMETER = ',A8,5X,'RATE = ',I4)                           
 1020 FORMAT(' NUMSEC,LOCMIN(J),WORDS 1-3 OF LORANC ',2I8,3(1X,O5) )            
 1030 FORMAT('        WORDS 92-94 OF LORANC BLOCK =  ', 3(1X,O5) )              
 1040 FORMAT(1X,10A8)                                                           
      PRINT 1010, NAMES(LOCPOS(J)),RATES(LOCPOS(J))                             
                                                                                
C-----WORDS 1-3 ARE BINARY INFORMATION IMBEDDED IN THE ASCII                    
      IWORD(1)=INT( MINUTE(NUMSEC,LOCMIN(J)-1)  )                               
      IWORD(2)=INT( MINUTE(NUMSEC,LOCMIN(J)  )  )                               
      IWORD(3)=INT( MINUTE(NUMSEC,LOCMIN(J)+1)  )                               
C     IWORD(4)=INT( MINUTE(NUMSEC,LOCMIN(J)+90) )                               
C     IWORD(5)=INT( MINUTE(NUMSEC,LOCMIN(J)+91) )                               
C     IWORD(6)=INT( MINUTE(NUMSEC,LOCMIN(J)+92) )                               
                                                                                
C-----PRINT OUT THE BINARY INFORMATION FIRST                                    
      PRINT 1020, NUMSEC,LOCMIN(J),(IWORD(I),I=1,3)                             
C     PRINT 1030, (IWORD(I),I=4,6)                                              
      IST=LOCMIN(J)+2                                                           
      ISP=LOCMIN(J)-1+RATES(LOCPOS(J))                                          
      PRINT 1040, (MINUTE(NUMSEC,K),K=IST,ISP)                                  
      RETURN                                                                    
      END                                                                       
C*********************************************************************          
         SUBROUTINE SKEY8 (IW1,IW2,IW3,FREQ,FLAG,RANG)                          
         DIMENSION IBQ(10),STAT(4)                                              
         DATA STAT /112.8,113.1,109.6,117.0/                                    
         DATA IBQ /9,24,20,12,10,6,5,3,18,17/                                   
C                                                                               
C     THIS CONKEY DOES DME DECODING                                             
C     CONKEY=8                                                                  
C                                                                               
C  NUMR, THE SAMPLE RATE IS 3.  SAMPLE 1 IS FREQ, 2 IS FLAG, 3 IS RANGE         
C                                                                               
C    THE DATA VALUES IN STAT (ABOVE) ARE SUBJECT TO CHANGE                      
C    AS PER REQUIRED FOR EACH NEW PROJECT.  THE DME DATA WILL                   
C    COME ON TAPE AS A BLOCK OF 3 16-BIT WORDS .  THESE WORDS                   
C    ARE ACCORDINGLY DECODED FOR FREQUENCY, FLAG AND RANGE CALCULATIONS.        
C                                                                               
C    WORD 1 :  SAMPLED DME STATION (W/BI-QUINARY)                               
C         XXX 12 11 / 10 9 8 7 6 / 5 4 3 2 1 / 0      BINARY DATA BITS          
C            / *10. /   *1.      /   *.1     / *.05   WEIGHTING OF BITS         
C                                                                               
C         BITS 1 THRU 5 AND 6 THRU 10 ARE IN BI-QUINARY CODE.                   
C                                                                               
C    WORD 2 :  MOST SIGNIFICANT WORD OF RANGE AND STATION FLAG                  
C                                                                               
C     15 14 / 13 12 / 11 10 9 8 / 7 6 5 4 / 3 2 1 0  BINARY DATA BITS           
C     FLAG  / *100. /    *10.   /   *1.   /   *.1    WEIGHTING OF BITS          
C                                                                               
C    WORD 3 : LEAST OF SIGNIFICANT WORD OF RANGE                                
C                                                                               
C         15 14 13 12 11 10 9 8 / 7 6 5 4 / 3 2 1 0  BINARY DATA BITS           
C                 ANY VALUE     /  *.01   / ZEROS    WEIGHTING OF BITS          
C                                                                               
C      BI-QUINARY TO DECIMAL TABLE FOLLOWS :                                    
C                                                                               
C      BI-QUINARY  9 , 24 , 20 , 12 , 10 , 6 , 5 , 3 , 18 , 17                  
C      DECIMAL     0 ,  1 ,  2 ,  3 ,  4 , 5 , 6 , 7 ,  8 ,  9                  
C                                                                               
C                                                                               
C      THIS ROUTINE WILL OUTPUT (TO NEXT OPERATION) :                           
C           FREQ - FREQUENCY STATION ID                                         
C           FLAG - GOOD OR BAD DATA FLAG                                        
C           RANG - DISTANCE DATA                                                
C                                                                               
C        --------------------------------------------------------               
C                                                                               
C        MASK THE 3 16-BIT WORDS INTO 10 PARTS                                  
         ICNT = 0                                                               
  100    CALL GBYTES(IW1,I1211,51,2,0,1)                                        
         CALL GBYTES(IW1,I1006,53,5,0,1)                                        
         CALL GBYTES(IW1,I0501,58,5,0,1)                                        
         CALL GBYTES(IW1,I0000,63,1,0,1)                                        
         CALL GBYTES(IW2,IFLAG,48,1,0,1)                                        
         CALL GBYTES(IW2,I1312,50,2,0,1)                                        
         CALL GBYTES(IW2,I1108,52,4,0,1)                                        
         CALL GBYTES(IW2,I0704,56,4,0,1)                                        
         CALL GBYTES(IW2,I0300,60,4,0,1)                                        
         CALL GBYTES(IW3,ILST,56,4,0,1)                                         
         CALL GBYTES(IW3,IZ0,60,4,0,1)                                          
C        PRINT 239,IZ0,ILST,I0300,I0704,I1108,I1312,IW2,IW3                     
  239    FORMAT(' KEY8:IZ0-IW3 ',6I8,1X,O10,1X,O10)                             
         IF(IZ0.GT.0 .OR. ILST.GT.9 .OR. I0300.GT.9 .OR. I0704.GT.9 .OR.        
     1I1108.GT.9 .OR. I1312.GT.9) THEN                                          
C           PRINT 240,IZ0,ILST,I0300,I0704,I1108,I1312,IW2,IW3                  
  240       FORMAT(' KEY8:IZ0-IW3 ',6I8,1X,O10,1X,O10)                          
            CALL BTSHIFT(IW2,IW3)                                               
            ICNT = ICNT + 1                                                     
C           PRINT 241,ICNT,IW2,IW3                                              
C 241       FORMAT(' KEY8:AFTER BITSHIFT,ICNT,IW2,IW3 ',I4,1X,O10,1X,O10        
            IF(ICNT.GT.1) GO TO 601                                             
            GO TO 100                                                           
         END IF                                                                 
         DO 1230 IDME=1,10                                                      
            IF(I1006.EQ.IBQ(IDME)) THEN                                         
               I1006=IDME-1                                                     
               GO TO 201                                                        
            ELSE                                                                
               IF(IDME.EQ.10)I1006=0                                            
            END IF                                                              
 1230    CONTINUE                                                               
 201     DO 1231 IIDME=1,10                                                     
            IF(I0501.EQ.IBQ(IIDME)) THEN                                        
               I0501=IIDME-1                                                    
               GO TO 401                                                        
            ELSE                                                                
               IF(IIDME.EQ.10)I0501=0                                           
            END IF                                                              
 1231    CONTINUE                                                               
 401     CONTINUE                                                               
         FREQ=100.+(I1211-1.)*10.+I1006+(I0501*.1)+(I0000*.05)                  
         FLAG=IFLAG                                                             
         RANG=100.*I1312+(10.*I1108)+I0704+(.1*I0300)+(.01*ILST)                
C                                                                               
C        PRINT 185,FREQ,FLAG,RANG                                               
  185    FORMAT(' KEY8: FREQ,FLAG,RANG ',3F10.2)                                
         RETURN                                                                 
C          ************ BAD DATA ******************                             
 601     FREQ = 99999.00                                                        
         RANG = 99999.00                                                        
         FLAG = 99999.00                                                        
         RETURN                                                                 
C                                                                               
         END                                                                    
         SUBROUTINE BTSHIFT(IW2,IW3)                                            
C                                                                               
C        SUB. TO SHIFT WORD 2-3 BY ONE BIT                                      
C                                                                               
C        SHIFT WORD 3 TO LEFT ONE BIT                                           
         IT3 = SHIFTL(IW3,1)                                                    
         IW3 = IT3                                                              
C        GRAB BIT 9 FROM WORD 3 TO ADD ONTO WORD 2                              
         CALL GBYTES(IW3,MSK,55,1,0,1)                                          
C        SHIFT WORD 2 TO LEFT ONE BIT                                           
         IT2 = SHIFTL(IW2,1)                                                    
         IW2 = IT2                                                              
C        ADD BIT 9 OF WORD 3 TO END OF WORD 2                                   
         IT4 = IW2 .OR. MSK                                                     
         IW2 = IT4                                                              
         RETURN                                                                 
         END                                                                    
C                                                                               
      SUBROUTINE SLORANC(NMSEC,INDEX)                                           
C                                                                               
      COMMON/HEADER/NAMBLK(8),ISIZE(8),IFIRST(8),ILAST(8),ISDI(120),            
     1              LOCATE(2,120),NAMSDI(120),C1(120),C2(120),C3(120),          
     2              ITYPE(120),LENLOG,NLREC,NBUF(2000),                         
     3              NHEAD(20),MPYSCL,MAXBAS,IBUFBAS(10),MAXSDI,MAXANA,          
     4              NAMPMS(4),ISZPMS(4),IRTPMS(4),LOCPMS(4),KPROB,LFLAG         
      COMMON/DATE/NYEAR,NMON,NDAY,NHR,NMIN,NSEC,NPRO,NFLT                       
C-----THE DIMENSION 397=HDR(20),INS(11),HSK(55),SDI(120),PMS1(4),PMS2(4)        
C-----                  DME(1),LORN(91)                                         
      COMMON/RAWDMP/FSTBIT(397),BITS(397),BTSKIP(397),RATES(397),               
     1              FACTOR(397),NAMES(397),DUMMY(10),KR,CONKEY(397)             
      COMMON/PNRTER/MINUTE(60,5000),LOCMIN(5000),LOCPOS(200),TIME(60,3),        
     1              BEGREC(60)                                                  
      DIMENSION NAMDMP(200),IDATA(700),ITIME(5)                                 
      INTEGER DUMMY,NAMDMP,SPREC,STREC,TIMEST,TIMESP,FSTBIT,BTSKIP,             
     1        BITS,RATES,TIME,CONKEY,BEGREC                                     
      REAL MINUTE                                                               
      DATA NMLORN/8HLRNC    /                                                   
C                                                                               
C-----BECAUSE THE LORAN-C DATA IS ENCODED AS 8 BIT ASCII WHILE THE              
C     REST OF THE DATA IS IN PACKED INTEGER, IT CANNOT BE STORED                
C     IN THE MINUTE ARRAY AS A FLOATING POINT NUMBER. INSTEAD, A                
C     SEPARATE INTEGER ARRAY IS SET UP AND EQUIVALENCED TO THE                  
C     MINUTE ARRAY SO THAT THE INFORMATION IS STORED IN ITS                     
C     ORIGINAL FORM.                                                            
      INTEGER IMINUTE(60,5000)                                                  
      EQUIVALENCE (MINUTE, IMINUTE )                                            
                                                                                
      LOGICAL INULL                                                             
                                                                                
C                                                                               
C     VARIABLE DESCRIPTIONS USED IN SUBROUTINE TAPE DUMP PRINT OUT              
C                                                                               
C     IDATA       -- ARRAY WHICH HOLD GBYTED NBUF ARRAY                         
C     IHRATE      -- LARGET RATE IN CURRENT PRINTING SEQUENCE                   
C     IPAGE       -- PAGE OF FORMATED DUMP DONE                                 
C     IPFLAG      -- PRINT FLAG TO PRINT HEADER ON TOP OF EACH PAGE             
C     ITRNPG      -- COUNTER; PRINT 60 LINES PER PAGE                           
C     KR          -- NUMBER OF PARAMETERS                                       
C     LOCPOS(200) -- HOLDS POSITION OF PARAMETER IN MINUTE                      
C     LOGBEG -- LOGICAL DATA RECORD TO BE PROCESSED IN PHYSICAL DATA REC        
C     MINUTE(60,10000) -- HOLDS ONE MINUTE OF DATA                              
C     NDREC       -- NUMBER OF RECORD CURRENTLY BEING PROCESSED                 
C     NLREC       -- NUMBER OF LOGICAL RECORDS PER PHYSICAL RECORD              
C     NUMSEC      -- INDEX USED IN MINUTE  TO INDICATE SECONDS                  
C     TIMESP      -- STOP TIME IN SECONDS                                       
C     TIMEST      -- START TIME IN SECONDS                                      
C***********************************************************************        
C THE FORMAT FOR THE LORAN-C BLOCK AS OF JAN. 1986:                             
C  ONE BLOCK OF 91 16-BIT WORDS TO BE INCLUDED IN EACH SECOND OF DATA           
C  WORDS 1, 2, 3 ARE BINARY; THE REST OF THE DATA BLOCK IS IN ASCII.            
C----------                                                                     
C 16-BIT WD    DESCRIPTION                                                      
C        01    STATUS WORD  (BINARY)                                            
C        02    SECOND       (BINARY)                                            
C        03    FRACTION OF SECONS (1/250) (BINARY)                              
C        04    RECORD ID FOR LORAN-C RECORD #1 (CHARACTERS 01)                  
C     05-25    DATA FOR RECORD #1                                               
C        26    RECORD ID FOR LORAN-C RECORD #2 (CHARACTERS 02)                  
C     27-47    DATA FOR RECORD #2                                               
C        48    RECORD ID FOR LORAN-C RECORD #3 (CHARACTERS 03)                  
C     49-69    DATA FOR RECORD #3                                               
C        70    RECORD ID FOR LORAN-C RECORD #4 (CHARACTERS 04)                  
C     71-91    DATA FOR RECORD #4                                               
C***********************************************************************        
C  THIS SUBROUTINE ASSUMES THAT THE ASCII FORMATTED LORAN-C DATA BLOCK          
C  TO BE PICKED UP BY GENPRO INPUT OPERATION AS A DATA BLOCK NAMED LORN         
C  OF 176 8-BIT WORDS, AND EACH WORD IS A RIGHT-JUSTIFIED CHARACTER.            
C  (BINARY WORDS PICKED UP BY INPUT OPERATION SEPARATELY)                       
C-------  LORN DATA BLOCK FORMAT ---------------                                
C WORD    DESCRIPTION                                                           
C 01,02   RECORD ID FOR LORAN-C RECORD #1 (CHARACTERS 01)                       
C 03-44   DATA FOR RECORD #1                                                    
C 45,46   RECORD ID FOR LORAN-C RECORD #2 (CHARACTERS 02)                       
C 47-88   DATA FOR RECORD #2                                                    
C 89,90   RECORD ID FOR LORAN-C RECORD #3 (CHARACTERS 03)                       
C 91-132  DATA FOR RECORD #3                                                    
C 133,134 RECORD ID FOR LORAN-C RECORD #4 (CHARACTERS 04)                       
C 134-176 DATA FOR RECORD #4                                                    
C                                                                               
C---------------------------------------------------------------------          
C** NOW DECODE EACH 8-BIT WORD INTO INTEGER                                     
C** DECODE ID-1                                                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+1),IDX)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+2),IDY)                                 
       IDWDT  = IDX*10 + IDY                                                    
C** DECODE LORAN-D NAV DATA TIME(CTIME)                                         
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+3),IA )                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+4),IB )                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+5),IC )                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+6),ID )                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+7),IE )                                 
       CTIME= IA*10000.+IB*1000.+IC*100.+ID*10.+IE                              
C                                                                               
C      WD 8    : SIGN FOR LATITUDE (+/-)                                        
C      WD 9,10 : LATITUDE IN DEGREES                                            
C      WD 11,12: LATITUDE IN MINUTES                                            
C      WD 13,14: LATITUDE IN HUNDREDTHS MINUTES                                 
C                                                                               
C**     DECODE POS LATITUDE : LORAN(8)-LORAN(14)                                
   11  FORMAT(7X,A1)                                                            
       DECODE(8,11,MINUTE(NMSEC,INDEX-1+8)) ISIGN                               
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+9 ),IDEG1)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+10),IDEG2)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+11),IMIN1)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+12),IMIN2)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+13),IMIN3)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+14),IMIN4)                              
        DEG = IDEG1*10. + IDEG2                                                 
        SMIN= IMIN1*10. + IMIN2 + IMIN3*0.1 + IMIN4*0.01                        
        CLAT = DEG + SMIN/60.                                                   
        IF(ISIGN .EQ.1H- ) CLAT = - CLAT                                        
C                                                                               
C      WD 15      : SIGN FOR LONGITUDE (+/-)                                    
C      WD 16,17,18: LONGITUDE IN DEGREES                                        
C      WD 19,20   : LONGITUDE IN MINUTES                                        
C      WD 21,22   : LONGITUDE IN HUNDREDTHS MINUTES                             
C                                                                               
C** DECODE POS LONGITUDE : MINUTE(NMSEC,INDEX-1+15)-MINUTE(NMSEC,INDEX-1        
       DECODE(8,11,MINUTE(NMSEC,INDEX-1+15)) ISIGN                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+16),IDEG1)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+17),IDEG2)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+18),IDEG3)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+19),IMIN1)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+20),IMIN2)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+21),IMIN3)                              
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+22),IMIN4)                              
       DEG = IDEG1*100.+ IDEG2*10. + IDEG3                                      
       SMIN= IMIN1*10. + IMIN2 + IMIN3*0.1 + IMIN4*0.01                         
       CLON = DEG + SMIN/60.                                                    
       IF(ISIGN .EQ. 1H-) CLON = - CLON                                         
C                                                                               
C** DECODE LORAN-C CEP:CIRCULAR ERROR PROBABILITY                               
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+41),IA)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+42),IB)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+43),IC)                                 
       CCEP= IA*10.+ IB + IC*0.1                                                
C** DECODE LORAN-C GROUND SPEED                                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+57),IA)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+58),IB)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+59),IC)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+60),ID)                                 
       CGS = IA*100.+ IB*10. + IC + ID*0.1                                      
C** DECODE LORAN-C TRACK ANGLE                                                  
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+61),IA)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+62),IB)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+63),IC)                                 
       CALL NOSCK(IMINUTE(NMSEC,INDEX-1+64),ID)                                 
       CTK = IA*100.+ IB*10. + IC + ID*0.1                                      
C** CONVERT GROUND SPEED TO M/S (FROM KNOTS)                                    
       CGS = CGS * 0.5144                                                       
C**                                                                             
       PRINT *, ' '                                                             
       PRINT *, 'LORAN-C DATA:  IDWD,    CLAT,     CLON,     CGS,      C        
     $CEP,     CTK'                                                             
       PRINT 1214,IDWDT,CLAT,CLON,CGS,CCEP,CTK                                  
 1214  FORMAT(14X,I6,5(2X,F8.3))                                                
      RETURN                                                                    
      END                                                                       
C                                                                               
C **********************************************************************        
      SUBROUTINE NOSCK(NCH,NO)                                                  
   10 FORMAT(7X,I1)                                                             
      IF (NCH.GE.48 .AND. NCH.LE.57)  THEN                                      
       DECODE(8,10,NCH) NO                                                      
      ELSE                                                                      
       NO=0                                                                     
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
C                                                                               
