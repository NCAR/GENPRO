      PROGRAM TDUMP                                                             
C                                                                               
C   This program reads a GENPRO-1 or GENPRO-2 output tape and then              
C   prints the following information:                                           
C                                                                               
C   1. FROM THE HEADER FILE(BLOCK)                                              
C      A.  NO. OF VARIABLES ON THE TAPE                                         
C      B.  VARIABLE NAMES IN THE ORDER THAT THEY ARE ON THE TAPE.               
C      C.  SCALE FACTOR FOR EACH VARIABLE.                                      
C      D.  FIRST BIT LOCATION OF EACH VARIABLE.                                 
C      E.  NO. OF WORDS/BITS PER LOGICAL RECORD.                                
C      F.  NO. OF LOGICAL RECORD PER PHYSICAL RECORD.                           
C                                                                               
C                                                                               
C   2. FROM THE DATA FILE(BLOCK)                                                
C                                                                               
C      A.  FIRST PHYSICAL RECORD IN OCTAL.                                      
C      B.  FIRST PHYSICAL RECORD IN INTEGER (PACKED VALUES).                    
C      C.  FIRST PHYSICAL RECORD IN DECIMAL (UNPACKED VALUES).                  
C                                                                               
C                                                                               
C   3. OPTION TO DUMP DATA FROM TAPE IN F10.3 FORMAT                            
C      A. SPECIFY BEGINNING AND ENDING TIME PERIOD                              
C                                                                               
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/READIT/IRWFLG,IARRAY(4000),IDATA(10000)                            
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER IRWFLG, IARRAY, IDATA                                             
      INTEGER IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME                
      REAL    STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                       
                                                                                
C---External subroutines used in this module:                                   
      EXTERNAL ENPUT, G1HEDR, HEADER, DATA, TDPRT                               
                                                                                
                                                                                
                                                                                
C---Call the Enput routine to read the Unit-5 input variables                   
      CALL ENPUT                                                                
                                                                                
C---Read the header file according to the Genpro version that wrote it.         
      IF(GNPVER.EQ.0) CALL G1HEDR                                               
      IF(GNPVER.EQ.1) CALL HEADER                                               
                                                                                
C---If it is desired to dump the first couple of records in octal,              
C   integer packed, and decimal unpacked, call the data routine.                
      IF (IRWFLG .EQ. 1 ) CALL DATA                                             
                                                                                
C---Now dump the desired time period from the dataset.                          
       CALL TDPRT                                                               
                                                                                
      END                                                                       
                                                                                
C-----------------------------------------------------------------------        
                                                                                
      SUBROUTINE HEADER                                                         
                                                                                
C---This subroutine reads and echos the header file while decoding              
C   some of the information that resides within the header.                     
C   On Entry:                                                                   
C      The tape is rewound to the beginning of the tape.                        
C      The unit-5 input has been read which decribes the Genpro version         
C          and library which was used to write the tape.                        
C                                                                               
C   On exit:                                                                    
C      The header file has been read, echoed, and the information decoded       
C          in a usable format for this program.                                 
C      The tape is positioned at the beginning of the data.                     
C                                                                               
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $      NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC            
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/HDPRT1/MON,DAY,YRR,HR,MIN,SEC,BGSNP(3),                            
     $                                   EDSNP(3),MCHINE(1),TITLE(300,5)        
      COMMON/HDPRT2/HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                           
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR                                                  
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
      CHARACTER*8 MON, DAY, YRR, HR, MIN, SEC, BGSNP, EDSNP, MCHINE             
      CHARACTER*8 TITLE                                                         
      INTEGER     HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                             
      INTEGER     IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME            
      REAL        STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                   
      REAL        RUNI                                                          
C---External subroutines used in this module:                                   
      EXTERNAL    ABORT,  GBYTES, PRTHDR                                        
                                                                                
C---Local variables:                                                            
      INTEGER NDUMMY(10), NHDREC, KOR, LOGRHD, KT, K3                           
      INTEGER LEN, I, IK, K, JJ, IIARRAY(100)                                   
      LOGICAL READERR                                                           
      CHARACTER*8 NAME, IARRAY(100)                                             
                                                                                
                                                                                
C     VARIABLE DESCRIPTIONS FOR SUBROUTINE HEADER                               
C                                                                               
C     HDRLOG -- HEADER LOGICAL RECORD PER PHYSICAL RECORD                       
C     HDRSIZ -- BITS PER HEADER PHYSICAL RECORD                                 
C     LOGBIT -- BITS PER DATA LOGICAL RECORD                                    
C     DATLOG -- DATA LOGICAL RECORDS PER PHYSICAL RECORD                        
C     DATSIZ -- BITS PER DATA PHYSICAL RECORD                                   
C                                                                               
                                                                                
C---Initialization                                                              
      NHDREC=0                                                                  
      KOR=0                                                                     
      LOGRHD=0                                                                  
      KT=0                                                                      
      KR=0                                                                      
      K3=0                                                                      
      NUMSMP=0                                                                  
                                                                                
      REWIND  IUNIT                                                             
                                                                                
C---Top of main subroutine loop.                                                
   10 CONTINUE                                                                  
                                                                                
C---Use a buffered read for obtaining the header data.                          
C   the logical record size is 80 characters.                                   
      READERR = .FALSE.                                                         
   20 BUFFER IN (IUNIT,1) (IIARRAY(1),IIARRAY(100))                             
                                                                                
C---Check for disk read errors, and if so, give messages and try                
C   one more time. Stop after 2 bad reads.                                      
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .GE.0.0) THEN                                                    
C-----IF (ANINT(UNIT(IUNIT)) .GE. 0.0)  THEN                                    
                                                                                
C---Check for any error.                                                        
        IF (RUNI .GT. 0.0) THEN                                                 
         WRITE(6,8995)                                                          
 8995    FORMAT(' ERROR CODE EXECUTED - SUBROUTINE HEADER ' )                   
        ENDIF                                                                   
                                                                                
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 0.0) THEN                                  
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ. 0.0) THEN                                                   
                                                                                
C--------No error, an EOF has been found, now just print out the                
C        header information that has been determined and exit this              
C        subroutine.                                                            
            WRITE (6, 9000) NHDREC+1                                            
 9000       FORMAT(' EOF SENSED IN HEADER; DATA FILE REC. NO.', I8,///)         
            CALL PRTHDR(NHDREC)                                                 
            RETURN                                                              
         ENDIF                                                                  
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 1.0)                                       
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.1.0)                                                         
     $      WRITE (6, 9005) NHDREC                                              
 9005       FORMAT(' PARITY ERROR SENSED IN HEADER; FILE REC. NO.',I8)          
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 2.0)                                       
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.2.0)                                                         
     $      WRITE (6, 9010) NHDREC                                              
 9010       FORMAT(' DISK MALFUNCTION; HEADER READ, FILE REC. NO.',I8)          
                                                                                
C---If the parity or disk error repeated, abort.                                
         IF ( READERR ) THEN                                                    
            WRITE ( 6, 9015) NHDREC                                             
 9015       FORMAT(' DISK MALFUNCTION; PARITY ABORT, REC. NO.',I8)              
            CALL ABORT                                                          
         ENDIF                                                                  
                                                                                
C----Set flag and set up for another read.                                      
         READERR = .TRUE.                                                       
         BACKSPACE IUNIT                                                        
         GOTO 20                                                                
      ENDIF                                                                     
                                                                                
C---Otherwise, a good read has occurred.                                        
                                                                                
C---Transfer the buffered array to a character array. This is done              
C   because the BUFFER IN read requires a non-character read buffer.            
      CALL GBYTES ( IIARRAY, IARRAY, 0, 64, 0, 100)                             
                                                                                
C   Get the physical record length so it can be used as bound.                  
      LEN=LENGTH(IUNIT)                                                         
                                                                                
C---Process a physical record of data                                           
      DO 70 I=1,LEN,10                                                          
         IF(LIBVER.EQ.0) NAME(1:8) = IARRAY(I)                                  
         IF(LIBVER.EQ.1) DECODE( 8,9020,IARRAY(I)) NAME(1:7)                    
 9020    FORMAT(1X,A7)                                                          
         IK = I+9                                                               
         LOGRHD = LOGRHD+1                                                      
                                                                                
C------Echo the header into the output file                                     
         WRITE(6,9025) (IARRAY(K),K=I,IK)                                       
 9025    FORMAT(2X,10A8)                                                        
                                                                                
C------Now, check for various expected keywords in the header file and          
C      decode the information asssociated with them, only one keyword           
C      will match each pass through this code.                                  
C      The LIBVER flag is used to flag differences in library output            
C      formats from Genpro.                                                     
         IF(NAME(1:7).EQ.'PROJECT')DECODE(51,9030,IARRAY(I)) PROJCT             
 9030    FORMAT(11X,5A8)                                                        
         IF(NAME(1:6).EQ.'PRDATE') DECODE(25,9035,IARRAY(I)) DAY,MON,YRR        
 9035    FORMAT(12X,A2,3X,A3,3X,A2)                                             
         IF(NAME(1:6).EQ.'PRTIME') DECODE(26,9040,IARRAY(I)) HR,MIN,SEC         
 9040    FORMAT(12X,A2,4X,A2,4X,A2)                                             
         IF(NAME(1:6).EQ.'BEGSNP')                                              
     $      DECODE(46,9045,IARRAY(I)) BGSNP(1),BGSNP(2),BGSNP(3)                
 9045    FORMAT(12X,A8,5X,A8,5X,A8)                                             
         IF(NAME(1:6).EQ.'ENDSNP')                                              
     $      DECODE(46,9045,IARRAY(I)) EDSNP(1),EDSNP(2),EDSNP(3)                
         IF(NAME(1:7).EQ.'MACHINE') DECODE(15,9050,IARRAY(I)) MCHINE            
 9050    FORMAT(11X,A4)                                                         
         IF(NAME(1:6) .EQ. 'HDRLOG') THEN                                       
           IF(LIBVER.EQ.0) DECODE(13,9055,IARRAY(I)) HDRLOG                     
 9055      FORMAT( 9X,I4)                                                       
           IF(LIBVER.EQ.1) DECODE(15,9060,IARRAY(I)) HDRLOG                     
 9060      FORMAT(10X,I5)                                                       
         END IF                                                                 
         IF(NAME(1:6) .EQ. 'HDRSIZ') THEN                                       
           IF(LIBVER.EQ.0) DECODE(15,9065,IARRAY(I)) HDRSIZ                     
 9065      FORMAT(9X,I6)                                                        
           IF(LIBVER.EQ.1) DECODE(19,9070,IARRAY(I)) HDRSIZ                     
 9070      FORMAT(10X,I9)                                                       
           NSIZZ=HDRSIZ/HDRLOG                                                  
         END IF                                                                 
         IF(NAME(1:6) .EQ. 'DATLOG') THEN                                       
           IF(LIBVER.EQ.0) DECODE(13,9055,IARRAY(I)) DATLOG                     
           IF(LIBVER.EQ.1) DECODE(15,9060,IARRAY(I)) DATLOG                     
         END IF                                                                 
         IF(NAME(1:6) .EQ. 'DATSIZ') THEN                                       
           IF(LIBVER.EQ.0) DECODE(15,9065,IARRAY(I)) DATSIZ                     
           IF(LIBVER.EQ.1) DECODE(19,9070,IARRAY(I)) DATSIZ                     
         END IF                                                                 
         IF(NAME(1:6) .EQ. 'LOGBIT') THEN                                       
           IF(LIBVER.EQ.0) DECODE(15,9065,IARRAY(I)) LOGBIT                     
           IF(LIBVER.EQ.1) DECODE(19,9070,IARRAY(I)) LOGBIT                     
         END IF                                                                 
                                                                                
C-----Three 'ORDVAR' directives are expected in the header file. Each           
C     time that one is encountered, different information is expected           
C     to follow.  After the first encounter, the rates will follow.             
C     After the second time, variable location information will follow.         
C     After the third time, factors and terms are determined. See the           
C     Genpro output operation documentation for more details.                   
                                                                                
C-----ORDVAR encountered, update the KOR counter.                               
         IF(NAME(1:6) .EQ. 'ORDVAR' ) KOR=KOR+1                                 
                                                                                
C-----ORDVAR encountered -- first time                                          
         IF (KOR.EQ.1) THEN                                                     
            IF (NAME(1:6) .EQ. 'LETVAR') THEN                                   
               KR= KR+1                                                         
               IF (LIBVER.EQ.0) DECODE(57,9075,IARRAY(I)) NRATE(KR)             
 9075          FORMAT(53X,I4)                                                   
               DECODE (51,9030,IARRAY(I)) (TITLE(KR,JJ),JJ=1,5)                 
               IF (LIBVER.EQ.0) NUMSMP=NUMSMP+NRATE(KR)                         
            END IF                                                              
         END IF                                                                 
                                                                                
C-----ORDVAR encountered -- second time                                         
         IF (KOR.EQ.2) THEN                                                     
            IF (NAME(1:6) .EQ. 'LETVAR') THEN                                   
               KT= KT+1                                                         
               IF (LIBVER.EQ.0) THEN                                            
                  DECODE(70,9080,IARRAY(I)) UNITS(KT),TERM(KT),                 
     $                    FACTOR(KT),NBITS(KT),NFSTBT(KT),NAMVAR(KT)            
 9080             FORMAT(11X,A8,2X,F7.1,1X,F7.1,4X,I3,1X,I6,12X,A8)             
               ENDIF                                                            
               IF (LIBVER.EQ.1) THEN                                            
                 DECODE(69,9085,IARRAY(I)) UNITS(KT),NRATE(KT),                 
     $                                  NBITS(KT),NFSTBT(KT),NAMVAR(KT)         
 9085            FORMAT(11X,A8,9X,I6,1X,I3,1X,I8,15X,A8)                        
                 NUMSMP=NUMSMP+NRATE(KT)                                        
               ENDIF                                                            
               NFSTWD(KT)=(NFSTBT(KT)/NBITS(KT))+1                              
            ENDIF                                                               
         END IF                                                                 
                                                                                
C-----ORDVAR encountered ----  third time                                       
         IF (KOR.EQ.3.AND.LIBVER.EQ.1) THEN                                     
            IF (NAME(1:6) .EQ. 'LETVAR') THEN                                   
               K3= K3+1                                                         
               DECODE(42,9090,IARRAY(I)) TERM(K3),FACTOR(K3)                    
 9090          FORMAT(16X,F11.4,5X,F10.4)                                       
            END IF                                                              
         END IF                                                                 
  70  CONTINUE                                                                  
                                                                                
C---Continue returning to the top and read until an EOF marker is found.        
C   The normal exit to this routine is through the error processing             
C   section when and EOF has been found.                                        
      NHDREC = NHDREC + 1                                                       
      GO TO 10                                                                  
                                                                                
      END                                                                       
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE G1HEDR                                                         
                                                                                
C---This subroutine functions similarly to the HEADER routine with              
C   the difference being that this routine decodes a tape that has              
C   been written by Genpro-1.                                                   
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $       NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC           
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/HDPRT1/MON,DAY,YRR,HR,MIN,SEC,BGSNP(3),                            
     $                                   EDSNP(3),MCHINE(1),TITLE(300,5)        
      COMMON/HDPRT2/HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                           
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR,RUNI                                             
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
      CHARACTER*8 MON, DAY, YRR, HR, MIN, SEC, BGSNP, EDSNP, MCHINE             
      CHARACTER*8 TITLE                                                         
      INTEGER     HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                             
      INTEGER IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME                
      REAL    STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                       
C---Local area variable declarations:                                           
      INTEGER   IARRAY(3000),NDUMMY(10),KARRAY(4000),JARRAY(5000)               
      INTEGER   IFILLR(5200), NBIT10(10)                                        
      INTEGER   NHDREC, LEN, NBTTS, LOCEND, IER, ITOT, ITOT2, ITER              
      INTEGER   IB, JB, I, IK, K, IWRDS, JJ, J                                  
      CHARACTER*10  CTEMP1, CTEMP2                                              
      CHARACTER*2  UNITS2(300)                                                  
      LOGICAL   READERR                                                         
                                                                                
C---External routines used in this module                                       
      EXTERNAL  ABORT,  SCONV, SBYTES, GBYTES, PRTHDR                           
      EXTERNAL  GADSUD                                                          
                                                                                
      DATA      MCHINE/'7600    '/                                              
                                                                                
C---Initialization                                                              
      KR=0                                                                      
      NHDREC=1                                                                  
      NUMSMP=0                                                                  
                                                                                
C---Use a buffered read for obtaining the header data.                          
C   The logical record size is 104 characters.                                  
C   It is expected that the entire physical record will exist in the            
C   first read.                                                                 
      READERR = .FALSE.                                                         
   20 BUFFER IN (IUNIT,1) (KARRAY(1),KARRAY(4000))                              
                                                                                
C---Check for disk read errors, and if so, give messages and try                
C   one more time. Stop after 2 bad reads.                                      
C     IF (ANINT(UNIT(IUNIT)) .GE. 0.0)  THEN                                    
      RUNI= UNIT(IUNIT)                                                         
      IF (ANINT(RUNI) .GE.0.0) THEN                                             
                                                                                
         WRITE(6,8995)                                                          
 8995    FORMAT(' ERROR CODE EXECUTED - SUBROUTINE G1HEDR ' )                   
                                                                                
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 0.0) THEN                                  
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.0.0) THEN                                                    
                                                                                
C--------An unexpected EOF has been found.                                      
            WRITE (6, 9000) NHDREC                                              
 9000       FORMAT(' UNEXPECTED EOF SENSED IN HEADER; REC. NO.', I8)            
            RETURN                                                              
         ENDIF                                                                  
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 1.0)                                       
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.1.0)                                                         
     $      WRITE (6, 9005) NHDREC                                              
 9005       FORMAT(' PARITY ERROR SENSED IN HEADER; FILE REC. NO.',I8)          
C--------IF (ANINT(UNIT(IUNIT)) .EQ. 2.0)                                       
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.2.0)                                                         
     $      WRITE (6, 9010) NHDREC                                              
 9010       FORMAT(' DISK MALFUNCTION; HEADER READ, FILE REC. NO.',I8)          
                                                                                
C---If the parity or disk error repeated, abort.                                
         IF ( READERR ) THEN                                                    
            WRITE ( 6, 9015) NHDREC                                             
 9015       FORMAT(' DISK MALFUNCTION; PARITY ABORT, REC. NO.',I8)              
            CALL ABORT                                                          
         ENDIF                                                                  
                                                                                
C----Set flag and set up for another read.                                      
         READERR = .TRUE.                                                       
         BACKSPACE IUNIT                                                        
         GOTO 20                                                                
      ENDIF                                                                     
                                                                                
C---Otherwise a good read has occurred.                                         
C   Now, convert KARRAY containing a CDC 7600 binary bit stream to a            
C   corrosponding CRAY data array, according to MODE=0.                         
   30 LEN=LENGTH(IUNIT)                                                         
      NBTTS=LEN*64                                                              
                                                                                
C---Documentation on the SCONV routine can be found in the NCAR manual:         
C   'Managing Datasets and Programs at NCAR: The Mass Storage Subsystem'.       
      CALL SCONV(KARRAY,JARRAY,5000,0,NBTTS,LOCEND,IER)                         
                                                                                
C---Check for error in sconv routine                                            
      IF (IER .NE. 0) THEN                                                      
         WRITE (6, 9020 ) IER                                                   
 9020    FORMAT ( /,1X,' PROBLEM WITH 7600 DATA CONVERSION, IER = ', I2)        
      ENDIF                                                                     
                                                                                
C---GBYTE and SBYTE JARRAY into 104 character ASCII logical records             
      ITOT2=LOCEND*2                                                            
      ITER=ITOT2/25                                                             
      IB=1                                                                      
      JB=1                                                                      
      CALL GBYTES(JARRAY,IFILLR,0,32,0,ITOT2)                                   
      DO 40 I=1,ITER                                                            
         CALL SBYTES(IARRAY(IB),IFILLR(JB),0,32,0,25)                           
         IB=IB+13                                                               
         JB=JB+25                                                               
  40  CONTINUE                                                                  
                                                                                
      ITOT=IB-1                                                                 
      DO 50 I=1,ITOT,13                                                         
         IK = I+12                                                              
                                                                                
C---Echo input header to output file                                            
         WRITE(6,9025) (IARRAY(K),K=I,IK)                                       
 9025    FORMAT(2X,13A8)                                                        
                                                                                
C------Certain info resides on lines 1, 14, and 40 of header                    
         IF(I.EQ.1) DECODE(82,9030,IARRAY(I)) PROJCT,HR,MIN,SEC                 
 9030    FORMAT(5A8,34X,A2,1X,A2,1X,A2)                                         
         IF(I.EQ.1) DECODE(23,9035,IARRAY(I)) DAY,MON,YRR                       
 9035    FORMAT(14X,A2,A3,2X,A2)                                                
         IF(I.EQ.14) DECODE(49,9040,IARRAY(I))                                  
     $       BGSNP(1),BGSNP(2),BGSNP(3),EDSNP(1),EDSNP(2),EDSNP(3)              
 9040    FORMAT(29X,A2,1X,A2,1X,A2,4X,A2,1X,A2,1X,A2)                           
         IF (I.EQ.40) THEN                                                      
            DECODE(62,9045,IARRAY(I)) DATLOG,LOGBIT,DATSIZ,IWRDS                
 9045       FORMAT(5X,I2,11X,I3,12X,I4,23X,I2)                                  
                                                                                
            HDRLOG=LOGBIT+11                                                    
            HDRSIZ=HDRLOG*8*104                                                 
            LOGBIT=LOGBIT*IWRDS                                                 
            DATSIZ=DATSIZ*IWRDS                                                 
            NSIZZ=HDRSIZ/HDRLOG                                                 
         ENDIF                                                                  
                                                                                
         IF (I.GT.131) THEN                                                     
            KR=KR+1                                                             
            DECODE(96,9050,IARRAY(I)) NRATE(KR),(TITLE(KR,JJ),JJ=1,5),          
     $            NAMVAR(KR),UNITS(KR),UNITS2(KR),FACTOR(KR),TERM(KR)           
 9050       FORMAT(5X,I3,5X,5A8,2X,A8,1X,A8,A2,5X,F7.1,3X,F7.1)                 
            NBITS(KR)=20                                                        
            IF(KR.EQ.1) NFSTBT(KR)=1                                            
            IF(KR.GT.1) NFSTBT(KR)=NFSTBT(KR-1)+NRATE(KR-1)*NBITS(KR-1)         
            NUMSMP=NUMSMP+NRATE(KR)                                             
            NFSTWD(KR)=(NFSTBT(KR)/NBITS(KR))+1                                 
         ENDIF                                                                  
                                                                                
  50  CONTINUE                                                                  
                                                                                
C---Now just left justify UNITS and convert from A10 to A8.                     
      DO 90, I=1,KR                                                             
         CTEMP1 = UNITS(I) // UNITS2(I)(1:2)                                    
         DO 80, J=1, 10                                                         
            IF (CTEMP1(1:1) .EQ. ' ' ) THEN                                     
               CTEMP2 = CTEMP1(2:10) // ' '                                     
               CTEMP1 = CTEMP2                                                  
            ENDIF                                                               
  80     CONTINUE                                                               
         UNITS(I) = CTEMP1(1:8)                                                 
  90  CONTINUE                                                                  
                                                                                
      CALL PRTHDR(NHDREC)                                                       
      CALL GADSUD                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE PRTHDR(NHDREC)                                                 
                                                                                
C---This subroutine will print out all of the header information which          
C   was decoded using the header subroutine.                                    
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $    NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC              
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/HDPRT1/MON,DAY,YRR,HR,MIN,SEC,BGSNP(3),                            
     $                                   EDSNP(3),MCHINE(1),TITLE(300,5)        
      COMMON/HDPRT2/HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                           
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR,RUNI                                             
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
      CHARACTER*8 MON, DAY, YRR, HR, MIN, SEC, BGSNP, EDSNP, MCHINE             
      CHARACTER*8 TITLE                                                         
      INTEGER     HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                             
                                                                                
C---Argument variable definitions:                                              
      INTEGER     NHDREC                                                        
                                                                                
C---Local area variable defintions:                                             
      CHARACTER*8 CTEMP                                                         
      INTEGER I, JJ                                                             
                                                                                
C---Left justify all of the variable names                                      
      DO 30 I=1,KR                                                              
                                                                                
C------Test whether or not the leading character in the word is a               
C      space, if so, shift all of the characters one location to the            
C      left .                                                                   
                                                                                
         DO 30 JJ=1,8                                                           
            IF (NAMVAR(I)(1:1) .EQ. ' ') THEN                                   
               CTEMP = NAMVAR(I)                                                
               NAMVAR(I) = CTEMP(2:8) // ' '                                    
            ENDIF                                                               
  30  CONTINUE                                                                  
                                                                                
C---Now, print out the header.                                                  
      WRITE(6,9000) NHDREC                                                      
 9000 FORMAT(/,' THERE ARE',I4,' PHYSICAL RECORDS IN THE HEADER FILE')          
                                                                                
      WRITE(6,9005) PROJCT,BGSNP(1),BGSNP(2),BGSNP(3),EDSNP(1),                 
     $       EDSNP(2),EDSNP(3),NSIZZ,HDRLOG,HDRSIZ,LOGBIT,DATLOG,DATSIZ         
 9005 FORMAT('1',2X,5A8,/,2X,'THIS FILE IS ALL OR PART OF THE TIME ',           
     $ 'PERIOD ',A8,2X,A8,2X,A8,'   TO   ',A8,2X,A8,2X,A8,                      
     $ /,2X,'DESCRIPTION OF TAPE: ',/,10X,'HEADER FILE --',                     
     $ I6,' BITS (104 ASCII CHARACTERS) PER LOGICAL RECORD',                    
     $ /,24X,I6,' LOGICAL HEADER RECORDS PER PHYSICAL RECORD',                  
     $ /,24X,I6,' BITS PER PHYSICAL HEADER RECORD',                             
     $ /,12X,'DATA FILE --',I6,' BITS PER LOGICAL DATA RECORD',                 
     $ /,24X,I6,' LOGICAL DATA RECORDS PER PHYSICAL RECORD',                    
     $ /,24X,I6,' BITS PER PHYSICAL DATA RECORD')                               
                                                                                
      IWDSZ=NBITS(1)                                                            
      WRITE (6,9010) DAY,MON,YRR,HR,MIN,SEC,MCHINE,KR,NUMSMP,IWDSZ              
 9010 FORMAT(2X,'PROJECT FLIGHT DATE ',A2,A3,A2,                                
     $ /,2X,'PROJECT FLIGHT TIME ',A2,2X,A2,2X,A2,                              
     $ /,2X,'MACHINE = ',A4,                                                    
     $ /,2X,I4,' PARAMETERS WERE SAVED AT THEIR RESPECTIVE RATES',              
     $ /,2X,I4,' SAMPLES WERE SCALED INTO ',I2,' BIT INTEGER WORDS.')           
                                                                                
      WRITE (6,9015) IWDSZ,IWDSZ                                                
 9015 FORMAT  (2X,'METHOD OF SCALING -- A BIAS TERM WAS ADDED TO EACH ',        
     $ 'SAMPLE OF EACH PARAMETER TO ELIMINATE',/,2X,                            
     $ 'ANY NEGATIVE VALUES. THE BIASED SAMPLE WAS THEN MULTIPLIED ',           
     $ 'BY FACTOR TO INSURE THAT THE ',/,2X,                                    
     $ 'PROPER NUMBER OF DECIMAL PLACES WERE SAVED. THE RECORD MAY BE ',        
     $ 'DECODED BY RIGHT ',/,2X,                                                
     $ 'JUSTIFYING ',I2,' BITS AT A TIME AND REVERSING THE ABOVE ',             
     $ 'SCALING PROCESS.  FOR EXAMPLE: ',/,2X,                                  
     $ 'S(I)=N/FACTOR(I)-TERM(I), WHERE N IS THE ',I2,' BIT SCALED ',           
     $ 'INTEGER WORD, AND S(I) ',/,2X,                                          
     $ 'IS THE DESIRED UNSCALED PARAMETER.')                                    
                                                                                
      WRITE(6,9020)                                                             
 9020 FORMAT(4X,'I',3X,'NFSTWD',3X,'NFSTBT',3X,'RATE',17X,'TITLE',22X,          
     $                         'NAME',6X,'UNITS',9X,'FACTOR',5X,'TERM')         
                                                                                
      DO 40 I=1,KR                                                              
         WRITE(6,9025) I,NFSTWD(I),NFSTBT(I),NRATE(I),                          
     $       (TITLE(I,JJ),JJ=1,5),NAMVAR(I),UNITS(I),FACTOR(I),TERM(I)
 9025    FORMAT(2X,I3,3X,I6,3X,I6,3X,I4,3X,5A8,1X,A8,3X,A8,                     
     $                                       '   (N/',F7.1,') - ',F7.1)         
  40  CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE GADSUD                                                         
                                                                                
C---This subroutine will generate a Genpro compatible User Directive            
C   which can be used when building a Genpro deck that uses a tape              
C   format compatible with the tape currently being analyzed.                   
C                                                                               
C   All of the write statements in this subroutine write to unit number 7       
C   and the output is subsequently disposed using the Cray JCL supplied         
C   with the program.                                                           
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $    NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC              
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/HDPRT1/MON,DAY,YRR,HR,MIN,SEC,BGSNP(3),                            
     $                                   EDSNP(3),MCHINE(1),TITLE(300,5)        
      COMMON/HDPRT2/HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                           
                                                                                
C---Common area variable declarations:                                          
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR ,RUNI                                            
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
      CHARACTER*8 MON, DAY, YRR, HR, MIN, SEC, BGSNP, EDSNP, MCHINE             
      CHARACTER*8 TITLE                                                         
      INTEGER     HDRLOG,HDRSIZ,DATSIZ,NSIZZ,NUMSMP                             
                                                                                
C---Local variable declarations:                                                
      INTEGER I, J, II                                                          
                                                                                
      WRITE(7,9000) HDRLOG                                                      
 9000 FORMAT('/HDRLOG =',I5,20X,'/LOGICAL HEADER RECORDS PER PHYSICAL',         
     $                                                         'RECORD')        
                                                                                
      WRITE(7,9005) HDRSIZ                                                      
 9005 FORMAT('/HDRSIZ =',I9,16X,'/PHYSICAL HEADER RECORD SIZE (BITS)')          
                                                                                
      WRITE(7,9010) PROJCT                                                      
 9010 FORMAT('/PROJECT= "',5A8,'"')                                             
                                                                                
      WRITE(7,9015) DAY,MON,YRR                                                 
 9015 FORMAT('/PRDATE = ("',A2,'","',A3,'","',A2,'")',7X,'/PROJECT ',           
     $                                                           'DATE')        
                                                                                
      WRITE(7,9020) HR,MIN,SEC                                                  
 9020 FORMAT('/PRTIME = ("',A2,'H","',A2,'M","',A2'S")',5X,'/PROJECT ',         
     $                                                           'TIME')        
                                                                                
      WRITE(7,9025) BGSNP(1),BGSNP(2),BGSNP(3)                                  
 9025 FORMAT('/BEGSNP = (  ',A2,'.000    ,  ',A2,'.000    ,  ',A2,              
     $                                                      '.000    )')        
                                                                                
      WRITE(7,9030) EDSNP(1),EDSNP(2),EDSNP(3)                                  
 9030 FORMAT('/ENDSNP = (  ',A2,'.000    ,  ',A2,'.000    ,  ',A2,              
     $                                                      '.000    )')        
                                                                                
      WRITE(7,9035) LOGBIT                                                      
 9035 FORMAT(1X,'LOGBIT =',I9,16X,'/BITS PER LOGICAL DATA RECORD')              
                                                                                
      WRITE(7,9040) DATLOG                                                      
 9040 FORMAT(1X,'DATLOG =',I5,20X,'/LOGICAL DATA RECORDS PER PHYSICAL',         
     $                                                         'RECORD')        
                                                                                
      WRITE(7,9045) DATSIZ                                                      
 9045 FORMAT(1X,'DATSIZ =',I9,16X,'/PHYSICAL DATA RECORDS SIZE (BITS)')         
                                                                                
      WRITE(7,9050)                                                             
 9050 FORMAT('/VARIABLES WRITTEN FOR THIS SNAPSHOT PERIOD')                     
      DO 10 I=1,KR,6                                                            
         J=I+5                                                                  
         IF(J.GT.KR) J=KR                                                       
         WRITE(7,9055) (NAMVAR(II),II=I,J)                                      
 9055    FORMAT(1X,'APPVAR = ',5(A8,'  , '),A8)                                 
  10  CONTINUE                                                                  
                                                                                
      WRITE(7,9060)                                                             
 9060 FORMAT(1X,'ORDVAR = TITLE')                                               
                                                                                
      DO 20 I=1,KR                                                              
         WRITE(7,9065) (TITLE(I,J),J=1,5),NAMVAR(I)                             
 9065    FORMAT(1X,'LETVAR = "',5A8,'", %FOR, ',A8)                             
   20 CONTINUE                                                                  
                                                                                
      WRITE(7,9070)                                                             
 9070 FORMAT(1X,'ORDVAR = UNITS, SAMPLE, RATE, BITS, FSTBIT, SKIP')             
                                                                                
      DO 30 I=1,KR                                                              
         WRITE(7,9075) UNITS(I),NRATE(I),NRATE(I),NBITS(I),NFSTBT(I),           
     $                                                      NAMVAR(I)           
 9075    FORMAT(1X,'LETVAR = "',A8,'",',I6,',',I6,',',I3,',',I8,',',7X,         
     $                                                   '0, %FOR, ',A8)        
   30 CONTINUE                                                                  
                                                                                
      WRITE(7,9080)                                                             
 9080 FORMAT(1X,'ORDVAR = CONKEY, SCLKEY, TERM, FACTOR')                        
                                                                                
      DO 40 I=1,KR                                                              
         WRITE(7,9085) TERM(I),FACTOR(I),NAMVAR(I)                              
 9085    FORMAT(1X,'LETVAR =  1, 2, ',F10.4,'    ,',F10.4,'    , %FOR, '        
     $                                                              ,A8)        
   40 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
C-----------------------------------------------------------------------        
                                                                                
      SUBROUTINE DATA                                                           
C                                                                               
C---Subroutine DATA reads the first two physical records of the input           
C   tape. The contents of each record are then printed out in the               
C   following three forms:                                                      
C     1. Octal                                                                  
C     2. Integer (packed)                                                       
C     3. Decimal (unpacked)                                                     
C                                                                               
C   Only two attempts are made to read each record and if two failures          
C   occur, the program is aborted after issuing a message to the user.          
C                                                                               
C   The data file is returned to the main program positioned at the             
C   beginning of the data.                                                      
C                                                                               
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/READIT/IRWFLG,IARRAY(4000),IDATA(10000)                            
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER      LOGBIT,NOLOG,NOVAR,NBITS,NFSTBT,NRATE,KR                     
      REAL         TERM,FACTOR,RUNI                                             
      CHARACTER*8  NAMVAR                                                       
      INTEGER      DATLOG,IWDSZ                                                 
      CHARACTER*8  PROJCT, UNITS                                                
      INTEGER      IRWFLG,IARRAY,IDATA                                          
      INTEGER      IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME           
      REAL         STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                  
                                                                                
C---Local variables:                                                            
      REAL RDATA(6400)                                                          
      LOGICAL READERR                                                           
      INTEGER NDREC, LEN, ITER, LOCWD, LW, I, J, K                              
                                                                                
C---External subroutines and functions used in this module:                     
      EXTERNAL ABORT, GBYTES                                                    
                                                                                
C---Loop through the first two physical data records dumping integer            
C   octal, integer packed, and decimal unpacked.                                
      DO 40, NDREC = 1, 2                                                       
         READERR = .FALSE.                                                      
   10    BUFFER IN (IUNIT,1) (IARRAY(1),IARRAY(4000))                           
                                                                                
C------Check for disk read errors, and if so, give messages and try             
C      one more time. Stop after 2 bad reads.                                   
C--------IF (ANINT(UNIT(IUNIT)) .GE. 0.0)  THEN                                 
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .GE.0.0) THEN                                                    
                                                                                
            WRITE(6,9895)                                                       
 9895       FORMAT(' ERROR CODE EXECUTED - SUBROUTINE DATA ' )                  
                                                                                
C-----------IF (ANINT(UNIT(IUNIT)) .EQ. 0.0) THEN                               
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.0.0) THEN                                                    
               WRITE (6, 9900) NDREC                                            
 9900          FORMAT(' EOF SENSED ON DATA FILE REC. NO.', I8)                  
               RETURN                                                           
            ENDIF                                                               
C-----------IF (ANINT(UNIT(IUNIT)) .EQ. 1.0)                                    
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.1.0)                                                         
     $         WRITE (6, 9905) NDREC                                            
 9905          FORMAT(' PARITY ERROR SENSED ON DATA FILE REC. NO.',I8)          
C-----------IF (ANINT(UNIT(IUNIT)) .EQ. 2.0)                                    
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.2.0)                                                         
     $         WRITE (6, 9910) NDREC                                            
 9910          FORMAT(' DISK MALFUNCTION DURING READ, FILE REC. NO.',I8)        
                                                                                
C------If the parity or disk error repeated, abort.                             
            IF ( READERR ) THEN                                                 
               WRITE ( 6, 9915) NDREC                                           
 9915          FORMAT(' DISK MALFUNCTION; PARITY ABORT, REC. NO.',I8)           
               CALL ABORT                                                       
            ENDIF                                                               
                                                                                
C------Set flag and set up for another read.                                    
            READERR = .TRUE.                                                    
            BACKSPACE IUNIT                                                     
            GOTO 10                                                             
         ENDIF                                                                  
                                                                                
C------Otherwise, a good read has occurred.                                     
C      Get length so it can be used for the GBYTES call.                        
         LEN=LENGTH(IUNIT)                                                      
                                                                                
         WRITE(6,9920) NDREC,LEN                                                
 9920    FORMAT('1DATA REC. NO. ',I4,', RECORD LENGTH',I8)                      
         ITER=LOGBIT*DATLOG/IWDSZ                                               
         CALL GBYTES(IARRAY,IDATA,0,IWDSZ,0,ITER)                               
                                                                                
C------Set up and write out record in octal                                     
         WRITE(6,9925) NDREC                                                    
 9925    FORMAT(/,' OCTAL DUMP OF RECORD # ',I3,/)                              
         WRITE(6,9930) (IDATA(I),I=1,ITER)                                      
 9930    FORMAT(10(2X,O11))                                                     
                                                                                
C------Set up and write out record in integer packed                            
         WRITE(6,9935) NDREC                                                    
 9935    FORMAT('1INTEGER (PACKED) DUMP OF RECORD # ',I3,/)                     
         WRITE(6,9940) (IDATA(I),I=1,ITER)                                      
 9940    FORMAT(10(2X,I10))                                                     
                                                                                
C------Now determine the unscaled parameters                                    
         DO 30 I=1,DATLOG                                                       
            DO 20 J=1,KR                                                        
               LOCWD=((NFSTBT(J)-1)/NBITS(J))+1                                 
               DO 20 K=1,NRATE(J)                                               
                  LW=LOCWD+K-1+(LOGBIT/IWDSZ)*(I-1)                             
                  RDATA(LW)=(IDATA(LW)/FACTOR(J))-TERM(J)                       
   20       CONTINUE                                                            
   30    CONTINUE                                                               
                                                                                
C------Write out record in decimal unpacked                                     
         WRITE(6,9945) NDREC                                                    
 9945    FORMAT('1DECIMAL (UNPACKED) DUMP OF RECORD # ',I3,/)                   
         WRITE(6,9950) (RDATA(K),K=1,ITER)                                      
 9950    FORMAT(10(2X,F10.2))                                                   
                                                                                
   40 CONTINUE                                                                  
                                                                                
C---Reposition at the beginning of the data.                                    
      BACKSPACE IUNIT                                                           
      BACKSPACE IUNIT                                                           
      RETURN                                                                    
      END                                                                       
                                                                                
C-----------------------------------------------------------------------        
                                                                                
      SUBROUTINE TDPRT                                                          
                                                                                
C---This subroutine loops through the dataset from the desired beginning        
C   time to end time, filling a one minute array of data which is then          
C   printed as it either becomes full or the end time is reached.               
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/READIT/IRWFLG,IARRAY(4000),IDATA(10000)                            
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $         NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC         
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR ,RUNI                                            
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      INTEGER     IRWFLG, IARRAY, IDATA                                         
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      INTEGER     IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME            
      REAL        STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                   
                                                                                
C---External subroutines used in this module:                                   
      EXTERNAL    ABORT, GBYTES, PRTHDR, VNMDUMP, RATPRT                        
                                                                                
C---Local variables:                                                            
      INTEGER IPOS, ITER, J, KL, I, IIAA, ILT, TFLG                             
      INTEGER LOCWD, LOGBEG, LW, NDREC, STFLAG                                  
      REAL    STOPDF, STRTDF, TIMESP, TIMEST                                    
      LOGICAL READERR                                                           
                                                                                
C                                                                               
C     VARIABLE DESCRIPTIONS USED IN SUBROUTINE TAPE DUMP PRINT OUT              
C                                                                               
C     DATLOG--LOGICAL DATA RECORDS PER PHYSICAL RECORD                          
C     IARRAY(4000)--HOLDS 1 PHYSICAL DATA RECORD                                
C     IDATA--ARRAY WHICH HOLD GBYTED IARRAY ARRAY                               
C     IIRATE--VALUE OF CURRENT RATE THAT IS BEING SEARCHED FOR                  
C     ILCK--COUNTER OF PARAMETERS FOUND                                         
C     ILT--INTEGER LOCATION OF TIME                                             
C     IMRATE--INPUT RATE FLAGS READ FROM DATA FILE                              
C     INRATE--VALUE OF RATES WHICH IMRATE FLAGS                                 
C     IPFLAG--PRINT FLAG TO PRINT HEADER ON TOP OF EACH PAGE                    
C     IPRPOS(10)--PRINTER POSTION.  CORRESPONDS TO LOCPOS                       
C     IRRPOS(10)--PRINTER POSTION; USED IN HIGH RATE LOCATIONING                
C     IRWFLG--FLAG WILL BE SET TO 1 IF SUBROUTINE DATA WAS USED                 
C     ITRNPG--COUNTER; PRINT 60 LINES PER PAGE                                  
C     IWDSZ--SIZE OF WORD                                                       
C     KR--NUMBER OF PARAMETERS                                                  
C     LOCPOS(150)--HOLDS POSITION OF PARAMETER IN MINUTE                        
C     LOCWD--LOCATION OF PARAMETER IN MINUTE                                    
C     LOGBEG--LOGICAL DATA RECORD TO BE PROCESSED IN PHYSICAL DATA REC          
C     LOGBIT--BITS PER LOGICAL DATA RECORD                                      
C     LW--LOCATION OF HIGH RATE PARAMETER IN MINUTE (LW<>LOCWD)                 
C     MINUTE(60,10000)--HOLDS ONE MINUTE OF DATA                                
C     NAMPOS(10)--NAME POSITION; EQUALS INDEX OF NAMVAR                         
C     NDREC--NUMBER OF RECORD CURRENTLY BEING PROCESSED                         
C     NUMSEC--INDEX USED IN MINUTE  TO INDICATE SECONDS                         
C     STFLAG--FLAG SET TO 1 IF START TIME IS FOUND                              
C     TIMESP--STOP TIME IN SECONDS                                              
C     TIMEST--START TIME IN SECONDS                                             
C                                                                               
                                                                                
C---Initialization                                                              
      TIMEST=STARTH*3600.+STARTM*60.+STARTS                                     
      TIMESP=STOPH*3600.+STOPM*60.+STOPS                                        
      STFLAG=0                                                                  
      NDREC=0                                                                   
                                                                                
   10 NUMSEC=0                                                                  
                                                                                
C---A buffered read will be used to obtain the data from that input tape.       
C   The logical record size is variable, as is the physical record for          
C   each project.                                                               
   15 READERR = .FALSE.                                                         
      NDREC=NDREC+1                                                             
   20 BUFFER IN (IUNIT,1) (IARRAY(1),IARRAY(4000))                              
                                                                                
C---Check for disk read errors, and if so, give messages and try                
C   one more time. Stop after 2 bad reads.                                      
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .GE.0.0) THEN                                                    
       IF (RUNI .EQ.0.0) THEN                                                   
                                                                                
C--------No error, an EOF has been found, now just print out any data           
C        that still resides within the minute array according to the            
C        options that are currently set.                                        
            IF (IHOWTO .EQ. 1) CALL VNMDUMP(IPOS,KPRT)                          
            IF (KPRT .EQ. 0 )  CALL RATPRT(ILT)                                 
            WRITE (6, 9000) NDREC                                               
 9000       FORMAT(' EOF SENSED IN TDPRT; DATA FILE REC. NO.', I8)              
            RETURN                                                              
         ENDIF                                                                  
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.1.0)                                                         
     $      WRITE (6, 9005) NDREC                                               
 9005       FORMAT(' PARITY ERROR SENSED IN TDPRT; FILE REC. NO.',I8)           
      RUNI= UNIT(IUNIT)                                                         
      IF (RUNI .EQ.2.0)                                                         
     $      WRITE (6, 9010) NDREC                                               
 9010       FORMAT(' DISK MALFUNCTION; TDPRT READ, FILE REC. NO.',I8)           
                                                                                
C---If the parity or disk error repeated, abort.                                
         IF ( READERR ) THEN                                                    
            WRITE ( 6, 9015) NDREC                                              
 9015       FORMAT(' DISK MALFUNCTION; PARITY ABORT, DATA REC. NO.',I8)         
            CALL ABORT                                                          
         ENDIF                                                                  
                                                                                
C----Set flag and set up for another read.                                      
         READERR = .TRUE.                                                       
         BACKSPACE IUNIT                                                        
         GOTO 20                                                                
      ENDIF                                                                     
                                                                                
C---Otherwise, a good read has occurred.                                        
      LOGBEG=1                                                                  
      ITER=LOGBIT/IWDSZ                                                         
                                                                                
C---Loop over the number of logical records                                     
  55  DO 70 KL=LOGBEG,DATLOG                                                    
         NUMSEC=NUMSEC+1                                                        
         BEGREC(NUMSEC)=NDREC                                                   
         IIAA=((KL-1)*LOGBIT)                                                   
         CALL GBYTES(IARRAY,IDATA,IIAA,IWDSZ,0,ITER)                            
                                                                                
C------Loop over the number of variables in each record                         
         DO 61 I=1,KR                                                           
            LOCWD=((NFSTBT(I)-1)/NBITS(I))+1                                    
            LOCPOS(I)=LOCWD                                                     
                                                                                
C---------Loop over the rate of each variable                                   
            DO 60 J=1,NRATE(I)                                                  
               LW=LOCWD+J-1                                                     
                                                                                
C---------Fill the minute array with the data values                            
               MINUTE(NUMSEC,LW)=(IDATA(LW)/FACTOR(I))-TERM(I)                  
C                                                                               
               TFLG = 0                                                         
C                                                                               
C-----Sets parameters to read by tape time.                                     
               IF (LTIME .EQ. 0) THEN                                           
                  IF (NAMVAR(I)(1:6) .EQ. 'TPTIME') THEN                        
                     ILT=LW                                                     
                     TFLG = 1                                                   
                  ENDIF                                                         
               ENDIF                                                            
                                                                                
C-------Sets parameters to read by processor time.                              
               IF (LTIME .EQ. 1) THEN                                           
                  IF (NAMVAR(I)(1:6) .EQ. 'PTIME ') THEN                        
                     ILT = LW                                                   
                     TFLG = 1                                                   
                  ENDIF                                                         
               ENDIF                                                            
                                                                                
C---------Check for a time gap in the data record.                              
C         (99999.0 is a Genpro Flagging convention used by RAFDMG)              
C         If such a gap exists, send a message to the output file               
C         and go on and process the next logical record of data.                
               IF (TFLG .EQ. 1) THEN                                            
                  IF (ABS(MINUTE(NUMSEC,ILT)-99999.0).LT.0.001) THEN            
                     WRITE(6,1020) NDREC                                        
 1020                FORMAT(' TIME GAP ENCOUNTED ON DATA FILE',                 
     $                                                  ' REC. NO.',I8)         
                     GOTO 70                                                    
                  ENDIF                                                         
                                                                                
C---------Calculate the time from the data record                               
                  TIME(NUMSEC,1)=INT(MINUTE(NUMSEC,ILT)/3600)                   
                  TIME(NUMSEC,2)=INT(MINUTE(NUMSEC,ILT)/60)                     
     $                                             -TIME(NUMSEC,1)*60.          
                  TIME(NUMSEC,3)=INT(MINUTE(NUMSEC,ILT)+.00001)                 
     $                        -TIME(NUMSEC,1)*3600- TIME(NUMSEC,2)*60.          
               ENDIF                                                            
                                                                                
  60        CONTINUE                                                            
  61     CONTINUE                                                               
                                                                                
C------Calculate the differences between the current data record time           
C      and the start and stop times that the user has supplied                  
         STRTDF=MINUTE(NUMSEC,ILT)-TIMEST                                       
         STOPDF=MINUTE(NUMSEC,ILT)-TIMESP                                       
                                                                                
C------See if the start time has been found, if not, go process the next        
C      logical record of data.                                                  
         IF (STFLAG.NE.1) THEN                                                  
            IF (NUMSEC.EQ.1) THEN                                               
               IF ((STRTDF .GT. 0.0) .OR. (ABS(STRTDF).LT.0.001)) THEN          
                  STFLAG=1                                                      
               ELSE                                                             
                  NUMSEC=0                                                      
                  GOTO 70                                                       
               ENDIF                                                            
            ENDIF                                                               
         ENDIF                                                                  
                                                                                
C------See if the end time has been found, if so, go and print out any          
C      data that is left in the minute array and then return.                   
         IF ((STOPDF .GT. 0.0) .OR. (ABS(STOPDF).LT.0.001)) THEN                
            IF (NUMSEC.GT.1) THEN                                               
               IF (IHOWTO .EQ. 1) CALL VNMDUMP(IPOS,KPRT)                       
               IF (KPRT .EQ. 0 )  CALL RATPRT(ILT)                              
            ENDIF                                                               
            RETURN                                                              
         ENDIF                                                                  
                                                                                
C------Check if the minute array is full and if so, print out the data.         
         IF (NUMSEC.EQ.60) THEN                                                 
            LOGBEG=KL+1                                                         
            IF(LOGBEG.GT.DATLOG) LOGBEG=1                                       
            IF (IHOWTO .EQ. 1) CALL VNMDUMP(IPOS,KPRT)                          
            IF (KPRT .EQ. 0 )  CALL RATPRT(ILT)                                 
            NUMSEC = 0                                                          
         ENDIF                                                                  
   70 CONTINUE                                                                  
                                                                                
C---If the start time has been found, then continue processing data             
C   otherwise, reinitialize and start over.                                     
      IF (STFLAG.EQ.1) THEN                                                     
         GOTO 15                                                                
       ELSE                                                                     
         GOTO 10                                                                
      ENDIF                                                                     
                                                                                
                                                                                
C---The normal exit to this routine is either through the EOF sensing           
C   or through the end time checking.  Both those exits are above and           
C   hence, the reason why this program has no return coupled with the           
C   end statement                                                               
                                                                                
      END                                                                       
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE VNMDUMP(IPOS,KPRT)                                             
                                                                                
C---This subroutine will search the input tape variables and flag               
C   all matches that are found with the input tape variable names.              
C   Once all of the variables are found, the routine calls a print              
C   routine that actually prints out the desired variables.                     
C   This enables the user to dump by variable name rather than by rate.         
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     1NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC                  
      COMMON/PNRTR2/NAMDMP(300)                                                 
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR,RUNI                                             
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
                                                                                
C---Argument list variable definitions:                                         
      INTEGER IPOS, KPRT                                                        
                                                                                
C---Local variable declarations:                                                
      INTEGER I, J                                                              
                                                                                
C---External routines to this subroutine:                                       
      EXTERNAL  HORZPRNT                                                        
                                                                                
C                                                                               
C     VARIABLE DESCRIPTIONS USED IN SUBROUTINE CELIA CHEN TAPE DUMP             
C                                                                               
C     KR--NUMBER OF PARAMETERS                                                  
C     LOCPOS(200)--HOLDS POSITION OF PARAMETER IN MINUTE                        
C     LOGBEG--LOGICAL DATA RECORD TO BE PROCESSED IN PHYSICAL DATA REC          
C     MINUTE(60,10000)--HOLDS ONE MINUTE OF DATA                                
C     NUMSEC--INDEX USED IN MINUTE  TO INDICATE SECONDS                         
C                                                                               
                                                                                
      IPOS=0                                                                    
                                                                                
C---Search the NAMVAR array to see if any variable names on the input           
C   tape match those that are desired to be dumped. If so, determine            
C   its' location within the minute array.                                      
                                                                                
      DO 20 J=1,KR                                                              
         DO 10 I=1,INUM                                                         
            IF(NAMVAR(J).NE. NAMDMP(I)) GOTO 10                                 
            IPOS=IPOS+1                                                         
            LOCPOS(IPOS)=J                                                      
            LOCMIN(IPOS)=NFSTWD(J)                                              
            GOTO 20                                                             
  10     CONTINUE                                                               
  20  CONTINUE                                                                  
                                                                                
C---If no variables matched, output an error message and return.                
      IF (IPOS.EQ.0) THEN                                                       
         PRINT 9000                                                             
 9000    FORMAT('1NO MATCH WAS FOUND FOR ADS CALIBRATED TAPE DUMP ',            
     $                                                     'INPUT FILE')        
         RETURN                                                                 
      ENDIF                                                                     
                                                                                
C---If a horizontal rather than columnar print is desired, call the             
C   HORZPRNT routine, otherwise return to the TDPRT routine where               
C   the variables will be printed in columns.                                   
      IF(KPRT.EQ.1) CALL HORZPRNT(IPOS)                                         
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE HORZPRNT(IPOS)                                                 
                                                                                
C---This subroutine will print out the desired information in a                 
C   horizontal format rather than the implicit columnar output                  
C   format that resides in the RATPRT routine.                                  
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP1/DATLOG,IWDSZ                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     1NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC                  
      COMMON/PNRTR2/NAMDMP(300)                                                 
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR ,RUNI                                            
      CHARACTER*8 NAMVAR                                                        
      INTEGER     DATLOG, IWDSZ                                                 
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
                                                                                
C---Argument list variable declarations:                                        
      INTEGER IPOS                                                              
                                                                                
C---Local variables:                                                            
      INTEGER  I, J, K, IST, ISP                                                
                                                                                
      DO 20 I=1,NUMSEC                                                          
         PRINT 9000,BEGREC(I),TIME(I,1),TIME(I,2),TIME(I,3)                     
 9000    FORMAT(//,' --------------RECORD NUMBER:',I5,5X,'TIME:  ',             
     $           I2,':',I2,':',I2,'  ---------------------------------')        
         DO 10 J=1,IPOS                                                         
            PRINT 9005,NAMVAR(LOCPOS(J)),NRATE(LOCPOS(J))                       
 9005       FORMAT(/,15X,'PARAMETER = ',A8,5X,'RATE = ',I4)                     
            IST=LOCMIN(J)                                                       
            ISP=LOCMIN(J)-1+NRATE(LOCPOS(J))                                    
            PRINT 9010,(MINUTE(I,K),K=IST,ISP)                                  
 9010       FORMAT(10(1X,F10.3,1X))                                             
   10    CONTINUE                                                               
   20 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
C-----------------------------------------------------------------------        
                                                                                
      SUBROUTINE ENPUT                                                          
                                                                                
C---This subroutine will read all of the input from the Unit-5 input            
C   file which is appended to the deck. All the information read in is          
C   passed to the rest of the program and subroutines through common            
C   blocks.                                                                     
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $    NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC              
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/READIT/IRWFLG,IARRAY(4000),IDATA(10000)                            
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
                                                                                
C---Common area variable definitions:                                           
      CHARACTER*8  NAMDMP                                                       
      INTEGER INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC                
      REAL MINUTE,RUNI                                                          
      INTEGER IRWFLG, IARRAY, IDATA                                             
      INTEGER IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME                
      REAL STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                          
                                                                                
C---Local variable definitions:                                                 
      CHARACTER*8  DUMMY(10)                                                    
      INTEGER I                                                                 
                                                                                
 9900 FORMAT(10A8)                                                              
 9910 FORMAT(I5)                                                                
 9920 FORMAT(3F5.0)                                                             
 9930 FORMAT(13I5)                                                              
 9940 FORMAT(8(1X,A8))                                                          
                                                                                
C   The variable DUMMY is used to read the explanational text in the            
C   input file.                                                                 
      READ(5,9900) DUMMY                                                        
      READ(5,9900) DUMMY                                                        
      READ(5,9900) DUMMY                                                        
C   Determine unit number                                                       
      READ(5,9910) IUNIT                                                        
                                                                                
C---Read which version of Genpro wrote the tape, and which library.             
      READ(5,9900) DUMMY                                                        
      READ(5,9910) GNPVER                                                       
      READ(5,9900) DUMMY                                                        
      READ(5,9900) DUMMY                                                        
      READ(5,9910) LIBVER                                                       
                                                                                
C---Read whether or not to dump the first two data records as octal,            
C   integer packed, and decimal unpacked.                                       
      READ(5,9900) DUMMY                                                        
      READ(5,9910) IRWFLG                                                       
                                                                                
C---Read whether to use tape time or processer time.                            
      READ(5,9900) DUMMY                                                        
      READ(5,9910) LTIME                                                        
                                                                                
C---Read in start and stop time.                                                
      READ(5,9900) DUMMY                                                        
      READ(5,9920) STARTH,STARTM,STARTS                                         
      READ(5,9920) STOPH,STOPM,STOPS                                            
                                                                                
C---Read whether to dump by variable rate or name.                              
      READ(5,9900) DUMMY                                                        
      READ(5,9910) IHOWTO                                                       
                                                                                
C---Read the rates to be printed, only significant if dumping by rate.          
      READ(5,9900) DUMMY                                                        
      READ(5,9900) DUMMY                                                        
      READ(5,9930) (IMRATE(I),I=1,13)                                           
                                                                                
C---See if a horizontal print or a verical print is desired.                    
      READ(5,9900) DUMMY                                                        
      READ(5,9910) KPRT                                                         
                                                                                
C---Read the number of variables and the variable names the user wishes         
C   to print, only significant if dumping by name.                              
      IF (IHOWTO .GE. 1.0) THEN                                                 
         READ(5,9900) DUMMY                                                     
         READ(5,9910) INUM                                                      
         READ(5,9900) DUMMY                                                     
         READ(5,9940) (NAMDMP(I),I=1,INUM)                                      
      ENDIF                                                                     
                                                                                
      WRITE(6,9945)                                                             
 9945 FORMAT(///,'1')                                                           
                                                                                
      RETURN                                                                    
      END                                                                       
                                                                                
C---------------------------------------------------------------------BW        
                                                                                
      SUBROUTINE RATPRT(ILT)                                                    
                                                                                
C---This subroutine will print out the desired rates that have been read        
C   read from the input file in a columnar format. If a dump by variable        
C   name has been requested, provisions have been made to dump them             
C   in the columnar format.                                                     
                                                                                
      IMPLICIT NONE                                                             
                                                                                
C---Common areas:                                                               
      COMMON/HDINFO1/LOGBIT,NOLOG,NOVAR,TERM(300),FACTOR(300)                   
     $                         ,NBITS(300),NFSTBT(300),NRATE(300),KR            
      COMMON/HDINFO2/NAMVAR(300)                                                
      COMMON/TDUMP2/PROJCT(5), UNITS(300)                                       
      COMMON/PNRTR1/INUM,LOCPOS(300),LOCMIN(300),                               
     $         NFSTWD(300),MINUTE(60,4000),BEGREC(60),TIME(60,3),NUMSEC         
      COMMON/PNRTR2/NAMDMP(300)                                                 
      COMMON/INPARMS/IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE(13),           
     $         LTIME, STARTH, STARTM, STARTS, STOPH, STOPM, STOPS               
                                                                                
C---Common area variable definitions:                                           
      INTEGER     LOGBIT, NOLOG, NOVAR, NBITS, NFSTBT, NRATE, KR                
      REAL        TERM, FACTOR,RUNI                                             
      CHARACTER*8 NAMVAR                                                        
      CHARACTER*8 PROJCT, UNITS                                                 
      INTEGER     INUM, LOCPOS, LOCMIN, NFSTWD, BEGREC, TIME, NUMSEC            
      REAL        MINUTE                                                        
      CHARACTER*8 NAMDMP                                                        
      INTEGER     IUNIT, LIBVER, GNPVER, IHOWTO, KPRT, IMRATE, LTIME            
      REAL        STARTH, STARTM, STARTS, STOPH, STOPM, STOPS                   
                                                                                
C---Argument list variable definitions:                                         
      INTEGER ILT                                                               
                                                                                
C---External subroutines used in this module:                                   
      EXTERNAL    ABORT, GBYTES, PRTHDR                                         
                                                                                
C---Local variables:                                                            
      INTEGER HRS, MIN, IIRATE, IKL, ILCK, IPAGE, IPFLAG                        
      INTEGER ITRNPG, J, JJ, K, KK, L                                           
      INTEGER IPRPOS(10), NAMPOS(10), INRATE(13), IRRPOS(10)                    
      REAL    SEC, SECD                                                         
                                                                                
      DATA INRATE / 1, 5, 10, 15, 20, 21, 25, 50, 62, 70, 150, 210, 250/        
                                                                                
C---Initialization                                                              
      IIRATE=0                                                                  
                                                                                
C---Loop over all the possible rates                                            
      DO 140 L=1,13                                                             
         IF (IMRATE(L).EQ.0.AND.IHOWTO.EQ.0) GOTO 140                           
         IPFLAG=1                                                               
         IIRATE=INRATE(L)                                                       
                                                                                
C---Check if IIRATE is a rate that is used in this project                      
         ILCK=0                                                                 
                                                                                
C---Find parameters which have requested rate                                   
         DO 130 K=1,KR                                                          
            IF (K.EQ.KR.AND.IIRATE.NE.NRATE(K)) GOTO 100                        
            IF(IIRATE.NE.NRATE(K)) GOTO 130                                     
            IF (IHOWTO.EQ.1) THEN                                               
               DO 90 IKL=1,INUM                                                 
                  IF (NAMDMP(IKL).NE.NAMVAR(K)) GOTO 90                         
                  ILCK=ILCK+1                                                   
                  IPRPOS(ILCK)=NFSTWD(K)                                        
                  NAMPOS(ILCK)=K                                                
   90          CONTINUE                                                         
            ELSE                                                                
               ILCK=ILCK+1                                                      
               IPRPOS(ILCK)=NFSTWD(K)                                           
               NAMPOS(ILCK)=K                                                   
            ENDIF                                                               
            IF (ILCK.EQ.10.OR.K.EQ.KR) GOTO 100                                 
            GOTO 130                                                            
  100       IF (ILCK.LE.0) GOTO 140                                             
                                                                                
C---Loop over 60 seconds or the ending time segment.                            
            DO 120 J=1,NUMSEC                                                   
               HRS=INT(MINUTE(J,ILT)/3600.)                                     
               MIN=INT(((MINUTE(J,ILT)/3600.)-HRS)*60.)                         
               SEC=((((MINUTE(J,ILT)/3600.)-FLOAT(HRS))*60.)                    
     $                                                 -FLOAT(MIN))*60.         
                                                                                
C---Print the page header                                                       
               DO 110 JJ=1,IIRATE                                               
                  IF (ITRNPG.EQ.60.OR.IPFLAG.EQ.1) THEN                         
                     IPAGE=IPAGE+1                                              
                     PRINT 1040                                                 
 1030                FORMAT(F10.3,A8)                                           
 1040                FORMAT(1H1)                                                
                     PRINT 2000,PROJCT,IPAGE                                    
 2000                FORMAT(37X,5A8,39X,'PAGE',2X,I5)                           
                     PRINT 2010,(NAMVAR(NAMPOS(KK)),KK=1,ILCK)                  
 2010                FORMAT('  HR MI SEC       ',10(3X,A8))                     
                     PRINT 2020,(UNITS(NAMPOS(KK)),KK=1,ILCK)                   
 2020                FORMAT(20X,10(3X,A8))                                      
                     ITRNPG=0                                                   
                     IPFLAG=0                                                   
                  ENDIF                                                         
                  SECD=SEC+FLOAT(JJ-1)/FLOAT(IIRATE)                            
                                                                                
C---Set IRRPOS to location of parameters to be printed which are located        
C   in the minute array.                                                        
                  DO 105 KK=1,10                                                
  105                IRRPOS(KK)=IPRPOS(KK)+JJ-1                                 
                  IF ((SECD.GE.60.).OR.(ABS(SECD-60.).LE.0.001)) THEN           
                     SEC = 0                                                    
                     SECD = 0.0                                                 
                     MIN = MIN + 1                                              
                  ENDIF                                                         
                  PRINT 2030,HRS,MIN,SECD,                                      
     $                                 (MINUTE(J,IRRPOS(KK)),KK=1,ILCK)         
 2030             FORMAT(2X,I2.2,':',I2.2,':',F6.3,2X,10(1X,F10.3))             
                  ITRNPG=ITRNPG+1                                               
  110          CONTINUE                                                         
  120       CONTINUE                                                            
            IPFLAG=1                                                            
            ILCK=0                                                              
  130    CONTINUE                                                               
  140 CONTINUE                                                                  
                                                                                
                                                                                
      RETURN                                                                    
      END                                                                       
