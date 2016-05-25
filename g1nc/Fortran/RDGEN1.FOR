      SUBROUTINE RDDATA (INUNIT,T1,T2,NRECRDS,NPARMS,INDEX,ARRAY,NHDR,  &
     & IFLAG,M,N,NST)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!        THIS SUBROUTINE IS TO BE USED IN CONJUNCTION WITH SUBROUTINES C
!        RDHDR, AND RDREC0.  THIS ROUTINE ACTS AS A DRIVER FOR THE     C
!        OTHERS, ACCEPTING A START AND FINISH TIME AND A MAXIMUM NUMBERC
!        OF RECORDS TO BE RETURNED.  THESE RECORDS CONSIST ONLY OF     C
!        SELECTED PARAMETERS, GIVEN BY THE VALUES OF THE ARRAY INDEX.  C
!                                                                      C
!        ENTRY RDDAT2 ALLOWS MORE RECORDS TO BE READ FROM THE FILE     C
!        WITHOUT TRYING TO READ A HEADER RECORD.                       C
!                                                                      C
!                                                                      C
!        THE CALL IS                                                   C
!             CALL RDDATA (INUNIT,T1,T2,NRECRDS,NPARMS,INDEX,ARRAY,    C
!                          NHDR,IFLAG)                                 C
!        INUNIT    -  LOGICAL UNIT TAPE IS MOUNTED ON                  C
!        T1        -  START TIME FOR SAMPLING.                         C
!        T2        -  END TIME OF SAMPLING PERIOD.                     C
!        NRECRDS   -  MAXIMUM NUMBER OF RECORDS TO BE RETURNED.  IF YOUC
!                     WANT ALL THE RECORDS BETWEEN T1 AND T2, BE SURE  C
!                     NRECRDS >= (T2-T1)/CYCTIME +1.                   C
!                     ON RETURN, NRECRDS HOLDS THE NUMBER OF RECORDS   C
!                     ACTUALLY RETURNED.                               C
!        NPARMS    -  NUMBER OF PARAMETERS TO BE RETURNED FOR EACH     C
!                     RECORD.                                          C
!        INDEX     -  ARRAY CONTAINING THE LOCATIONS OF THE PARAMETERS C
!                     WITHIN A RECORD TO BE RETURNED.  THESE NEED BE   C
!                     IN NO SPECIAL ORDER.                             C
!        ARRAY     -  THE ARRAY TO CONTAIN THE RETURNED VALUES.  IT IS C
!                     A TWO-DIMENSIONAL ARRAY. WITH THE FIRST INDEX    C
!                     REFERRING TO RECORD NUMBER, AND THE SECOND TO    C
!                     PARAMETER NUMBER (NOT LOCATION).                 C
!        NHDR      -  GIVES THE NUMBER OF PHYSICAL RECORDS IN THE HEADER
!        IFLAG     -  AS INPUT, CONTROLS THE PRINTING OF THE HEADER    C
!                     INFORMATION.  IF IFLAG = 1, THE PRINTING WILL BE C
!                     SUPPRESSED.                                      C
!                     AS OUTPUT, IT INDICATES THE COMPLETION CODE.     C
!                     IFLAG=0, NORMAL RETURN, WITH T2 LIMITING SEARCH. C
!                     IFLAG=1, NORMAL RETURN, NRECRDS LIMITING SEARCH  C
!                     IFLAG=2, T1 AND T2 HAVE INVALID TIMES.           C
!                     IFLAG=3, NRECRDS OR NPARMS IS < ZERO.            C
!                     IFLAG=4, EOF WAS REACHED ON A HEADER READ.       C
!                     IFLAG=5, UNRECOVERABLE PARITY ERROR OCCURRED.    C
!                     IFLAG=6, EOF WAS REACHED DURING DATA READ.       C
!                     IFLAG=7, CATASTROPHIC ERRORS HAVE OCCURRED IN    C
!                              EXECUTION.                              C
!        M         -  FIRST DIMENSION OF INPUT ARRAY "ARRAY"           C
!        N         -  SECOND DIMENSION OF INPUT ARRAY "ARRAY"          C
!        NST       -  RECORDS WILL BE STORED IN "ARRAY" BEGINNING AT   C
!                     ARRAY(NST,1).                                    C
!                                                                      C
!        WRITTEN BY GEORGE LEHMANN, JR.                                C
!                   OCTOBER,1976                                       C
!                                                                      C
!        MODIFIED TO RUN ON THE CRAY-1 BY MIKE DANIELS                 C
!                   JANUARY, 1983                                      C
!                                                                      C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      DIMENSION INDEX(NPARMS),ARRAY(M,N),IRATE(100),                    &
     &  SCALE(100),BIAS(100),INARRAY(1500),DATARAY(1500)
      DATA  ISWCH /0/
      IF(T2.LE.T1) GO TO 900
      IF (NRECRDS.LE.0.OR.NPARMS.LE.0) GO TO 910
      IF (ISWCH.EQ.1) GO TO 100
      ISWCH = 1
!
!        PROCESS HEADER INFORMATION
!
      CALL RDHDR (INUNIT,IRATE,SCALE,BIAS,NP,NPER,CTIME,NR,NW,INARRAY,  &
     & NHDR,IFLAG)
      IFLAG=IFLAG+1
      GO TO (100,920,930,90),IFLAG
   90 WRITE (6,91) INUNIT
!
!        THIS ENTRY POINT ALLOWS READING DATA WITHOUT FIRST READING
!        THE HEADER
!
      ENTRY RDDAT2
  100 N=NST-1
      T0=T1-FLOAT(NR)*CTIME
!
!        SEARCH FOR THE RIGHT RECORDS
!
  110 IFLAG=1
  120 CALL RDREC (INUNIT,IRATE,SCALE,BIAS,NP,NPER,NR,DATARAY,INARRAY,   &
     & IFLAG)
      IF (IFLAG.EQ.0) GO TO 200
      IFLAG=IFLAG-89
      GO TO (940,950,950,930,190), IFLAG
  190 WRITE (6,91) INUNIT
  200 IF (DATARAY(1).LT.T0) GO TO 110
      IF (DATARAY(1).LT.T1) GO TO 120
      IF (DATARAY(1).GT.T2) GO TO 800
!
!        MOVE PARAMETERS INTO NEW ARRAY ELEMENT
!
      N=N+1
      DO 300 J=1,NPARMS
      K=INDEX(J)
  300 ARRAY(N,J)=DATARAY(K)
      IF (N.GE.NRECRDS) GO TO 810
      GO TO 120
!
!        FINISH UP AND RETURN
!
  800 NRECRDS=N
      IFLAG=0
      RETURN
  810 NRECRDS=N
      IFLAG=1
      RETURN
!
  900 IFLAG=2
      WRITE (6,905)
      RETURN
!
  910 IFLAG=3
      WRITE (6,915)
      RETURN
!
  920 IFLAG=4
      ISWCH = 0
      NRECRDS=N
      WRITE (6,925)
      RETURN
!
  930  IFLAG=5
      WRITE (6,935)
      RETURN
!
  940 IFLAG=6
      ISWCH = 0
      NRECRDS=N
      WRITE (6,945)
      RETURN
!
  950 IFLAG=7
      WRITE (6,955)
      RETURN
!
   91 FORMAT ("0RECOVERABLE PARITY ERROR OCCURRED ON UNIT ",I2)
  905 FORMAT ("-  >>>ERROR<<<  INVALID VALUES FOR TIMES T1 AND T2")
  915 FORMAT ("-  >>>ERROR<<<  NUMBER OF RECORDS OR NUMBER OF PARAMETERS&
     & REQUESTED IS NON-POSITIVE NUMBER")
  925 FORMAT ("-  END OF VOLUME")
  935 FORMAT ("-  >>>ERROR<<<  PARITY ERROR OCCURRED DURING TAPE READ")
  945 FORMAT ("-  END OF FILE")
  955 FORMAT ("-  >>>>>ERROR<<<<< CATASTROPHIC ERROR HAS OCCURRED")
      END
!
      SUBROUTINE RDHDR   (INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,       &
     &  CYCTIME,NREC,NWORDS,INARRAY,NHDR,IFLAG)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!                                                                      C
!        THIS SUBROUTINE READS A HEADER BLOCK, PRINTS THE INFORMATION  C
!        CONTAINED IN THAT BLOCK, DECODES AND RETURNS CERTAIN PARAMETERS
!        TO THE CALLING PROGRAM.                                       C
!                                                                      C
!        THE CALL IS                                                   C
!            CALL RDHDR   (INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,     C
!                           CYCTIME,NREC,NWORDS,INARRAY,IFLAG)         C
!        INUNIT    -  LOGICAL UNIT NUMBER OF THE TAPE USED             C
!        IRATE     -  ARRAY TO CONTAIN RETURNED VALUES OF SAMPLING RATEC
!        SCALE     -  ARRAY TO CONTAIN RETURNED VALUES FOR RESCALING   C
!                     THE DATA.                                        C
!        BIAS      -  ARRAY TO CONTAIN THE RETURNED VALUES FOR         C
!                     CORRECTING THE DATA OFFSET.                      C
!        NPARMS    -  RETURNS THE NUMBER OF PARAMETERS PER LOGICAL REC C
!        NPERCYC   -  RETURNS THE NUMBER OF PARAMETERS TAKEN PER CYCLE C
!        CYCTIME   -  RETURNS THE CYCLE TIME USED                      C
!        NREC      -  RETURNS THE NUMBER OF LOGICAL RECORDS PER PHYSICAL
!                     RECORD                                           C
!        NWORDS    -  RETURNS THE NUMBER OF WORDS OF MEMORY OCCUPIED   C
!                     BY A PHYSICAL RECORD                             C
!        INARRAY   -  WORKING ARRAY FOR THE SUBROUTINE, MUST BE AT     C
!                     LEAST 1500 WORDS.                                C
!        NHDR      -  GIVES NUMBER OF PHYSICAL RECORDS IN THE HEADER   C
!        IFLAG     -  AS INPUT, SUPPRESSES PRINT IF IFLAG=1            C
!                  -  AS OUTPUT, RETURNS COMPLETION CODE AS FOLLOWS    C
!                     IFLAG=0 INDICATES NORMAL RETURN                  C
!                     IFLAG=1 INDICATES EOF SENSED DURING OPERATION    C
!                     IFLAG=2 INDICATES UNRECOVERABLE PARITY ERROR     C
!                             OCCURED DURING OPERATION.                C
!                     IFLAG=3 INDICATES RECOVERABLE PARITY ERROR WAS   C
!                             DETECTED.                                C
!                                                                      C
!        WRITTEN BY GEORGE LEHMANN, JR.                                C
!                   OCTOBER,1976                                       C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      DIMENSION IRATE(*),SCALE(*),BIAS(*),INARRAY(*),LABEL(7)
      INTEGER ASCII(64),WKARRAY(16000),FLG, status
      DIMENSION ARRAY( 1500)
      COMMON /TPCORR/ ICORR1
      COMMON /LOCATN/ LOCAT(500)
      LOGICAL ICORR1
      DATA ASCII/O'72',O'101',O'102',O'103',O'104',O'105',O'106',O'107',&
     &O'110',O'111',O'112',O'113',O'114',O'115',O'116',O'117',O'120',   &
     &O'121',O'122',O'123',O'124',O'125',O'126',O'127',O'130',O'131',   &
     &O'132',O'60',O'61',O'62',O'63',O'64',O'65',O'66',O'67',O'70',     &
     &O'71',O'53',O'55',O'52',O'57',O'50',O'51',O'44',O'75',O'40',O'54',&
     &O'56',O'43',O'133',O'135',O'45',O'42',O'137',O'41',O'46',O'47',   &
     &O'77',O'74',O'76',O'100',O'134',O'136',O'73'/
      NPARITY=0
      IF (IFLAG.EQ.0) WRITE(6,11)
!
!        FETCH THE HEADER RECORDS FROM TAPE
!
      IST = 1
      ICORR1 = NHDR.EQ.3
      DO 100 I=1,NHDR
    7 CONTINUE
      READ( UNIT=INUNIT, FMT='(A)', IOSTAT= status, ADVANCE='NO', SIZE=ILEN) INARRAY(1:1500)
   10 CONTINUE
      IF( status ) 20, 991, 15
   15 IF(NPARITY.NE.0) GO TO 995
      WRITE (6,996)
      NPARITY=1
      BACKSPACE INUNIT
      GO TO 7
   20 CONTINUE
  100 IST=IST+ILEN
      ITER=INT((IST-1)*64/6)+1
      CALL GBYTES(INARRAY,WKARRAY,0,6,0,ITER)
      DO 102 J1=1,ITER
      IF (WKARRAY(J1).GT.63) THEN
         WKARRAY(J1)=ASCII(45)
      ELSE
         WKARRAY(J1)=ASCII(WKARRAY(J1)+1)
      ENDIF
  102 END DO
      CALL SBYTES(INARRAY,WKARRAY,0,8,0,ITER)
      IF(IFLAG.EQ.0) WRITE (6,150) (INARRAY(K),K=1,138)
!
!        READ RECORD DESCRIPTOR INFORMATION
!
!      DECODE (121,170,INARRAY(23)) NPARMS,NPERCYC,CYCTIME
!  170 FORMAT (I3,67X,I4,40X,F5.3)
      READ(INARRAY(23), '(I3,67X,I4,40X,F5.3)') NPARMS,NPERCYC,CYCTIME
      READ( INARRAY( 39), '(1X,I2,26X,I4,70X,I3)') NPARMS,NPERCYC,CYCTIME
!
!        READ AND PRINT THE REMAINDER OF THE HEADER
!
      IF (IFLAG.EQ.0) WRITE (6,195)
      FLG=0
      ID=138
      LOC=1
      DO 200 I=1,NPARMS
      IF (FLG.EQ.0) THEN
         READ( INARRAY( ID), '(4X,I3,1X,I4,5X,7A8,10X,F7.1,3X,F7.1)')   &
     &        J,IRATE(I),LABEL,SCALE(I),BIAS(I)
      ELSE
         READ( INARRAY( ID+1), '(I3,1X,I4,5X,7A8,10X,F7.1,3X,F7.1)')    &
     &        J,IRATE(I),LABEL,SCALE(I),BIAS(I)
      ENDIF
      IF (I.EQ.1) GO TO 190
      LOC=LOC+IRATE(I-1)
  190 IF(IFLAG.EQ.0) WRITE (6,220) J,LOC,IRATE(I),LABEL,SCALE(I),BIAS(I)
      LOCAT(I)=LOC
      IF (FLG.EQ.0) THEN
         ID=ID+12
         FLG=1
      ELSE
         ID=ID+13
         FLG=0
      ENDIF
  200 END DO
  202 CALL RDREC0 (INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,NREC,ARRAY,   &
     &INARRAY,IFLAG)
      IFLAG=0
      IF(NPARITY.NE.0) IFLAG=3
      RETURN
!
!        ERROR MESSAGES
!
  991 WRITE (6,992)
      IFLAG=1
      RETURN
!
  995 WRITE (6,996)
      IFLAG=2
      RETURN
!
   11 FORMAT ("1")
  150 FORMAT (10(1X,14A8,/))
  180 FORMAT (1X,I2,26X,I4,70X,I3)
  195 FORMAT (/," ITEM   LOCATION   RATE",17X,"DESCRIPTION",20X,"NAME", &
     &4X,"UNITS",18X,"SCALE",12X,"BIAS",/)
  210 FORMAT (4X,I3,1X,I4,5X,7A8,10X,F7.1,3X,F7.1)
  215 FORMAT (I3,1X,I4,5X,7A8,10X,F7.1,3X,F7.1)
  220 FORMAT (1X,I3,I8,I9,5X,7A8,17X,F9.2,5X,F9.2)
  992 FORMAT ("-  >>>ERROR<<<  EOF SENSED ON HEADER READ")
  996 FORMAT ("-  >>>ERROR<<<  PARITY ERROR SENSED ON HEADER READ")
      END
!
      SUBROUTINE RDREC0  (INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,NREC,  &
     &  ARRAY,INARRAY,IFLAG)
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                      C
!        THIS SUBROUTINE IS USED TO FETCH LOGICAL RECORDS FROM TAPE.   C
!        IT HAS TWO MODES OF OPERATION, GOVERNED BY PARAMETER IFLAG.   C
!        IN SCROLL MODE, THE NEXT SEQUENTIAL LOGICAL RECORD IS         C
!        RETURNED.  IN SELECT MODE, LOGICAL RECORD N IS RETURNED.      C
!        IF THE LOGICAL RECORD REQUESTED COMES BEFORE THE CURRENT      C
!        RECORD POINTER, THE NEXT PHYSICAL RECORD IS READ TO OBTAIN    C
!        THE CORRECT LOGICAL RECORD.                                   C
!                                                                      C
!        A CALL TO RDREC0 INITIALIZES THE POINTER AND ENABLES THE READ C
!        OPERATION.  THIS CALL IS NORMALLY MADE BY SUBROUTINE RDHDR.   C
!        ENTRY POINT RDREC IS THE CALL USED TO READ A LOGICAL RECORD.  C
!                                                                      C
!        THE CALL IS                                                   C
!          CALL RDREC (INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,NREC,    C
!                      ARRAY,INARRAY,IFLAG                             C
!                                                                      C
!        INUNIT    -  THE NUMBER OF THE LOGICAL UNIT THE TAPE IS MOUNTED
!                     ON                                               C
!        IRATE     -  THE ARRAY CONTAINING THE SAMPLE RATES FOR THE    C
!                     PARAMETERS (RETURNED FROM RDHDR)                 C
!        SCALE     -  THE ARRAY CONTAINING THE SCALING FACTORS FOR THE C
!                     PARAMETERS (RETURNED FROM RDHDR)                 C
!        BIAS      -  THE ARRAY CONTAINING THE OFFSET VALUES FOR THE   C
!                     PARAMETERS (RETURNED FROM RDHDR)                 C
!        NPARMS    -  NUMBER OF PARAMETERS TO BE FOUND IN A LOGICAL    C
!                     RECORD                                           C
!        NPERCYC   -  TOTAL NUMBER OF DATA ITEMS PER LOGICAL RECORD    C
!        NREC      -  NUMBER OF LOGICAL RECORDS PER PHYSICAL RECORD    C
!        ARRAY     -  REAL ARRAY IN WHICH THE PROPERLY BIASED AND      C
!                     SCALED DATA ITEMS WILL BE RETURNED               C
!        INARRAY   -  WORKING ARRAY FOR PHYSICAL RECORD STORAGE.  MUST C
!                     BE AT LEAST 1500 WORDS LONG.                     C
!        IFLAG     -  AS INPUT, CONTROLS THE MODE OF OPERATION AND THE C
!                     LOGICAL RECORD TO BE RETURNED                    C
!                     IFLAG=0 SELECTS SCROLL MODE                      C
!                     IFLAG=1,2,...,NPARMS SELECTS SELECT MODE, WITH   C
!                     THE VALUE OF IFLAG CHOOSING THE LOGICAL RECORD   C
!                     TO BE RETURNED.  IFLAG<0 OR IFLAG>NPARMS WILL    C
!                     PRODUCE AN ERROR MESSAGE.                        C
!                     AS OUTPUT, IFLAG INDICATE THE COMPLETION CODE    C
!                     IFLAG=0  INDICATES NORMAL COMPLETION             C
!                     IFLAG=90 INDICATES EOF WAS ENCOUNTERED           C
!                     IFLAG=91 INDICATES AN INVALID VALUE FOR IFLAG    C
!                     IFLAG=92 INDICATES THE SUBROUTINE WAS DISABLED   C
!                     IFLAG=93 INDICATES AN UNRECOVERABLE PARITY ERROR C
!                              OCCURRED.                               C
!                     IFLAG=94 INDICATES A RECOVERABLE PARITY ERROR    C
!                              OCCURRED.                               C
!                                                                      C
!        WRITTEN BY GEORGE LEHMANN, JR.                                C
!                   OCTOBER,1976                                       C
!                                                                      C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      DIMENSION IRATE(*),SCALE(*),BIAS(*),ARRAY(*),INARRAY(*)
      COMMON /TPCORR/ RHOTDC
      LOGICAL RHOTDC
      DATA RHOTDC /.TRUE./
      INTEGER ENABLE, status
      DATA ENABLE /1/
      LOGREC=NREC+1
      ENABLE=0
      ITEMS=NPERCYC*NREC
      RETURN
!
!        BEGIN NORMAL READ SEQUENCE
!
      ENTRY RDREC(INUNIT,IRATE,SCALE,BIAS,NPARMS,NPERCYC,NREC,ARRAY,    &
     &            INARRAY,IFLAG)
!
      NPARITY=0
      IF (IFLAG.GT.NREC) GO TO 970
      IF (IFLAG.LT.0) GO TO 960
      IF (ENABLE.NE.0) GO TO 980

      IF (LOGREC.LE.NREC.AND.IFLAG.EQ.0.OR.IFLAG.GT.LOGREC) GO TO 100
!
!        READ IN NEW PHYSICAL RECORD
!
    5 CONTINUE
      READ( UNIT=INUNIT, FMT='(A)', IOSTAT= status, SIZE=NLENG, ADVANCE='NO') INARRAY(1001:1500)
   10 CONTINUE
      IF (status) 20,950,990
   20 LOGREC=1
      IF (IFLAG.NE.0) LOGREC=IFLAG
      CALL GBYTES (INARRAY(1001),INARRAY(1),0,20,0,ITEMS)
  100 ISTART=(LOGREC-1)*NPERCYC
      ISET=1
      DO 200 J=1,NPARMS
      J2=IRATE(J)
      DO 200 K=1,J2
      ARRAY(ISET)=FLOAT(INARRAY(ISTART+ISET))/SCALE(J)-BIAS(J)
  200 ISET=ISET+1
      IF (RHOTDC) ARRAY(7)=ARRAY(7)*(ARRAY(6)+273.16)/(ARRAY(4)+273.16)
      LOGREC=LOGREC+1
      IFLAG=0
      IF (NPARITY.NE.0) IFLAG=94
      RETURN
!
!        ERROR MESSAGES
!
  960 IFLAG=91
      WRITE (6,961)
      RETURN
!
  950 IFLAG=90
      ENABLE=1
      RETURN
!
  970 IFLAG=91
      WRITE (6,971)
      RETURN
!
  980 IFLAG=92
      WRITE (6,981)
      RETURN
!
  990 WRITE (6,991) INUNIT
      IF (NPARITY.NE.0) GO TO 992
      BACKSPACE INUNIT
      NPARITY=1
      GO TO 5
  992 IFLAG=93
      RETURN
!
  961 FORMAT ("-  >>>ERROR<<<  VALUE OF IFLAG IS NEGATIVE")
  971 FORMAT ("-  >>>ERROR<<<  LOGICAL RECORD NUMBER REQUESTED EXCEEDS A&
     & VAILABLE NUMBER OF RECORDS")
  981 FORMAT ("-  >>>ERROR<<<  DATA RECORD REQUESTED BEFORE HEADER RECOR&
     & HAS BEEN READ")
  991 FORMAT ("-  >>>ERROR<<<  PARITY ERROR ENCOUNTERED DURING READ OPER&
     & ATION ON UNIT ",I2)
      END
