C
C*      Copyright University Corporation for Atmospheric Research, 1995   *
C     MODS: ("C.." indicates new source code)
C     - June 19,1994  D. Hansen
C     - File: g2inp.com
C             Added "KEYSCL" = 1 (no scaling), 2 (scaling)
C..           &                KEY(NUMVARS), KEYSCL(NUMVARS),
C     - File: G2prmpt.f
C             Changed imput line to include "KEYSCL"
C..            READ(DEBUF(INDXD),810) KEY(NTERM),KEYSCL(NTERM),
C..            $             TERM(NTERM),FACTR(NTERM) 
C..            810          FORMAT(11X,i1,2X,I1,1X,F15.8,1X,F14.8,34X)
C     - File: G2rdata.f
C             Changed UNPCK() to note whether "scaling" is used
C..C..      2=scaling, 1=no scaling
c.... -> For PC Linux version, must swap bytes changing order of
c..      each 4 bytes from order 1234 to 4321.  Routine tests byte
c..      order and does this swap if needed.
C..         IF (KEYSCL(I) .EQ. 2) THEN
C..            DATARY(IC)=(IDAT(IC)/FACTR(I))-TERM(I)
C..         ELSE
C..            DATARY(IC)=IDAT(IC)
C..         ENDIF
C
      SUBROUTINE G2RDATA(ibatch)
      INCLUDE 'file.com'
      INCLUDE 'g2inp.com' 
      INCLUDE 'tmout.com'
      INCLUDE 'slus.prm'
      LOGICAL PARITY,FIRST,GAP
      DIMENSION DATARY(20000) 
      INTEGER*4 IDUMM 
      data tscnds/-1./
      save TIMIN
C 
C-----> Initialize some variables 
C 
      NPAR=0
      NVOLT=0 
      NWORDS=NVAR+4 
      GAP = .FALSE.
C 
C-----> Find the index of PTIME in the data file.  If it's not there, use
C-----> TPTIME instead.  Also, in any case, store the index of TPTIME to
C-----> allow for checking of the GAPFILL value.
c-----> Also, save index of FSSP range (which must be changed because
c-----> GENPRO outputs 1--4 but DAP assumes 0--3)
C 
      INDTIM=0
      itptim=0
      itime = 0
      IFSPRG = 0
      JHR=0
      JMIN=0
      JSEC=0
      DO 2 I=1,NVARS
         IF (VNAME(I)(1:6).EQ.'PTIME ') INDTIM=I 
         IF (VNAME(I)(1:6).EQ.'TPTIME') ITPTIM = I
c....... this is for N2UW:
         if( (vname(i)(1:6).eq.'time  ') .or. 
     $      (vname(i)(1:6).eq.'TIME  ')) itime=I
         if(vname(i)(1:6).eq.'HR    ') JHR=i
         if(vname(i)(1:6).eq.'MIN   ') JMIN=i
         if(vname(i)(1:6).eq.'SEC   ') JSEC=i
         IF (VNAME(I)(1:6).EQ.'FRANGE') IFSPRG = I
    2 CONTINUE
      IF (INDTIM.EQ.0) INDTIM=ITPTIM 
c....... use HR, MIN, SEC if present:
      if(INDTIM.le.0 .and. jhr.ne.0 .and. jmin.ne.0 .and. jsec.ne.0)
     $   INDTIM=-1
      if (INDTIM.eq.0 .and. itime.ne.0) INDTIM=-2
c
7     continue

      write(6,'(" indtim,itptim,itime,ihr,imin,isec=",6i9)') INDTIM,
     $      ITPTIM,itime,JHR,JMIN,JSEC
C 
C-----> Create the data file
C 
      CALL FILE(7,IDUMM,IERR,1) 
C 
C-----> Loop through all the time segments
C 
c....... see how many seconds of data are expected
      BEGSEC=ITMOUT(1,1)*3600.+ITMOUT(2,1)*60.+ITMOUT(3,1)
      ENDSEC=ITMOUT(4,1)*3600.+ITMOUT(5,1)*60.+ITMOUT(6,1)
      TOTSEC = ENDSEC-BEGSEC
      ifrsc = 0
      if(TOTSEC.lt.0.) TOTSEC = TOTSEC + 86400.
c....... if this is more than available data, reset to available:
      BEGSEC=ITMSEG(1,1)*3600.+ITMSEG(2,1)*60.+ITMSEG(3,1)
      ENDSEC=ITMSEG(4,NTMSEG)*3600.+ITMSEG(5,NTMSEG)*60.
     $   +ITMSEG(6,NTMSEG)
      testt = ENDSEC-BEGSEC
      if(testt.lt.0.) testt=testt+86400.
      if(TOTSEC.gt.testt) TOTSEC = testt
      if(TOTSEC.gt.36000.) TOTSEC = 8.*3600.
      secpr=0.
      NTMSEG=0
      DO 100 I=1,NTMOUT 
      BEGSEC=ITMOUT(1,I)*3600.+ITMOUT(2,I)*60.+ITMOUT(3,I)
      ENDSEC=ITMOUT(4,I)*3600.+ITMOUT(5,I)*60.+ITMOUT(6,I)
      FIRST=.TRUE.
      WRITE(LUTO,88) ITMOUT(1,I),ITMOUT(2,I),ITMOUT(3,I) 
   88 FORMAT(' Searching for ',I2,':',I2,':',I2,' ...')
C...... Preceding line is non-standard, required for PC-linux version
      IF (FIRST.AND.(I.NE.1)) GOTO 15 
   10 CALL GENDAT(MTLU,0,DATARY,ISTAT)
c      if(indtim.gt.0) then
c         WRITE(6,900) INDTIM,DATARY(IPOS(INDTIM)),BEGSEC,ENDSEC
c      elseif(indtim.eq.-2) then
c         WRITE(6,900) ITIME,DATARY(IPOS(ITIME)),BEGSEC,ENDSEC
c      endif
 900  FORMAT('INDTIM,DATARY(IPOS(INDTIM)),BEGSEC,ENDSEC=',I9,3F15.5)
c....... correct from GENPRO to DAP convention for FSSP range
      if(ifsprg.gt.0) datary(ipos(ifsprg)) = datary(ipos(ifsprg))-1.
      if(indtim.gt.0) then
         time=datary(ipos(indtim))
      elseif(indtim.eq.-1) then
         time=datary(IPOS(JHR))*3600.+datary(IPOS(JMIN))*60.
     $          +datary(IPOS(JSEC))
      elseif(indtim.eq.-2) then
	 ktime = datary(ipos(itime))
c 	 write(6,'(" ktime=",i9)') ktime
	 time = 3600*(ktime/10000) + 60*(mod(ktime,10000)/100)
     $      + mod(ktime, 100)
      else
         if(tscnds.le.0.) tscnds=strttm-1.
         tscnds=tscnds+1.
         time=tscnds
      endif
c     write(6,'(" time=",f10.0)') time
   15 IF (BRFLG) THEN
         ITMSEG(4,NTMSEG)=TIMIN/3600. 
         ITMSEG(5,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.)/60.
         ITMSEG(6,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.-
     &                    ITMSEG(5,NTMSEG)*60.) 
         GOTO 1000
         END IF 
C 
C-----> Check the status of the tape read 
C 
      PARITY=.FALSE.
      IF (ISTAT.GT.0) THEN
C 
C-----> READ error
C 
         PARITY=.TRUE.
       ELSE IF (ISTAT.EQ.0) THEN
C 
C-----> EOF sensed
C 
         WRITE(LUTO,20) 
  20     FORMAT(78(' '),/' EOF sensed on data file.')
C 
C-----> Set up the ending time for this segment 
C 
c        write(6,'(" ntmseg=",i5,", TIMIN=",f9.0)') ntmseg,TIMIN
         ITMSEG(4,NTMSEG)=TIMIN/3600. 
         ITMSEG(5,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.)/60.
         ITMSEG(6,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.-
     &                    ITMSEG(5,NTMSEG)*60.) 
   22       continue
      goto 1000
c           WRITE(LUTO,25) 
c  25       FORMAT(' End-of-file sensed on input. ',//
c    &      ' Do you wish to:',/ 
c    &      ' 1-Quit processing',/ 
c    &      ' 2-Continue processing a different tape on this drive ',/ 
c    &      ' 3-Continue processing a different file or a different',/
c    &      '   tape on a different drive',/ 
c    &      ' 4-Continue processing next file on current tape',//
c    &      ' Enter selection number: ',$)
c           READ(LUTI,*,ERR=22) NUMSEL
c           IF (NUMSEL.LT.1.OR.NUMSEL.GT.4) GOTO 22 
c           IF (NUMSEL.EQ.1) GOTO 1000
c           IF (NUMSEL.EQ.3) THEN 
c              OMTLU=MTLU
c 26           WRITE(LUTO,27) 
c 27  FORMAT(' Enter device name (mt[0,1], ex[0,1], or "fff" ',
c    &   'for file input): ',$)
c              READ(LUTI,'(a3)',ERR=26) DEVNAME
c     IF (DEVNAME.NE.'ex0'.AND.DEVNAME.NE.'ex1'.AND.DEVNAME.NE.'mt0'
c    &    .AND.DEVNAME.NE.'mt1' .AND.DEVNAME.NE.'fff') GOTO 26
c              IF(DEVNAME.EQ.'fff') then
c                 WRITE(LUTO,460)
c 460             FORMAT(' Enter filename:',$)
c                 READ(LUTI,465) FLNAME
c 465             FORMAT(A)
c                 ENDIF
c              END IF 
c     IF (NUMSEL.NE.4) THEN
c        MTLU=8
c        CALL CCLOSE(IFD,ISTAT,IERR)
c        IF (DEVNAME.EQ.'mt0') THEN
c           CALL COPEN(MTLU,IFD,HMT0SFN,1,IERR,0)
c         ELSE IF (DEVNAME.EQ.'mt1') THEN
c           CALL COPEN(MTLU,IFD,HMT1SFN,1,IERR,0)
c         ELSE IF (DEVNAME.EQ.'ex0') THEN
c           CALL COPEN(MTLU,IFD,EX0SFN,1,IERR,0)
c         ELSE IF (DEVNAME.EQ.'ex1') THEN
c           CALL COPEN(MTLU,IFD,EX1SFN,1,IERR,0)
c         ELSE
c           CALL COPEN(MTLU,IFD,FLNAME,1,IERR,0)
c        END IF

c     ENDIF
c
C-----> Skip over the header file on the tape 
C 
c     if(DEVNAME.NE.'fff') THEN
c           CALL FSFTAP(IFD,ISTAT,IERR)
c     else
c           CALL GENHDR(MTLU,LUOUT)
c     endif
c  29       NLOGRC=IDATLG 
c           ISTAT=-1
c           CALL GENDAT(MTLU,0,DATARY,ISTAT)
c           TIME=DATARY(IPOS(INDTIM)) 
c           if(indtim.gt.0) then
c              TIME=DATARY(IPOS(INDTIM)) 
c           elseif(indtim.lt.0) then
c              time=datary(IPOS(JHR))*3600.+datary(IPOS(JMIN))*60.
c    $             +datary(IPOS(JSEC))
c           else
c              time=tscnds
c           endif
c 
C-----> Check if a new time segment needs to be set up
C 
c           IF (TIME.NE.(TIMIN+1.).AND.TIME.GE.BEGSEC 
c    &          .AND.TIME.LE.ENDSEC) THEN 
c              NTMSEG=NTMSEG+1
c              ITMSEG(1,NTMSEG)=TIME/3600.
c              ITMSEG(2,NTMSEG)=(TIME-ITMSEG(1,NTMSEG)*3600.)/60. 
c              ITMSEG(3,NTMSEG)=(TIME-ITMSEG(1,NTMSEG)*3600.- 
c    &                           ITMSEG(2,NTMSEG)*60.)
c              END IF 
         ENDIF
c 
C-----> Check the time from the tape versus the time requested
C 
c     IF (DATARY(IPOS(INDTIM)).LT.BEGSEC) GOTO 10 
      IF (time.LT.BEGSEC) GOTO 10 
C 
C-----> Check the time for a GAPFILL value
C 
      IF (time.eq.99999. .and. TIMIN.ne.99998.) THEN
         GAP=.TRUE. 
         GOTO 10
         END IF 
      IF (time.GT.ENDSEC) GOTO 90 
C 
C-----> Set the time segment start time if this is the FIRST pass 
C 
      IF (FIRST) THEN 
         NTMSEG=NTMSEG+1
         ITMSEG(1,NTMSEG)=time/3600.
         ITMSEG(2,NTMSEG)=(time-ITMSEG(1,NTMSEG)*3600.) 
     &                    /60.
         ITMSEG(3,NTMSEG)=(time-ITMSEG(1,NTMSEG)*3600.- 
     &                    ITMSEG(2,NTMSEG)*60.) 
         FIRST=.FALSE.
c        WRITE(LUTO,30) (ITMOUT(J,I),J=1,6) 
  30     FORMAT(' Processing time segment ',I2,':',I2,':',I2,'-',
     $      I2,':',I2,':',I2) 
         END IF 
C 
C-----> If there was a gap in the data, create a new time segment 
C 
         IF (GAP) THEN
            ITMSEG(4,NTMSEG)=IHR
            ITMSEG(5,NTMSEG)=IMIN 
            ITMSEG(6,NTMSEG)=ISEC 
            NTMSEG=NTMSEG+1 
            ITMSEG(1,NTMSEG)=time/3600. 
            ITMSEG(2,NTMSEG)=(time-ITMSEG(1,NTMSEG) 
     &                       *3600.)/60.
            ITMSEG(3,NTMSEG)=(time-ITMSEG(1,NTMSEG) 
     &                       *3600.-ITMSEG(2,NTMSEG)*60.) 
            GAP=.FALSE. 
            END IF
C 
C-----> Set the time for this record
C 
      IHR=time/3600.
      IMIN=(time-IHR*3600.)/60. 
      ISEC=(time-IHR*3600.-IMIN*60.)
      if(time.le.0.) goto 10
      TIMIN=time
c     write(6,'(" time, seconds, indtim=",3i2,f10.1,i8)')
c    $   IHR,IMIN,ISEC, TIMIN, indtim 
c        write(6,'(" time,indtim=",f9.1,i7)') TIMIN,indtim
c        write(6,'(" time,indtim=",i9,i7)') TIMIN,indtim
c     WRITE(LUTO,40) IHR,IMIN,ISEC
c  40 FORMAT(' 'I2':'I2':'I2,8(''),$)
      if(imin.ne.iminold) WRITE(LUTO,40) IHR,IMIN
   40 FORMAT(' ',I2,':',I2,6(''),$)
      iminold=imin
      if(ifrsc.eq.0) ifrsc = time
      ifract = (time-ifrsc)*100./totsec
      if(ifract.ne.ifrold) then
         ifrold = ifract
c        write(0,'(i3)') ifract
	 if(ibatch.eq.0) call setgg(ifract)
      endif
C 
C-----> Assign the values and figure indexes into the tape array
C-----> so that all data appear at the desired rate 
C 
      DO 60 IMSEC2=0,999,IDELTT 
      DO 50 I2=1,NVAR 
      J=IND(I2) 
C     K=IMSEC*FLOAT(IRATE(J))/1000.
      K=IMSEC2*FLOAT(IRATE(J))/1000.
      VALUES(I2)=DATARY(IPOS(J)+K) 
   50 CONTINUE
      NMRECS=NMRECS+1 
C 
C-----> Set IMSEC to -1 if there was a read error for this record 
C 
      IF (PARITY) THEN
         IMSEC=-1 
         NPAR=NPAR+1
       ELSE 
         IMSEC=IMSEC2 
         ENDIF
C 
C-----> Create a DAP data record
C 
      CALL FILE(4,NMRECS,IERR,1)
   60 CONTINUE
      GOTO 10 
C 
C-----> Set up the ending time for this segment 
C 
   90 ITMSEG(4,NTMSEG)=TIMIN/3600.
      ITMSEG(5,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.)/60. 
      ITMSEG(6,NTMSEG)=(TIMIN-ITMSEG(4,NTMSEG)*3600.- 
     &                 ITMSEG(5,NTMSEG)*60.)
  100 CONTINUE
C 
C-----> Write out the DAP header file 
C 
1000  CALL CCLOSE(IFD,ISTAT,IERR)
      CALL FILE(3,1,IERR,1) 
C 
C-----> Write out processing summary
C 
      WRITE(LUTO,1010) NMDATA(2:6),NTMSEG
 1010 FORMAT(78(' ')//,1X,28('*'),' PROCESSING SUMMARY ',28('*'),//
     & ' Data file: ', A,//,1X,I2,' time segments were processed:')
      DO 1030 I=1,NTMSEG
      WRITE(LUTO,1020) I,(ITMSEG(J,I),J=1,6)
 1020 FORMAT(1X,I2,3X,I2,':',I2,':',I2,'-',I2,':',I2,':',I2)
 1030 CONTINUE
      call summary(nmdata(2:6), ntmseg, itmseg(1,1), itmseg(4,NTMSEG))
      WRITE(LUTO,1040) NPAR 
 1040 FORMAT(/,1X,I2,' parity errors occurred while processing',//,
     &       1X,78('*')) 
      ifract = 100
c        write(0,'(i3)') ifract
c        write(0,'(i3)') ifract
c        write(0,'(i3)') ifract
c        write(0,'(i3)') ifract
c 	 if(ibatch.eq.0) call setgg(ifract)
      RETURN 
      END 
C 
C 
C 
      SUBROUTINE UNPCK(DATARY,IDAT) 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Subroutine UNPCK converts the 32 bit integer data stored in IDAT C
C to a floating point real number to be placed in DATARY using the     C
C GENPRO II method of scaling.                                         C
C..  Also, for PC-Linux version, swaps order of bytes for binary case  C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'g2inp.com'
      INCLUDE 'file.com' 
      INTEGER*4 IDAT(1) 
      DIMENSION DATARY(1) 
      integer byteswap
      IC=0
      call testswap(byteswap)
      DO 10 I=1,NVARS 
      DO 10 J=1,IRATE(I)
      IC=IC+1 
c....... swap bytes if needed
      if(byteswap.eq.1) call swapf(IDAT(IC))
C...... Preceding line is non-standard, required for PC-linux version
c
c..     floating point
      if(KEY(I).eq.0) then
         call move(IDAT(IC), DATARY(IC), 1)
      else
C..      2=scaling, 1=no scaling
         IF (KEYSCL(I) .EQ. 2) THEN
            DATARY(IC)=(IDAT(IC)/FACTR(I))-TERM(I)
         ELSE
            DATARY(IC)=IDAT(IC)
         ENDIF
      endif
c      if(I.lt.8) then
c         WRITE(6,5) IC,I,IDAT(IC),FACTR(I),TERM(I),DATARY(IC)
c5    FORMAT("IC,I,IDAT(IC),FACTR(I),TERM(I),DATARY(IC)=",
c    &       3I9,3F15.5)
c      endif
   10 CONTINUE
      RETURN
      END 
C 
C 
C 
      SUBROUTINE GENDAT(LUIN,LUOUT,DATARY,ISTAT)
  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Subroutine GENDAT will read a logical data record.  It first     C
C checks to see if it is necessary to read a physical record.  It then C
C converts each integer data values to a real data value through       C
C subroutine UNPCK.                                                    C
C                                                                      C
C PARAMETERS USED BY GENDAT:                                           C
C   LUIN      -The variable used to store the logical unit number of   C
C              the input device from which the data are read.          C
C   LUOUT     -The logical unit number of the device to which all      C
C              error messages are to be written.                       C
C   DATARY()  -The array where the data are to be stored.              C
C   ISTAT     -A status return variable.                               C
C                ISTAT=-1  successful read                             C
C                ISTAT=0   eof sensed                                  C
C                ISTAT=1   unrecoverable parity error occured          C
C                                                                      C
C VARIABLES IN GENDAT:                                                 C
C   NLOGRC    -A variable used to keep track of the current logical    C
C              record being processed.  It is used to determine        C
C              whether or not a new physical record needs to be read.  C
C   ILEN      -A variables used to specify the number of characters to C
C              read from the data file.                                C
C   INDX      -A pointer used to position a logical record within a    C
C              physical record.                                        C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'g2inp.com' 
C 
C-----> SET PARAMETER IWDSZ TO THE NUMBER OF BITS AN INTEGER VARIABLE USES
C-----> (USUALLY THIS IS THE MACHINE WORD SIZE IN BITS) 
C 
      PARAMETER (IWDSZ=32)
      PARAMETER (ISIZ=131072/IWDSZ) 
      INTEGER*4 INDAT(ISIZ)
      REAL*4 DATARY(5) 
      SAVE INDAT
C 
C-----> Determine whether or not it is necessary to read the next 
C-----> physical data record
C 
      IF (NLOGRC.GE.IDATLG) THEN
         NLOGRC=0 
C 
C-----> ILEN should contain the number of characters to be read prior to
C-----> the BUFIN call
C 
         ILEN=IDATSIZ/8
c         write(6,'(" ready for bufin call, ilen=",i7)') ilen
         CALL BUFIN(LUIN,INDAT,ILEN,ISTAT)
c         WRITE(6,900) INDAT(1),INDAT(2),INDAT(3),ISTAT,ILEN
c 900    FORMAT("INDAT(1-3)= ",3I9, ", ISTAT,ILEN=",2i9) 
         IF (ISTAT) 50,100,20 
       ELSE 
         GOTO 50
         END IF 
C 
C-----> PARITY ERROR
C 
   20 WRITE(LUOUT,30) 
   30 FORMAT(/,' >>> UNRECOVERABLE PARITY ERROR ON DATA FILE',/)
C 
C-----> GOOD READ 
C 
   50 NLOGRC=NLOGRC+1 
      INDX=(ILGBIT/IWDSZ)*(NLOGRC-1)+1
c     WRITE(6,909) INDX,ILGBIT,IWDSZ,NLOGRC,INDAT(INDX) 
c 909 FORMAT("INDX,ILGBIT,IWDSZ,NLOGRC,INDAT(INDX)=",5I9) 
c     write(6,'(1x,10z9)') (indat(i),i=1,10)
      CALL UNPCK(DATARY,INDAT(INDX))
c     WRITE(6,910) DATARY(1),DATARY(2),DATARY(3),DATARY(4),DATARY(5)
c 910 FORMAT('DATARY(1-5)=',5F15.5) 
      RETURN
C 
C-----> EOF SENSED
C 
  100 WRITE(LUOUT,110)
  110 FORMAT(/,' >>> EOF SENSED ON DATA FILE',/)
      RETURN
      END 
