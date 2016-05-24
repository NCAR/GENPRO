      SUBROUTINE G2PRMPT(istart, iend, fname, gname,
     $   user, dnotes, itms, irpl, igseg, ibatch, igo)
C*      Copyright University Corporation for Atmospheric Research, 1995   *
      INCLUDE 'file.com'
      INCLUDE 'g2inp.com' 
      INCLUDE 'slus.prm'
      INCLUDE 'tmout.com'
      CHARACTER*5 FNAME 
      character*120 gname
      character*16 user
      character*80 dnotes
      character*80 hfile,dfile
      integer HFL,HDL
c     CHARACTER*1 ANSWR 
      irepl = mod(irpl,100)
      ifmtd = irpl / 100
      n=50
334   if(user(n:n).ne." ") goto 333
      n = n - 1
      if(n.gt.1) goto 334
333   continue
c....... flag to allow overwriting of DAP file (0 => NO)
      nmuser(1) = user
      notes = dnotes
      flname = gname
      NTMOUT = 1
      ITMOUT(1,1) = istart/10000
      ITMOUT(2,1) = mod(istart,10000)/100
      itmout(3,1) = mod(istart,100)
      ITMOUT(4,1) = iend/10000
      ITMOUT(5,1) = mod(iend,10000)/100
      itmout(6,1) = mod(iend,100)
C 
C-----> Get DAP data file name
C 
c     WRITE(LUTO,50)
   50 FORMAT(' Enter five character file identifier: ',$) 
c     READ(LUTI,100) FNAME
  100 FORMAT(A) 
c     CALL CAPTL(FNAME,5)
      NMHEAD='H'//FNAME 
      NMDATA='D'//FNAME 
      NTRY = 0
  110 CALL FILE(6,1,IERR,LUBIT)
C 
C-----> Check to see if the file already exists 
C 
      IF (IERR.EQ.117 .or. IERR.EQ.1017) THEN
	 NTRY = NTRY + 1
	 if(NTRY.gt.2) then
		write(LUTO,'(" Unable to create DAP file; aborting")')
		call exit(1)
	 endif
c........... newfile returns 0 to abort, <0 for overwriting, >0 for new name
	 if(irepl.lt.10) then
             if(ibatch.eq.0) then
		i =  newfile(FNAME)
		if(i.eq.0) then
c		   write(LUTO,'(" aborting")')
		   igo = -1
		   return
		else if(i.gt.0) then
	            NMHEAD='H'//FNAME 
                    NMDATA='D'//FNAME 
		   NTRY = 0
		   goto 110
		endif
             else
		write(0, '(" specified output file ",
     $               a6, " exists")') FNAME
                call cname (hfile, "XANDATA", NMHEAD,HFL)
		write(0, '("(full path to header:"$)')
		write(0, '(1x,(a),")")') hfile
		write(0, '(" delete or try another name")')
		write(0, '(" program is aborting...")')
		call exit(1)
   	     endif
	 endif
	 write(LUTO,'(1x,a," is being overwritten")') FNAME 
         NMHEAD='H'//FNAME 
         NMDATA='D'//FNAME 
C     
C----->Purge the old files
C     
         call cname (hfile, "XANDATA", NMHEAD,HFL)
         call cname (dfile, "XANDATA", NMDATA,HDL)
         OPEN(13,FILE=hfile(1:HFL)) 
         CLOSE(13,STATUS='DELETE') 
         OPEN(13,FILE=dfile(1:HDL)) 
         CLOSE(13,STATUS='DELETE') 
C     
C----->Prepare to create the new file
C     
         GOTO 110 
         END IF 
      if(FLNAME(1:2).eq."ex" .or. FLNAME(1:2).eq."mt") then
         DEVNAME = FLNAME(1:3)
      else
         DEVNAME = "fff"
      endif
c     IF (DEVNAME.NE.'ex0'.AND.DEVNAME.NE.'ex1'.AND.DEVNAME.NE.'mt0'
c    &    .AND.DEVNAME.NE.'mt1' .AND. DEVNAME.NE.'fff') THEN
  400 FORMAT(I2)
  450 FORMAT(' Illegal device name, ',
     &   'use "ex0", "ex1", "mt0", "mt1", or "fff"')
c        WRITE(LUTO,450)
c        GOTO 325 
c        ENDIF
C
C-----> Open the tape drive for reading
C
      MTLU=28
      IF (DEVNAME.EQ.'mt0') THEN
            CALL COPEN(MTLU,IFD,HMT0SFN,1,IERR,0)
       ELSE IF (DEVNAME.EQ.'mt1') THEN
            CALL COPEN(MTLU,IFD,MT0SFN,1,IERR,0)
       ELSE IF (DEVNAME.EQ.'ex0') THEN
            CALL COPEN(MTLU,IFD,EX0SFN,1,IERR,0)
       ELSE IF (DEVNAME.EQ.'ex1') THEN
            CALL COPEN(MTLU,IFD,EX1SFN,1,IERR,0)
       ELSE
         CALL COPEN(MTLU,IFD,FLNAME,1,IERR,0)
      END IF
      IF (IERR.NE.0) then
	WRITE(LUTO,470) IERR
	write(LUTO,'(" aborting")')
	call exit(1)
      endif
c     WRITE(LUTO,470) IERR
  470 FORMAT(' Error number ',I3,' when opening input device.')
C 
C-----> Get the number of files to skip 
C 
      NSKP=0
  570    FORMAT('1')
      if(mod(irepl,10).ne.0) then
          LUOUT=39
      else
          LUOUT=LUBIT
      endif
      WRITE(LUTO,580) 
  580 FORMAT(/' Reading Genpro II header ...') 
      CALL GENHDR_LOCAL(MTLU,LUOUT) 
c....... skip files here:
      if(igseg.gt.1) then
          do 582 mm=2,igseg
c      write(6,'(" bsec, esec, ilgbit=",2f10.1, i6)')
c    $		bsec,esec,ilgbiT
c............... this is special, for Tammy Weckwerth, needed to
c..              read past bad file in N2UW data:
c	      if(mm.eq.14) esec = esec - 24
c..... this is for /VAUGHAN/CAPE91/N2UW/S00450B:
c	      if(mm.eq.6) esec = esec - 12
              nbitskip = (ESEC - BSEC) * (ILGBIT/8)
              call bnseek(ifd, nbitskip, 0)
              rewind LUOUT
              CALL GENHDR_LOCAL(MTLU,LUOUT) 
 582       continue
      endif
c     WRITE(LUOUT,570)
C-----> Initialize number of variables, record length in words, and sample
C-----> rate
C 
      NVAR=NVARS
      IDELTT=itms 
      write(6,'(" ms between records=",i5)') itms
C 
C-----> Set the variable names and units for the DAP file to be created 
C 
      DO 700 I=1,NVAR 
      IND(I)=I
c....... check for name translation
      call newname(VNAME(i))
      WRITE(NAMES(I),600) VNAME(I)(1:8)
  600 FORMAT(A) 
      WRITE(UNITS(I),650) UNITSG(I)(1:4)
  650 FORMAT(A) 
  700 CONTINUE
c     WRITE(LUTO,750) 
c 750 FORMAT(' The default set up is all variables at one sample ' 
c    &       'per second for the',/,' entire tape.  Do you wish to ' 
c    &       'change this default? (Y/N, [r]=N) ',$) 
c     READ(LUTI,800) ANSWR
  800 FORMAT(A) 
c     IF (ANSWR.EQ.'Y'.OR.ANSWR.EQ.'y') THEN
c        MDFY=.TRUE.
c      ELSE 
         MDFY=.FALSE. 
c        ENDIF
      igo = 1
      RETURN
      END 
C 
C 
C 
c....... this version was modified by Ron Ruth to handle Genpro-I also...
c..        and by WAC for 2UW-format headers
      SUBROUTINE GENHDR_LOCAL(LUIN,LUOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Subroutine GENHDR will read a GENPRO II header, write it out (to C
C logical unit number LUOUT), store the values necessary to read the   C
C data file, and write out a table of the variables, their indices     C
C within the data array (when created) and their rates.                C
C                                                                      C
C PARAMETERS USED BY GENHDR:                                           C
C   LUIN      -The logical unit number of the input device from which  C
C              the data are read.                                      C
C   LUOUT     -The logical unit number of the device to which the      C
C              header should be written.                               C
C   IWDSZ     -A parameter indicating the number of bits an INTEGER    C
C              variable occupies.  NOTE: BE SURE TO SET THIS PARAMETER C
C              FOR YOUR MACHINE!!                                      C
C   ISIZ      -A parameter specifying the length of the input buffer   C
C              array.                                                  C
C   NCHR      -A parameter specifying the number of characters the     C
C              given machine may store in one INTEGER variable.        C
C                                                                      C
C VARIABLES IN GENHDR:                                                 C
C   ILGBIT    -The number of bits per logical data record.             C
C   IDATLG    -The number of logical data records per physical data    C
C              record.                                                 C
C   IRATE()   -This array stores the sample rate of each respective    C
C              variable.                                               C
C   IBITS     -The number of bits in a sample.                         C
C   IPOS()    -This array stores the index position of each respective C
C              variable in a data record array.                        C
C   KEY()     - == 0 for IEEE flt pt format, 1 for integer.
C   TERM()    -This array stores the TERM value for each respective    C
C              variable.                                               C
C   FACTR()   -This array stores the FACTOR value for each respective  C
C              variable.                                               C
C   NVAR      -The number of variables sampled for this data file.     C
C   NLOGRC    -A counter used to determine the current logical data    C
C              record being processed.                                 C
C   IBUF()    -The integer array used to buffer in a header record, it C
C              is equivalenced to character arrays so that string      C
C              processing is facilitated.                              C
C   INBUF()   -The array used to check for an eight character          C
C              identifier at the beginning of a logical header record. C
C   VNAME()   -The array to store the respective variable names.       C
C   DATSIZ    -Stores the eight character identifier ' DATSIZ '.       C
C   LOGBIT    -Stores the eight character identifier ' LOGBIT '.       C
C   DATLOG    -Stores the eight character identifier ' DATLOG '.       C
C   ORDVAR    -Stores the eight character identifier ' ORDVAR '.       C
C   LETVAR    -Stores the eight character identifier ' LETVAR '.       C
C   PROJECT   -Stores the eight character identifier '/PROJECT'.       C
C   PRDATE    -Stores the eight character identifier '/PRDATE '.       C
C   BEGSNP    -Stores the eight character identifier '/BEGSNP '.       C
C   ENDSNP    -Stores the eight character identifier '/ENDSNP '.       C
C   HDRLOG    -Stores the eight character identifier '/HDRLOG '.       C
C   ENDHD     -Stores the eight character identifier ' ENDHD  '.       C
C   ODATSIZ   -Stores the eight character identifier 'DATSIZ  '.       C
C   OLOGBIT   -Stores the eight character identifier 'LOGBIT  '.       C
C   ODATLOG   -Stores the eight character identifier 'DATLOG  '.       C
C   OORDVAR   -Stores the eight character identifier 'ORDVAR  '.       C
C   OLETVAR   -Stores the eight character identifier 'LETVAR  '.       C
C   OPROJECT  -Stores the eight character identifier 'PROJECT '.       C
C   OPRDATE   -Stores the eight character identifier 'PRDATE  '.       C
C   OBEGSNP   -Stores the eight character identifier 'BEGSNP  '.       C
C   OENDSNP   -Stores the eight character identifier 'ENDSNP  '.       C
C   OHDRLOG   -Stores the eight character identifier 'HDRLOG  '.       C
C   DEBUF()   -The array used to decode numerical values from a        C
C              logical header record.                                  C
C   ILEN      -A parameter used by BUFIN to specify the number of      C
C              characters to be read.  On return from BUFIN, the       C
C              variable stores the actual number of characters read.   C
C   NORDV     -A variables used to deetermine the number of times the  C
C              ' ORDVAR ' identifier has been encountered.             C
C   NRATE     -A variable used as an index into IRATE storing the      C
C              number of IRATE values read.                            C
C   NTERM     -A variable used as an index into TERM and FACTR storing C
C              the number of TERM and FACTR values read.               C
C   NREC      -A counter used to keep track of the number of physical  C
C              header records read.                                    C
C   INDX      -The current index into the INBUF array.                 C
C   INDXD     -The current index into the DEBUF array.                 C
C                                                                      C
C      This routine was written by Mike Daniels, September 1984.       C
C Questions and/or comments may be given to me by calling              C
C (303) 497-1037.                                                      C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C-----> COMMON BLOCK FOR DAP FILES
C
      INCLUDE 'file.com'
C
      INCLUDE 'g2inp.com'
      INCLUDE 'slus.prm'
C
C-----> IMPORTANT! : Set the value of parameter IWDSZ to the number of
C-----> bits an INTEGER variable uses (usually this is the machine
C-----> word size in bits)
C
      PARAMETER (IWDSZ=32)
      PARAMETER (ISIZ=6400/IWDSZ)
      PARAMETER (NCHR=IWDSZ/8)
      DIMENSION IBUF(ISIZ)
      INTEGER*4 IFBIT 
      INTEGER DATCON, ITEMP
      INTEGER         NTITLE
      CHARACTER*3 MONTH 
      CHARACTER*5 DEFMT 
      CHARACTER*2 hlog
      CHARACTER*8 INBUF(100),LOGBIT,DATLOG,ORDVAR,LETVAR,
     +            PROJECT,PRDATE,DATSIZ,ENDHD,PRTIME,BEGSNP,ENDSNP,
     +            HDRLOG, HDRSIZ
      CHARACTER*8 OLOGBIT,ODATLOG,OORDVAR,OLETVAR,
     +            OPROJECT,OPRDATE,ODATSIZ,OPRTIME,OBEGSNP,OENDSNP,
     +            OHDRLOG, OENDHD
      character*120 headnm
c     CHARACTER*200 CRBUF
      CHARACTER*80 DEBUF(10)
      EQUIVALENCE (DEBUF,INBUF)
C
C-----> Initialize the identifiers
C
      DATSIZ= ' DATSIZ '
      LOGBIT= ' LOGBIT '
      DATLOG= ' DATLOG '
      ORDVAR= ' ORDVAR '
      LETVAR= ' LETVAR '
      ENDHD=  ' ENDHD  '
      PROJECT='/PROJECT'
      PRDATE= '/PRDATE '
      PRTIME= '/PRTIME '
      BEGSNP= '/BEGSNP '
      ENDSNP= '/ENDSNP '
      HDRLOG= '/HDRLOG '
      HDRSIZ= '/HDRSIZ '
C  Old-style GENPRO-II header identifiers
      ODATSIZ  = 'DATSIZ  '
      OLOGBIT  = 'LOGBIT  '
      ODATLOG  = 'DATLOG  '
      OORDVAR  = 'ORDVAR  '
      OLETVAR  = 'LETVAR  '
      OPROJECT = 'PROJECT '
      OPRDATE  = 'PRDATE  '
      OPRTIME  = 'PRTIME  '
      OBEGSNP  = 'BEGSNP  '
      OENDSNP  = 'ENDSNP  '
      OHDRLOG  = 'HDRLOG  '
      OENDHD   = 'ENDHD   '
C
C 
C-----> Initialize the format used to create an equivalence between 
C-----> IBUF and INBUF
C
      ITEMP=8/NCHR
      WRITE(DEFMT,5) ITEMP,NCHR
    5 FORMAT('(',I1,'A',I1,')')
C
C-----> ILEN should contain the number of characters to be read prior to
C-----> the BUFIN call.
C
c      ILEN=800
c....... mod to accommodate N2UW form:
      ILEN = 80
c....... see if there is a separate ASCII-format header file (as for
c..      some N2UW files)
	headnm = flname
        i = 0
5487    i = i + 1
        if(headnm(i:i).ne.' ') goto 5487
        i = i - 1
        write(6,'(" i=",i5,", headnm=",(a))') i, headnm(1:i)
        headnm = headnm(1:i)//"_hd"
        open(21,file=headnm,status='OLD',iostat=ierr,
     &     access='sequential', err=5482, form='formatted')
	usefmt = 1
	write(6,'(" using separate ASCII header file, name=",(a))')
     $   headnm
	goto 5483
5482	usefmt = 0
	write(6,'(" using connected binary header file")')
5483	continue


C
C-----> Initialize the counters
C
      NORDV=0
      NRATE=0
      NTERM=0
      NREC=0
   10 NREC=NREC+1
      if(ihlog.gt.1 .and. NREC.eq.(ihlog+1)) ilen = ihlog*ilen
c....... beware of bad header records that cause long loops
      if(NREC.gt.2000) then
         WRITE(0, '(" too many header records before END")')
         WRITE(0, '(" ending run.  Check if file is GENPRO")')
         WRITE(0, '(" output file and is expected format.")')
         WRITE(0, '(" If N2UW file, try ""formatted"" option.")')
         i = perrr(j)
         call exit(1)
      endif
c      write(6,'("ilen=",i5)') ilen
      if (usefmt.ne.0) then
 	  read(21,'((a))', err=20, end=150) IBUF
c  	  read(21,'((a))', err=20, end=150) CRBUF
c  	  write(6,'(1x,(a))') CRBUF
c  	  call cmove(CRBUF,IBUF,80)
	  istat = -1
	  goto 40
      else
          CALL BUFIN(LUIN,IBUF,ILEN,ISTAT)
      endif
      IF (ISTAT) 40,150,20
C
C-----> PARITY ERROR
C
   20 WRITE(LUOUT,30) NREC
   30 FORMAT(/,' >>> UNRECOVERABLE PARITY ERROR ON HEADER FILE RECORD ',
     &       'NUMBER ',I4,/)
      GOTO 10
C
C-----> GOOD READ
C
C
C-----> Since ANSI FORTRAN 77 doesn't allow equivalencing of character
C-----> variables and integer variables, we must transfer them through
C-----> code
C 
c  40 DO 50 I=1,ISIZ,(8/NCHR) 
C     STARTNOW
   40 DO 50 I=1,(ILEN/NCHR),(8/NCHR) 
         J=(I-1)/(8/NCHR)+1
   50 WRITE(INBUF(J),DEFMT) (IBUF(K),K=I,(I+(8/NCHR)-1))
C
C-----> Write out each logical record while searching for the
C-----> identifiers
C
      DO 120 INDX=1,(ILEN/8),10
        INDXD=INDX/10+1
   60   FORMAT(1X,A80)
        IF (LUOUT.NE.LUBIT) WRITE(LUOUT,60) DEBUF(INDXD)
C
C-----> Decode project number and determine aircraft from first digit
C
C        write(6,'(" INBUF(INDX)=",(a))') INBUF(INDX)
C        IF (INBUF(INDX).EQ.PROJECT) THEN
        IF (INBUF(INDX).EQ.PROJECT .OR. INBUF(INDX).EQ.OPROJECT) THEN
           WRITE(LUTO,60) DEBUF(INDXD)
           READ(DEBUF(INDXD),62,ERR=63) IPROJ,IFLGHT
   62      FORMAT(11X,I3,1X,I2)
   63      IF (IPROJ/100.EQ.2) THEN
             IARCFT='312 '
           ELSE IF (IPROJ/100.EQ.4) THEN
             IARCFT='304 '
           ELSE IF (IPROJ/100.EQ.6) THEN
             IARCFT='306 '
           ELSE IF (IPROJ/100.EQ.8) THEN
             IARCFT='308 '
           ENDIF
c
c-----> get header logical record size (needed to read trailing space)
c
        ELSE IF (INBUF(INDX).EQ.HDRLOG) THEN
           READ(DEBUF(INDXD),'(12x,a2)') hlog
           if(hlog.eq.'10') then
                ihlog = 10
           else
		ihlog = 1
	   endif
        ELSE IF (INBUF(INDX).EQ.OHDRLOG) THEN
           READ(DEBUF(INDXD),'(11x,a2)') hlog
           if(hlog.eq.'10') then
                ihlog = 10
           else
                ihlog = 1
           endif
C 
C-----> Decode Project Date 
C 
        ELSE IF (INBUF(INDX).EQ.PRDATE .OR. INBUF(INDX).EQ.OPRDATE) THEN
           READ(DEBUF(INDXD),65) IDATED(3),MONTH,IDATED(1)
   65      FORMAT(12X,I2,3X,A3,3X,I2)
C
C-----> Datcon is a function to convert 3 char month to integer
C
           IDATED(2)=DATCON(MONTH)

c
c-----> Decode and save starting time
c
c        elseif(inbuf(indx).eq.PRTIME) then
        elseif(inbuf(indx).eq.PRTIME .OR. inbuf(indx).eq.OPRTIME) then
            read(debuf(indxd),965) it1,it2,it3
965         format(12x,i2,4x,i2,4x,i2)
            strttm=it1*3600.+it2*60.+it3
c           write(6,'(" starting time (seconds)=",f9.1)') strttm
c
c-----> Get segment times in case skip needed:
c
c        elseif(inbuf(indx).eq.BEGSNP) then
        elseif(inbuf(indx).eq.BEGSNP .OR. inbuf(indx).eq.OBEGSNP) then
            read(debuf(indxd),966) t1,t2,t3
C966         format(13x,f6.3,7x,f6.3,7x,f6.3)
966         format(10X,3(1X,F12.7))
            ITMSEG(1,1) = t1
            ITMSEG(2,1) = t2
            ITMSEG(3,1) = t3
            BSEC = 3600.*t1+60.*t2+t3
c           write(6,'(" begin time=",f3.0,f3.0,f3.0, "BS=",f8.0)')
c    $            t1,t2,t3,BSEC
C        elseif(inbuf(indx).eq.ENDSNP) then
        elseif(inbuf(indx).eq.ENDSNP .OR. inbuf(indx).eq.OENDSNP) then
            read(debuf(indxd),966) t1,t2,t3
            ITMSEG(4,1) = t1
            ITMSEG(5,1) = t2
            ITMSEG(6,1) = t3
            NTMSEG = 1
            ESEC = 3600.*t1+60.*t2+t3
c           write(6,'(" end time=",f3.0,f3.0,f3.0, ", ES=",f8.0)')
c    $            t1,t2,t3,ESEC
C
C-----> Decode the number of bits per logical data record
C
        ELSE IF (INBUF(INDX).EQ.LOGBIT) THEN
           READ(DEBUF(INDXD),80) ILGBIT
   80      FORMAT(9X,I9,62X)
           write(6,'(" ILGBIT=",i10)') ILGBIT
        ELSE IF (INBUF(INDX).EQ.OLOGBIT) THEN
           READ(DEBUF(INDXD),81) ILGBIT
   81      FORMAT(9X,I6,65X)
c               write(6,'(" ILGBIT=",i10)') ILGBIT
C
        ELSE IF (INBUF(INDX).EQ.DATSIZ) THEN
           READ(DEBUF(INDXD),80) IDATSIZ
        ELSE IF (INBUF(INDX).EQ.ODATSIZ) THEN
           READ(DEBUF(INDXD),81) IDATSIZ
C
C-----> Decode the number of logical data records per physical data
C-----> record
C
        ELSE IF (INBUF(INDX).EQ.DATLOG) THEN
           READ(DEBUF(INDXD),90) IDATLG
   90      FORMAT(9X,I5,66X)
        ELSE IF (INBUF(INDX).EQ.ODATLOG) THEN
           READ(DEBUF(INDXD),91) IDATLG
   91      FORMAT(9X,I4,67X)
C
C        ELSE IF (INBUF(INDX).EQ.ORDVAR) THEN
        ELSE IF (INBUF(INDX).EQ.ORDVAR .OR. INBUF(INDX).EQ.OORDVAR) THEN
           NORDV=NORDV+1
C
C-----> Check for variable information -- NEWER GENPRO FORMAT
C
        ELSE IF (INBUF(INDX).EQ.LETVAR) THEN
           IF (NORDV.EQ.2) THEN
C
C-----> Read a variable's UNITS; decode sample RATE, number of BITS in
C----->  a sample, its starting location, FSTBIT; read the variable name,
C----->  VNAME; compute the variable's index in a logical data record.
C
              NRATE=NRATE+1
              READ(DEBUF(INDXD),100) UNITSG(NRATE),IRATE(NRATE),IBITS,
     +                               IFBIT,VNAME(NRATE)
  100         FORMAT(11X,A4,13X,I6,1X,I3,1X,I8,15X,A8,10X)
              IPOS(NRATE)=((IFBIT-1)/IBITS)+1
           ELSE IF (NORDV.EQ.3) THEN
C
C-----> Decode a variable's TERM and FACTR
C 
              NTERM=NTERM+1 
              READ(DEBUF(INDXD),810) KEY(NTERM),KEYSCL(NTERM),
     $             TERM(NTERM),FACTR(NTERM) 
C     810          FORMAT(16X,F15.8,1X,F14.8,34X)
 810          FORMAT(11X,i1,2X,I1,1X,F15.8,1X,F14.8,34X)
              END IF
C
C-----> Check for variable information -- OLDER GENPRO FORMAT
C
        ELSE IF (INBUF(INDX).EQ.OLETVAR) THEN
C
C-----> Decode a variable's RATE and VNAME
C
           IF (NORDV.EQ.1) THEN
              NRATE=NRATE+1
              READ(DEBUF(INDXD),105) IRATE(NRATE),VNAME(NRATE)
  105         FORMAT(58X,I4,8X,A8,2X)
C
C-----> Decode a variable's UNITS, TERM, FACTOR, FSTBIT
C
           ELSE IF (NORDV.EQ.2) THEN
              NTERM=NTERM+1
              READ(DEBUF(INDXD),115) UNITSG(NTERM),TERM(NTERM),
     +                               FACTR(NTERM),IBITS,IFBIT
  115         FORMAT(11X,A4,6X,F7.1,1X,F7.1,4X,I3,1X,I6,30X)
              IPOS(NTERM)=((IFBIT-1)/IBITS)+1
c----> force KEY = 1 (used in rdata to translate data) for integer-translation
              KEY(NTERM) = 1
           END IF
C
        ELSE IF ((INBUF(INDX).EQ.ENDHD .OR. INBUF(INDX).eq.OENDHD)
     $		.and. DEVNAME.EQ."fff") THEN
           GOTO 150
 	ELSE
c	   write(6,'(" no match to ",a8".")') INBUF(INDX)
        END IF
  120 CONTINUE
      GOTO 10
C
C-----> EOF SENSED
C
  150 WRITE(LUOUT,160) NREC-1
      WRITE(LUTO,160) NREC-1
  160 FORMAT(/' EOF sensed on header file after ',I4,' records',/)
C
C-----> Assign the number of variables and set the NLOGRC pointer to
C-----> IDATLG so that when GENDAT is called a physical data record is
C-----> read immediately
C
      NVARS=NRATE
      NLOGRC=IDATLG
      RETURN
      END
C 
      SUBROUTINE BRKHAN
      INCLUDE 'g2inp.com'
      BRFLG=.TRUE.
      RETURN
      END
C 
C 
C 
      SUBROUTINE BUFIN(LUN,IBUF,ILEN,ISTAT) 
c....... ifmtd added to g2inp to flag formatted-read input (ifmtd==1)
      INCLUDE 'g2inp.com'
      INTEGER*4 IBUF(1)
      data ifirst/0/
      ISTAT=-1
c     write(6,'(" bnread call, ifd,ilen,LUN=",3i5)') ifd,ilen,LUN
c....... special version for formatted file:
      if(ifmtd.ne.0) then
          ILN = 4
          call bnread(IFD,IBUF,ILN,NLN,IERR,0)
          if (ifd.lt.1 .or. ierr.gt.0) then
	      istat = 1
              if (NLN.eq.0) ISTAT = 0
              return
          endif
          ILN = IBUF(1)+4
          call bnread(IFD, IBUF, ILN, NLEN, IERR, 0)
          if (NLEN.eq.4 .and. ifirst.eq.0) then
             ifirst = 1
	     ILN = 4
             call bnread(IFD, IBUF, ILN, NLN,  IERR, 0)
              if (ifd.lt.1 .or. ierr.gt.0) then
                  istat = 1
                  if (NLN.eq.0) ISTAT = 0
                  return
              endif
              ILN = IBUF(1)+4
              call bnread(IFD, IBUF, ILN, NLEN, IERR, 0)
          endif
          NLEN = NLEN - 4
c         kk = NLEN/4-1
c         write(6,'(" ILN=",i6,1x, (20a4))') ILN, (IBUF(k),k=1,kk)
      else
          CALL BNREAD(IFD,IBUF,ILEN,NLEN,IERR,0)
      endif
      IF (ifd.lt.1 .or. IERR.GT.0) ISTAT=1
      IF (NLEN.EQ.0) ISTAT=0
c     write(6,10) ILEN,NLEN,IERR
 10   format(" Read done: ILEN=",i5," NLEN=",i5," IERR=",i5)
c     write(6,'(" 1st 10 words of read: ", 10i10)') (IBUF(i), i=1,10)
      RETURN
      END 
      BLOCK DATA
      INCLUDE 'tmout.com'
      DATA NTMOUT/1/,ITMOUT/0,0,0,30,0,0,294*0/ 
      END 
C 
C 
C 
C 
      INTEGER FUNCTION DATCON(MONTH)
      CHARACTER*3 MONTH,MOS(12) 
      DATA (MOS(I),I=1,12) /'JAN','FEB','MAR','APR','MAY','JUN',
     +                      'JUL','AUG','SEP','OCT','NOV','DEC'/
C 
      CALL CAPTL(MONTH,3)
      DO 10 I=1,12 
        IF (MONTH.EQ.MOS(I)) THEN 
          DATCON=I
          RETURN
        ENDIF 
   10 CONTINUE
      RETURN
      END 
