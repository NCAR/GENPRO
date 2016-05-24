      SUBROUTINE SETOP
C*      Copyright University Corporation for Atmospheric Research, 1995   *
      PARAMETER (NCOMD=9) 
      INCLUDE 'g2inp.com' 
      INCLUDE 'slus.prm' 
      INCLUDE 'tmout.com'
      CHARACTER*80 IN80 
      CHARACTER*7 DEFMT 
      CHARACTER*6 SIXCH 
      CHARACTER*5 TBUFF 
      CHARACTER*2 COMD,CMDLS(NCOMD) 
      CHARACTER*1 INPT(80)
      INCLUDE 'file.com'
      LOGICAL SHFLG 
      EQUIVALENCE (IN80,INPT) 
      DATA CMDLS/'RT','AV','DV','TI','ML','DI','/E','EX','SH'/
      SHFLG=.TRUE.
   40 IF (SHFLG) WRITE(LUTO,50) 
   50 FORMAT(/,15X,'*** DAP GENPRO II INPUT ROUTINE ***',/,22X,
     &       'Rev', 
     &       ' <880201.1143> ', 
     &       'MDD',//,1X,78('-'),/,
     &     5X, ' Command        Description',/,
     &     5X, ' RT,secs[,MS]   change data rate',/, 
     &     5X, ' AV,vname(ALL)  add variable(s) to DAP file',/,
     &     5X, ' DV,vname(ALL)  delete variable(s) from DAP file',/, 
     &     5X, ' TI             show/change time periods',/, 
     &     5X, ' ML             show list of variables recorded on tape'
     &  ,/,5X, ' DI             show current list of variables in DAP '
     &                                                         ,'file'
     &  ,/,5X, ' SH             turn off/on menu listing',/, 
     &     5X, ' /E             begin processing',/, 
     &     5X, ' EX             terminate without processing',/,
     &     1X,78('-'),//)
      WRITE(LUTO,60) IDELTT,NVAR
   60 FORMAT(' Current data rate is ',I4,' milliseconds',/,
     &       1X,I3,' variables are to be written in the DAP file',//,
     &       ' G2INP: Command? ',$) 
      READ(LUTI,70) IN80
   70 FORMAT(A) 
      CALL CAPTL(IN80,80)
      WRITE(COMD,75) INPT(1),INPT(2)
   75 FORMAT(2A1) 
      DO 80 I=1,NCOMD 
      IF (COMD.EQ.CMDLS(I)) THEN
         ICOM=I 
         GOTO 95
         END IF 
   80 CONTINUE
      WRITE(LUTO,90) COMD 
   90 FORMAT(1X,A2,' command not found') 
      GOTO 40 
   95 N=6 
      GOTO (100,200,300,400,500,600,700,800,900), ICOM
C 
C-----> RT command
C 
  100 ICOM=8
      DO 110 I=4,8
      IF (INPT(I).EQ.',') THEN
         ICOM=I 
         GOTO 120 
         END IF 
  110 CONTINUE
  120 WRITE(DEFMT,130) ICOM-4,IABS(ICOM-9)
  130 FORMAT('(',I1,'A,',I1,'X)') 
      WRITE(TBUFF,DEFMT) (INPT(I),I=4,(ICOM-1)) 
      write(6,'(" tbuf=",a5)') tbuff
      write(DEFMT,'("(I",i1,")   ")') (icom-4)
c     READ(TBUFF,*,ERR=140) INSEC 
      read(tbuff,DEFMT) INSEC
      write(6,'(" defmt=",a7,", insec=",i5)') DEFMT,INSEC
      IF (ICOM.NE.8) THEN 
         IDELTT=INSEC 
       ELSE 
         IDELTT=INSEC*1000
         END IF 
      GOTO 40 
  140 WRITE(LUTO,150) 
  150 FORMAT(' Argument must be numeric')
      GOTO 40 
C 
C-----> AV command
C 
  200 WRITE(SIXCH,210) (INPT(I),I=4,9)
  210 FORMAT(6A1) 
      IF (SIXCH.EQ.'ALL   ') THEN 
         NVAR=NVARS 
         DO 215 I=1,NVARS 
         NAMES(I)=VNAME(I)
         UNITS(I)=UNITSG(I) 
         IND(I)=I 
  215    CONTINUE 
         GOTO 40
         END IF 
      DO 220 I=1,NVARS
      IF (SIXCH.EQ.VNAME(I)) THEN 
         NVAR=NVAR+1
         NAMES(NVAR)=VNAME(I) 
         UNITS(NVAR)=UNITSG(I)
         IND(NVAR)=I
         GOTO 40
         END IF 
  220 CONTINUE
      WRITE(LUTO,230) SIXCH 
  230 FORMAT(1X,A6,' not found in the master list, use ML for list') 
      GOTO 40 
C 
C-----> DV command
C 
  300 WRITE(SIXCH,210) (INPT(I),I=4,9)
      IF (SIXCH.EQ.'ALL   ') THEN 
         NVAR=0 
         GOTO 40
         END IF 
      DO 310 I=1,NVAR 
      IF (SIXCH.EQ.NAMES(I)) THEN 
         IDEL=I 
         GOTO 330 
         END IF 
  310 CONTINUE
      WRITE(LUTO,320) SIXCH 
  320 FORMAT(1X,A6,' not found in variable list, use DI for list') 
      GOTO 40 
  330 DO 340 I=IDEL,NVAR-1
      NAMES(I)=NAMES(I+1) 
      UNITS(I)=UNITS(I+1) 
      IND(I)=IND(I+1) 
  340 CONTINUE
      NVAR=NVAR-1 
      GOTO 40 
C 
C-----> TI command
C 
  400 CALL TIINP(ITMOUT,NTMOUT,LUTI,LUTO,LUBIT,IER1,IER1) 
      GOTO 40 
C 
C-----> ML command
C 
  500 WRITE(LUTO,510) (VNAME(I),I=1,NVARS)
  510 FORMAT(1X,10(A6,1X)) 
      WRITE(LUTO,520) 
  520 FORMAT(' Press [r] to continue... ',$) 
      READ(LUTI,70) IN80
      GOTO 40 
C 
C-----> DI command
C 
  600 WRITE(LUTO,510) (NAMES(I),I=1,NVAR) 
      WRITE(LUTO,520) 
      READ(LUTI,70) IN80
      GOTO 40 
C 
C-----> /E command
C 
  700 QUIT=.FALSE.
      RETURN
C 
C-----> EX command
C 
  800 QUIT=.TRUE. 
      RETURN
C 
C-----> SH command
C 
  900 IF (SHFLG) THEN 
         SHFLG=.FALSE.
       ELSE 
         SHFLG=.TRUE. 
         END IF 
      GOTO 40 
      END 
