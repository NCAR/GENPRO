      SUBROUTINE CALVEC                                                         
C                                                                               
C   Create the vecvar for the Calib operation. Do so by selecting               
C   all variables chosen by user for the Plot/Stats/Print operations            
C   (PSPPTR array pointing into VRPOSS) and, by consulting GPGENDER             
C   DBASE (RAFDMG 193), adding any source vars not already present              
C   ...as well, add any bulletin 9 vars that are on header but not in           
C   the PSPPTR array.  This must be done so Vecvar is complete for              
C   Output operation, who is expecting these variables.                         
C   -- also create the CALIBS file for this project number                      
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INTEGER  NMSORC,VECNUM,NUMDEF,NMLEFT                                      
C NMSORC -- max number of source vars per derived var                           
C VECNUM -- number of vecvar vars selected                                      
C NUMDEF -- number of default vars always present in calib vecvar               
      PARAMETER (NMSORC=10,NUMDEF=6,NMLEFT=MAXNAM-NUMDEF)                       
      CHARACTER *8 SOURCE(NMSORC),DERVAR,STDVAR(NMSORC)                         
      CHARACTER *8 KEY,VECVAR(MAXNAM),OUTVAR                                    
      CHARACTER *40 STRING                                                      
      LOGICAL ADDED                                                             
C list of default vars: present in every flight                                 
      DATA STDVAR/'QCXC','PSXC','TASX','DPXC','ATX','QCX','RHOX','AKX',         
     $ 'SSX','TTX'/                                                             
C list of vars that must always be present in Calib Vecvar                      
      DATA VECVAR/'HR','MIN','SEC','PTIME','TPTIME','ALPHA',NMLEFT*' '/         
      VECNUM=NUMDEF                                                             
C locate source vars for each var in PSPPTR                                     
      DO 100 J=1,PSPOUT                                                         
       OUTVAR=VRPOSS(PSPPTR(J))                                                 
C top of file / skip first two records                                          
C search for current output variable                                            
       REWIND (34)                                                              
       READ (34,'(A80)')                                                        
       READ (34,'(A80)')                                                        
   10  READ(34,'(A40,1X,I1,1X,A8)',END=99)                                      
     $ STRING,NUMSOU,DERVAR                                                     
       IF(DERVAR.NE.OUTVAR) GOTO 10                                             
C found it: extract source variables                                            
C initialize indices used to parse source variables out of STRING               
       IGO=-7                                                                   
       IEND=0                                                                   
C initialize number of VALID source vars in this list                           
       NMSRC=0                                                                  
       DO 50 JJ=1,NUMSOU                                                        
C extract each of NUMSOU source vars from STRING                                
        IGO=IGO+8                                                               
        IEND=IEND+8                                                             
C if this source var in STDVAR, then it is automatically OK: don't save         
        DO 45 K=1,NMSORC                                                        
   45    IF(STRING(IGO:IEND).EQ.STDVAR(K)) GOTO 50                              
C ...else it is a VALID source var to be used in ensuing search                 
        NMSRC=NMSRC+1                                                           
        SOURCE(NMSRC)=STRING(IGO:IEND)                                          
   50  CONTINUE                                                                 
C search PSPPTR to verify all source vars are present                           
       DO 55 K=1,NMSRC                                                          
        CALL SERCH2(SOURCE(K),VRPOSS,PSPPTR,PSPOUT,INDX)                        
        IF (INDX.EQ.0) THEN                                                     
C        WRITE(6,'('' source var '',A8,'' not present '')')SOURCE(K)            
         VECNUM=VECNUM+1                                                        
         VECVAR(VECNUM)=SOURCE(K)                                               
        ENDIF                                                                   
   55  CONTINUE                                                                 
C add the current output var                                                    
   99  CONTINUE                                                                 
       VECNUM=VECNUM+1                                                          
       VECVAR(VECNUM)=OUTVAR                                                    
  100 CONTINUE                                                                  
C add any bulletin 9 vars on header but not in PSP array                        
C     WRITE(6,'('' NMODE2: '',I6,''; NMBUL9: '',I6)')                           
C    $NMODE2,NMBUL9                                                             
      DO 150 J=1,NMODE2                                                         
C      WRITE(77,'('' SDINAM: '',A8)') SDINAM(MODE2(J))                          
       DO 145 K=1,PSPOUT                                                        
C if the header var is already chosen, this is good                             
        IF (SDINAM(MODE2(J)).EQ.VRPOSS(PSPPTR(K))) GOTO 150                     
 145   CONTINUE                                                                 
C  the header var is not chosen; check if it is a Bulletin 9 var                
C      WRITE(77,'(''   NOT IN PSPPTR'')')                                       
       DO 140 K=1,NMBUL9                                                        
       IF (K.GT.MAXNAM) THEN                                                    
        WRITE(6,'('' K > MAXNAM; NMBUL9 = '',I6)')  NMBUL9                      
        CALL PAUSER(' Contact GUINT expert ... hit <r> to continue')            
        STOP                                                                    
       ENDIF                                                                    
       IF (BULL9(K).GT.MAXNAM) THEN                                             
        WRITE(6,'('' BULL9('',I4,'') > MAXNAM'')')  K                           
        CALL PAUSER(' Contact GUINT expert ... hit <r> to continue')            
        STOP                                                                    
       ENDIF                                                                    
       IF (J.GT.DIMSDI) THEN                                                    
        WRITE(6,'('' J > DIMSDI; NMODE2 = '',I6)')  NMODE2                      
        CALL PAUSER(' Contact GUINT expert ... hit <r> to continue')            
        STOP                                                                    
       ENDIF                                                                    
       IF (MODE2(J).GT.DIMSDI) THEN                                             
        WRITE(6,'('' MODE2('',I4,'') > DIMSDI'')')  J                           
        CALL PAUSER(' Contact GUINT expert ... hit <r> to continue')            
        STOP                                                                    
       ENDIF                                                                    
        IF (SDINAM(MODE2(J)).EQ.VNAME(BULL9(K))) THEN                           
C  if so, add it to Calib Vecvar                                                
C        WRITE(77,'(''   ADD TO CALIB VECVAR'')')                               
         VECNUM=VECNUM+1                                                        
         VECVAR(VECNUM)=SDINAM(MODE2(J))                                        
         GOTO 150                                                               
        ENDIF                                                                   
 140   CONTINUE                                                                 
C      WRITE(77,'(''   NOT IN BULLETIN 9'')')                                   
C      GOTO 150                                                                 
C149   WRITE(77,'(''   YES IN PSPPTR'')')                                       
 150  CONTINUE                                                                  
C now write the list to the Direct file                                         
      WRITE(21) VECNUM,(VECVAR(J),J=1,VECNUM)                                   
C create list of files containing needed routines for processing vars           
      CALL CALIBS(VECNUM,VECVAR)                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE CALIBS(VECNUM,VECVAR)                                          
C                                                                               
C  create list of files needed to process the given list of variables           
C  -- read from PROCSSRS <project number> file, create the CALIBS               
C     <project number> file (CMS implementation)                                
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gppdata.h"                                                       
C                                                                               
C VECVAR -- list of selected vars                                               
C VECNUM -- number of vecvar vars selected                                      
C                                                                               
      INTEGER VECNUM                                                            
      CHARACTER *8 VECVAR(VECNUM)                                               
C                                                                               
C  create file of GETSRC statements that indicate which files to acquire        
C  from IBM 4381 disks (CMS implementation) to process requested variable       
C  list  (maximum of 30 total)                                                  
C                                                                               
C  filename, filetype, machine of files (CMS)                                   
C                                                                               
      CHARACTER*8 FILNAM(30),FILTYP(30),MACHIN(30)                              
C                                                                               
C  virtual address of files (CMS)                                               
C                                                                               
      CHARACTER*3 VADDR(30)                                                     
C                                                                               
      COMMON/LIST/FILNAM,FILTYP,MACHIN,VADDR                                    
C                                                                               
C  start symbol in file                                                         
C                                                                               
      CHARACTER*10 START                                                        
C                                                                               
C                                                                               
C  non-standard variable names  -- 10 maximum                                   
C                                                                               
      CHARACTER*8 NONSTD(10)                                                    
C                                                                               
C  PMS -- true if PMS probes are in use                                         
C                                                                               
      LOGICAL PMS                                                               
C                                                                               
C list of variables that imply radome is being used                             
C                                                                               
      PARAMETER (NMRADS=3,NMWIND=1)                                             
      CHARACTER*5 RADVAR(NMRADS)                                                
      CHARACTER*3 RDWIND(NMWIND)                                                
C                                                                               
C  radome present? RADOME = 'R':' '                                             
C                                                                               
      CHARACTER *1 RADOME                                                       
C                                                                               
C  radome winds wanted? WINDS='R': ' '                                          
C                                                                               
C  number of non-std vars for each non-std processor                            
C                                                                               
      DIMENSION NVARS(30)                                                       
C                                                                               
C initialize list of vars that imply radome is being used                       
      DATA RADVAR/'QCR','ADIFR','BDIFR'/RADOME/' '/                             
C initialize vars that imply radome winds are wanted                            
C     DATA RDWIND/'UIR'/                                                        
C  initialize number of files                                                   
      NMFILS=1                                                                  
C                                                                               
C  read in file of standard and non-standard processors                         
C                                                                               
      REWIND(47)                                                                
      REWIND(48)                                                                
C read until get past BEGIN record                                              
    1 READ(47,'(A10)') START                                                    
      IF (START.NE.'BEGIN LIST') GOTO 1                                         
    5 READ(47,'(3(A8,1X),A3)',END=10) FILNAM(NMFILS),FILTYP(NMFILS),            
     $ MACHIN(NMFILS),VADDR(NMFILS)                                             
C     WRITE(48,'(3(A8,1X),A3)',END=10) FILNAM(NMFILS),FILTYP(NMFILS),           
C    $ MACHIN(NMFILS),VADDR(NMFILS)                                             
      IF(FILNAM(NMFILS).EQ.'NON-STD ') THEN                                     
C adjust number of files to add the NON-STD line (needed as flag later)         
       NMFILS=NMFILS+1                                                          
C initialize total number of non-std vars                                       
       NMVARS=0                                                                 
C                                                                               
C get list of non-standard processors                                           
C  read total special files                                                     
       READ(47,'(BN,I2)') NMSPEC                                                
C      WRITE(48,'('' # NON-STD FILES: '',BN,I2)') NMSPEC                        
       DO 6 J=NMFILS,NMFILS+NMSPEC-1                                            
        READ(47,'(3(A8,1X),A3,1X,BN,I2)')FILNAM(J),FILTYP(J),                   
     $  MACHIN(J),VADDR(J),NVARS(J-NMFILS+1)                                    
C get NVARS lines worth of the vars that imply this non-std need                
        DO 66 K=NMVARS+1,NMVARS+NVARS(J-NMFILS+1)                               
   66    READ(47,'(A8)') NONSTD(K)                                              
C       WRITE(48,'(3(A8,1X),A3,1X,BN,I2)')FILNAM(J),FILTYP(J),                  
C    $  MACHIN(J),VADDR(J),NVARS(J-NMFILS+1)                                    
C       DO 67 K=NMVARS+1,NVARS(J-NMFILS+1)+NMVARS                               
C  67    WRITE(48,'(A8)') NONSTD(K)                                             
C accumulate total special vars                                                 
        NMVARS=NMVARS+NVARS(J-NMFILS+1)                                         
    6  CONTINUE                                                                 
C accumulate total files                                                        
       NMFILS=NMFILS+NMSPEC                                                     
      ELSE                                                                      
       NMFILS=NMFILS+1                                                          
      ENDIF                                                                     
      GOTO 5                                                                    
C adjust due to end of file                                                     
   10 NMFILS=NMFILS-1                                                           
C     WRITE(48,'('' NONSTD VARS: '',I3)') NMVARS                                
      DO 101 J=1,NMVARS                                                         
C      WRITE(48,'(A8)') NONSTD(J)                                               
  101 CONTINUE                                                                  
C                                                                               
C  determine if Radome is being used                                            
C                                                                               
      DO 15 J=1,VECNUM                                                          
       DO 15 K=1,NMRADS                                                         
        IF (VECVAR(J).EQ.RADVAR(K)) THEN                                        
         RADOME='R'                                                             
         GOTO 20                                                                
        ENDIF                                                                   
   15 CONTINUE                                                                  
   20 CONTINUE                                                                  
C                                                                               
C  determine if Radome winds requested                                          
C                                                                               
C ******** THIS NEXT BLOCK MOVED TO GPFLTGEN FORTRAN *********                  
C     WINDS=' '                                                                 
C     DO 16 J=1,VECNUM                                                          
C      DO 16 K=1,NMWIND                                                         
C  assuming only 3-character names used in RDWIND array                         
C       IF (VECVAR(J)(1:3).EQ.RDWIND(K)) THEN                                   
C        WINDS='R'                                                              
C        GOTO 21                                                                
C       ENDIF                                                                   
C  16 CONTINUE                                                                  
C  21 CONTINUE                                                                  
C                                                                               
C  determine if any PMS probes are used                                         
C                                                                               
      PMS=.FALSE.                                                               
      DO 25 J=1,VECNUM                                                          
       DO 25 K=1,NMPROB                                                         
        IF (VECVAR(J)(1:4).EQ.PNAM(K)) THEN                                     
         PMS=.TRUE.                                                             
C        PAUSE ' PMS TRUE'                                                      
         GOTO 30                                                                
        ENDIF                                                                   
   25 CONTINUE                                                                  
   30 CONTINUE                                                                  
C                                                                               
C  adjust filenames and filetypes to correspond to this project's status:       
C  replace &A with aircraft number; replace &R with 'R' if radome is in         
C  use, else replace with null character.                                       
C                                                                               
      DO 40 J=1,NMFILS                                                          
C CMS implementation: CALS<aircraft>R filenames, filetype IFTV3                 
       IPOS=INDEX(FILNAM(J),'&A')                                               
       IF(IPOS.NE.0) FILNAM(J)(IPOS:IPOS+3)=IARCFT//RADOME                      
C <aircraft>R filetypes on ACTSAV filename                                      
       IPOS=INDEX(FILTYP(J),'&A&R')                                             
       IF(IPOS.NE.0) FILTYP(J)(IPOS:IPOS+3)=IARCFT//RADOME                      
   40 CONTINUE                                                                  
C                                                                               
C  write out list to disk                                                       
C  initialize index into filenames                                              
      JJ=0                                                                      
   42 JJ=JJ+1                                                                   
C                                                                               
C  if any non-standard vars are requested, write out its associated file        
C                                                                               
C     WRITE(48,'('' FILNAM('',I2,'') = '',A8)') JJ,FILNAM(JJ)                   
      IF(FILNAM(JJ).EQ.'NON-STD ') THEN                                         
C total special vars                                                            
       NMVARS=0                                                                 
C look at each non-std file                                                     
       DO 39 N=1,NMSPEC                                                         
C adjust index into filenames to bypass the NON-STD flag                        
        JJ=JJ+1                                                                 
C       WRITE(48,'('' THERE ARE '',I4,'' SPEC. VARS FOR '',                     
C    $  '' SPECIAL FILE '',A8)') NVARS(N),FILNAM(JJ)                            
C look at each associated var for current non-std file                          
        DO 37 J=NMVARS+1,NMVARS+NVARS(N)                                        
C        WRITE(48,'(''AND THEY ARE (NONSTD('',I2,''))'',A8)')J,NONSTD(J)        
C search list of requested variables for a match                                
         DO 35 K=1,VECNUM                                                       
          IF (VECVAR(K).EQ.NONSTD(J)) THEN                                      
C          WRITE(48,'(''NEED NONSTD: '',A8,'';JJ='',I3)')NONSTD(J),JJ           
C match found; write out JCL and break out of loop to look at next file         
           CALL WRTFIL(JJ)                                                      
           GOTO 38                                                              
          ENDIF                                                                 
   35    CONTINUE                                                               
C        WRITE(48,'('' NOT FOUND '')')                                          
   37   CONTINUE                                                                
   38   NMVARS=NMVARS+NVARS(N)                                                  
C       WRITE(48,'('' NEW START INDEX INTO NONSTD: '',I3)') NMVARS+1            
   39  CONTINUE                                                                 
      ELSE                                                                      
C simply write out JCL for a standard processor                                 
       IF (FILNAM(JJ).EQ.'CALPMS'.AND.(.NOT.(PMS))) GOTO 45                     
C      WRITE(48,'('' WRITE STD; JJ='',I3)') JJ                                  
       CALL WRTFIL(JJ)                                                          
      ENDIF                                                                     
   45 IF (JJ.LT.NMFILS) GOTO 42                                                 
C  done                                                                         
      RETURN                                                                    
      END                                                                       
C                                                                               
C                                                                               
      SUBROUTINE WRTFIL(NUM)                                                    
C                                                                               
C  filename, filetype, machine of files (CMS)                                   
C                                                                               
      CHARACTER*8 FILNAM(30),FILTYP(30),MACHIN(30)                              
C                                                                               
C  virtual address of files (CMS)                                               
C                                                                               
      CHARACTER*3 VADDR(30)                                                     
C                                                                               
      COMMON/LIST/FILNAM,FILTYP,MACHIN,VADDR                                    
C                                                                               
C                                                                               
C  write out the JCL to acquire the NUM'th file in list of processors           
C                                                                               
      WRITE(48,'('' GETSRC,&PROJECT,'',                                       
     $ 3(A8,'',''),A3,''.'')') FILNAM(NUM),FILTYP(NUM),MACHIN(NUM),             
     $ VADDR(NUM)                                                               
      RETURN                                                                    
      END                                                                       
