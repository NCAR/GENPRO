      PROGRAM GPGENSET
C  Modified to run on IBM4381 Fortran...                                        
C                                                                               
C  See GPGUINT EXEC A for information on logical unit assignments               
C                                                                               
C                                                                               
C  Any routines that perform interactive IO should                              
C  include the INCLUDE (GPIO) statement                                         
C                                                                               
      include "gppdata.h"
      include "gpio.h"
      include "gpifile.h"
C     include "gpusrvar.h"
      LOGICAL TAPLOG                                                            
C   handle non-integer input on READ statements                                 
      INTEGER INTERR                                                            
      character*80 gspath
      character*1 exist
      COMMON/IOERR/INTERR                                                       
      EXTERNAL ERROR                                                            
      INTERR=0                                                                  
C     CALL ERRSET(215,255,-1,1,ERROR)                                           
C project name/number/whether tape log exists (stacked by GPGENSET EXEC)        
C get project number, derive aircraft number                                    
      call GETARG(1,iproj)
      call getarg(3,arg2)
      call GETARG(2,nmproj)
      call getarg(4,exist)
      call getenv('genset',gspath)
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
      taplog = .false.
      if (exist .eq. "T") then
         taplog = .true.
      endif
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
C-----------------------start to open 9
      do 18, i = 1, 80
         fulpth(i:i) = ' '
 18   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/header.'
      fulpth(lindex+9:lindex+11) = iproj
C------------recl now causes errors
      open (unit=9,file=fulpth,access='sequential',status='old',
     +  form='formatted')
C    $recl=80)
C--------------------------------------start to open 51
      if (taplog) then
         do 13, i = 1, 80
            fulpth(i:i) = ' '
 13      continue
         fulpth(1:lindex) = arg2(1:lindex)
         fulpth(lindex+1:lindex+8) = '/taplog.'
         fulpth(lindex+9:lindex+11) = iproj
         open (unit=51,file=fulpth,access='sequential',status='old',
     $          form='formatted')
      endif
C-------------start to open 10
      do 16, i = 1, 80
         fulpth(i:i) = ' '
 16   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/gpvar.dba'
      open (unit=10,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 21
      do 32, i = 1, 80
         fulpth(i:i) = ' '
 32   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/direct.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=21,file=fulpth,form='unformatted',access='sequential')
C-------------start to open 33
      do 33, i = 1, 80
         fulpth(i:i) = ' '
 33   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/gpderive.'
      fulpth(lindex+11:lindex+13) = "dba"
      open (unit=33,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 34
      do 34, i = 1, 80
         fulpth(i:i) = ' '
 34   continue
C---------adjust mindex to the end of the string gspath
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/gpgender.'
      fulpth(lindex+11:lindex+13) ="dba"
      open (unit=34,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 44
      do 35, i = 1, 80
         fulpth(i:i) = ' '
 35   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/fltfile.'
      fulpth(lindex+10:lindex+12) =iproj
      open (unit=44,file=fulpth,access='sequential',form='formatted')
C-------------start to open 46
      do 36, i = 1, 80
         fulpth(i:i) = ' '
 36   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/fltdecks.'
      fulpth(lindex+11:lindex+13) =iproj
      open (unit=46,file=fulpth,access='sequential',form='formatted')
C-------------start to open 47
      do 37, i = 1, 80
         fulpth(i:i) = ' '
 37   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+10) = '/procssrs.'
      fulpth(lindex+11:lindex+13) =iproj
      open (unit=47,file=fulpth,access='sequential',status='old',
     $          form='formatted')
C-------------start to open 48
      do 38, i = 1, 80
         fulpth(i:i) = ' '
 38   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/calibs.'
      fulpth(lindex+9:lindex+11) =iproj
      open (unit=48,file=fulpth,access='sequential',form='formatted')
C-------------start to open 49
      do 39, i = 1, 80
         fulpth(i:i) = ' '
 39   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/plot1.'
      fulpth(lindex+8:lindex+10) =iproj
      open (unit=49,file=fulpth,access='sequential',form='formatted')
C-------------start to open 50
      do 40, i = 1, 80
         fulpth(i:i) = ' '
 40   continue
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+7) = '/plot2.'
      fulpth(lindex+8:lindex+10) =iproj
      open (unit=50,file=fulpth,access='sequential',form='formatted')
C     READ(5,'(A16,1X,A3,1X,L1)') NMPROJ,IPROJ,TAPLOG                           
C if tape log exists, calculate the output snapshot length                      
      IF (TAPLOG) CALL TAPLEN(NSXOUT)                                           
C default user portion of title to project name                                 
      USRTIT=NMPROJ                                                             
C  default downstream processing to stop ... in case this process aborts        
C     OPEN(21,FORM='UNFORMATTED')                                               
      EXEC=.FALSE.                                                              
      REWIND(21)                                                                
      WRITE(21) EXEC                                                            
C     CLOSE(21)                                                                 
C  read header                                                                  
      CALL RDHDR                                                                
C  offer options                                                                
      CALL NSEG1                                                                
      END                                                                       
      BLOCK DATA GENBLK                                                         
C     IMPLICIT INTEGER *2 (I-N)                                                 
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpio.h"                                                          
      INCLUDE "gpifile.h"                                                       
C     INCLUDE "gpusrvar.h"                                                      
C   input/output logical units & flags                                          
C                                                                               
      DATA LUTO/6/ LUTI/5/ LUE/5/ IERR/0/ LUNPR/7/                              
C  See above listed include files for explanations of following                 
      DATA MODE1/DIMD13*0/ MODE2/DIMSDI*0/ NLORN/0/                             
      DATA NMODE1/0/ NMODE2/0/ NPMS1D/0/ NVOLT/0/ 
C     DATA NMUSRV/0/                     
      DATA NHSKP/0/ NPVARS/0/ PRNAME/10*' '/ PTRACC/NACCUM*0/                   
      DATA PTRCON/NCONC*0/ NACC/NMPROB*0/ NCON/NMPROB*0/                        
     1NPROB/NMPROB*0/                                                           
C     DATA USRVEC/MAXUVC * ' '/                                                 
C  The following are the names for the INS block                                
      DATA NDERIV/ 6 / NMODE3/ 6 /                                              
      DATA DERIV /'ALAT    ','ALON    ',                                        
     1            'THI     ','ALPHA   ','XVI     ','YVI     ',                  
     2           194 * ' '/                                                     
      DATA MODE3/1,2,3,4,5,6,194 * 0/                                           
C  PMS setups are in order listed in PNAM                                       
      DATA PNAM/'FSSP','ASAS','X200','Y200','X260'/                             
C                                                                               
C  PMSTAB is lookup table for indices and lengths for each of 5 probes          
C  into arrays PMSFAX (accumulations) and PCONC (concentrations)                
C  ...words 1 to 5: index into PMSFAX; words 6 to 10: # words for each          
C     words 11 to 15: index into PCONC; words 16 to 20: # words                 
C ... assumes ordering in memory as listed below in DATA stmts                  
      DATA PMSTAB/ 1, 21, 36, 51, 66, 20, 15, 15, 15, 62,                       
     $             1, 16, 31, 46, 61, 15, 15, 15, 15, 62/                       
C                                                                               
C  additional probe parameter names                                             
C                                                                               
C*** CONCx -- total concentrations of each probe                                
C*** DBARx -- mean diameter of each probe                                       
C*** DISPx -- dispersion factor of each probe                                   
C*** PLWCx -- liquid water content of each probe                                
C*** DBZx -- the reflectivity factor of each probe                              
C                                                                               
      DATA XINDX/1,10,14,19,24,29/         
      DATA XNAMES/'CONCF','DBARF','DISPF','PLWCF','FACT','FRANGE',
     $'FSTROB','FBMFR','FRESET',
     $'CONCA','DBARA','DISPA','AACT',                                           
     $'CONCX','DBARX','DISPX','PLWCX','DBZX',                                   
     $'CONCY','DBARY','DISPY','PLWCY','DBZY',                                   
     $'CONC6','DBAR6','DISP6','PLWC6','DBZ6'/                                   
C  accumulation variable names                                                  
      DATA PMSFAX/'FACT','FRANGE','FSTROB','FBMFR','FRESET'/                    
      DATA PMSNMF/'AFSP01','AFSP02','AFSP03','AFSP04','AFSP05',                 
     $'AFSP06','AFSP07','AFSP08','AFSP09','AFSP10','AFSP11',                    
     $'AFSP12','AFSP13','AFSP14','AFSP15'/                                      
      DATA PMSNMA/'AASS01','AASS02','AASS03','AASS04','AASS05',                 
     $'AASS06','AASS07','AASS08','AASS09','AASS10','AASS11',                    
     $'AASS12','AASS13','AASS14','AASS15'/                                      
      DATA PMSNMX/'A20X01','A20X02','A20X03','A20X04','A20X05','A20X06',        
     $'A20X07','A20X08','A20X09','A20X10','A20X11','A20X12','A20X13',           
     $'A20X14','A20X15'/                                                        
      DATA PMSNMY/'A20Y01','A20Y02','A20Y03','A20Y04','A20Y05','A20Y06',        
     $'A20Y07','A20Y08','A20Y09','A20Y10','A20Y11','A20Y12','A20Y13',           
     $'A20Y14','A20Y15'/                                                        
      DATA PMSNM6/'A26X01','A26X02','A26X03','A26X04','A26X05','A26X06',        
     $'A26X07','A26X08','A26X09','A26X10','A26X11','A26X12','A26X13',           
     $'A26X14','A26X15','A26X16','A26X17','A26X18','A26X19','A26X20',           
     $'A26X21','A26X22','A26X23','A26X24','A26X25','A26X26','A26X27',           
     $'A26X28','A26X29','A26X30','A26X31','A26X32','A26X33','A26X34',           
     $'A26X35','A26X36','A26X37','A26X38','A26X39','A26X40','A26X41',           
     $'A26X42','A26X43','A26X44','A26X45','A26X46','A26X47','A26X48',           
     $'A26X49','A26X50','A26X51','A26X52','A26X53','A26X54','A26X55',           
     $'A26X56','A26X57','A26X58','A26X59','A26X60','A26X61','A26X62'/           
C  concentration variable names                                                 
      DATA PCONC/'CFSP01','CFSP02','CFSP03','CFSP04','CFSP05','CFSP06',         
     $'CFSP07','CFSP08','CFSP09','CFSP10','CFSP11','CFSP12','CFSP13',           
     $'CFSP14','CFSP15','CASS01','CASS02','CASS03','CASS04','CASS05',           
     $'CASS06','CASS07','CASS08','CASS09','CASS10','CASS11','CASS12',           
     $'CASS13','CASS14','CASS15','C20X01','C20X02','C20X03','C20X04',           
     $'C20X05','C20X06','C20X07','C20X08','C20X09','C20X10','C20X11',           
     $'C20X12','C20X13','C20X14','C20X15','C20Y01','C20Y02','C20Y03',           
     $'C20Y04','C20Y05','C20Y06','C20Y07','C20Y08','C20Y09','C20Y10',           
     $'C20Y11','C20Y12','C20Y13','C20Y14','C20Y15',                             
     $'C26X01','C26X02','C26X03','C26X04','C26X05','C26X06','C26X07',           
     $'C26X08','C26X09','C26X10',                                               
     $'C26X11','C26X12','C26X13','C26X14', 'C26X15','C26X16','C26X17',          
     $'C26X18','C26X19','C26X20','C26X21','C26X22','C26X23','C26X24',           
     $'C26X25','C26X26','C26X27','C26X28','C26X29','C26X30','C26X31',           
     $'C26X32','C26X33','C26X34','C26X35','C26X36','C26X37','C26X38',           
     $'C26X39','C26X40','C26X41','C26X42','C26X43','C26X44','C26X45',           
     $'C26X46','C26X47','C26X48','C26X49','C26X50','C26X51','C26X52',           
     $'C26X53','C26X54','C26X55','C26X56','C26X57','C26X58','C26X59',           
     $'C26X60','C26X61','C26X62'/                                               
C default setup for PMS probes                                                  
      DATA DOF/2.74/                                                            
     $ BDIA/0.180/ TAU1/.0000063/ TAU2/.0000038/ TAU3/0.0/                      
     $ WIRET/403.16/ FNUSS/0.22/ REXP/0.62/                                     
C                                                                               
C  loran-c block variable names                                                 
      DATA LORNAM/'CLAT','CLON','CCEP','CGS'/                                   
C  housekeeping variable names                                                  
C                                                                               
      DATA HSKNAM/    'V10     ','V10R    ','TADS    ','TV10    ',              
     $     'FLOADS  ','FZV     ','FZVR    ','VDREF   ','CKEVP   ',              
     $     'EV1     ','EV2     ','EV3     ','EV4     ',                         
     $     'HR      ','MIN     ','SEC     ','PTIME   ','TPTIME  ',              
     $     'TMLAG   ','TSEC    '/                                               
C time segments, input and output tapes                                         
      DATA INMSEG/1/ONMSEG/1/                                                   
      DATA ITMFLG/MAXSEG*0/OTMFLG/MAXSEG*0/                                     
      DATA NSXIN/1800/NSXOUT/30000/                                             
C  Project, Genpro, and flight setup information                                
      DATA NMUSER/'NOONE'/NMPROJ/'UNKNOWN'/TAPNO/20 * '0'/                      
     $ IPROJ/'0'/IFLGHT/'0'/PRTITL/'NO TITLE YET'/OUTPUT/'GXXXXX'/              
     $ GAP/'0'/TAPE1/'0'/TURBRT/'HRT'/                                          
     $ IDATEF/3 * 0/ITIMEF/6 * 0/INILAT/0/INILON/0/                             
     $ IARCFT/'xxx'/IVTITL/'MOUNTAIN DAYLIGHT TIME'/QCREF/'QCx','QCx'/          
     $ OUTCYC/.FALSE./PRTCYC/.TRUE./PLTCYC/.TRUE./                              
     $ STACYC/.TRUE./PL2CYC/.FALSE./                                            
     $ PR2D1/.TRUE./PR2MS/.FALSE./PR2IO/.FALSE./PL2IO/.FALSE./                  
     $ PL2D1/.TRUE./PL2MS/.FALSE./PL2MP/.FALSE./OUT2MS/.FALSE./                 
     $ INPUT/.TRUE./                                                            
      END                                                                       
      SUBROUTINE ERROR  (RCODE,ERRNO,ICHR)                                      
      INTEGER INTERR                                                            
      COMMON/IOERR/INTERR                                                       
      INTEGER   RCODE,ERRNO                                                     
      CHARACTER *1 ICHR                                                         
C  LET FORTRAN CHANGE VALUE TO 0                                                
      RCODE = 0                                                                 
C  IBM error code for non-integer input: 215                                    
      IF (ERRNO.EQ.215) THEN                                                    
       INTERR = 215                                                             
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
