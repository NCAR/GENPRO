      SUBROUTINE INIPRO                                                         
C   do initial project setup                                                    
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpio.h"                                                          
      CHARACTER *8 DME(3)                                                       
      DATA DME/'FREQ','FLAG','RANG'/                                            
C     CALL system("clear")                                                      
C  attempt to open compflt flight log file                                      
      CALL OPNSUM                                                               
      READ(99,'(A8,3X,A3,3X,A4,1X,2(F9.4,1X),2(A3,1X),3(A6,2x))')  
     $ NMUSER,TURBRT,NMSCI,INILAT,INILON,QCREF,TTX,PSX,DPX                      
      NMACCT=NMSCI // ACCTNO // IARCFT                                          
  528 WRITE(LUTO,300)                                                           
  300 FORMAT (' Comment, <r> if none: ')                                        
      NOTES(15:80)=' '                                                          
      READ(LUTI,'(A65)',END=400) NOTES(16:80)                                   
  400 continue                                                              
      CALL TTIME(NOTES(1:14))                                                   
C  pointers to default derived vars                                             
      DO 79 J=1,NMODE3                                                          
  79   MODE3(J)=J                                                               
C                                                                               
C                                                                               
C  pointers to sampled vars                                                     
      DO  80 I=1,NFULL                                                          
      IF(SDINAM(I).EQ.'        ')GO TO 80                                       
      NMODE2=NMODE2+1                                                           
      MODE2(NMODE2)=I                                                           
   80 CONTINUE                                                                  
C  check blocks from flight for DME, PMS, LORAN-C, and HOUSEKEEPING             
      DO 234 J=1,8                                                              
C  If PMS block is present, pick up raw PMS data                                
C   Note: this code does not work for ARIS recordings, only ADS                 
       IF(BLKNAM(J).EQ.'PMS1') THEN                                             
C       CALL CLSIZ                                                              
        DO 52 K=1,NUMPMS                                                        
        DO 51 L=1,5                                                             
        IF(PNAM(L).EQ.PNAME(K))THEN                                             
         NPROB(L)=L                                                             
C index PNAM order of K'th probe, as per NPROB, into IPTAB                      
         IPTAB(K)=L                                                             
C add two probe vars: concentrations and accumulations for this probe           
         NPVARS=NPVARS+2                                                        
         PRNAME(NPVARS-1)=PNAM(L)//'A'                                          
         PRNAME(NPVARS)=PNAM(L)//'C'                                            
C add extra probe vars for this probe                                           
         DO 50 M=XINDX(L),XINDX(L+1)-1                                          
          NDERIV=NDERIV+1                                                       
          NMODE3=NMODE3+1                                                       
          MODE3(NMODE3)=NDERIV                                                  
          DERIV(NDERIV)=XNAMES(M)                                               
  50     CONTINUE                                                               
C set pointer arrays for given probe                                            
         DO 150 N=PMSTAB(L),PMSTAB(L)+PMSTAB(L+5)-1                             
          NACC(L)=NACC(L)+1                                                     
 150      PTRACC(NACC(L))=N                                                     
         DO 151 N=PMSTAB(L+10),PMSTAB(L+10)+PMSTAB(L+15)-1                      
          NCON(L)=NCON(L)+1                                                     
 151      PTRCON(NCON(L))=N                                                     
         GOTO 52                                                                
        ENDIF                                                                   
  51    CONTINUE                                                                
  52    CONTINUE                                                                
       ENDIF                                                                    
       IF (BLKNAM(J).EQ.'DME') THEN                                             
C add DME vars to sampled list                                                  
        DO 442 KK=1,3                                                           
         MODE2(NMODE2+KK)=NMODE2+KK                                             
         SDINAM(NMODE2+KK)=DME(KK)                                              
  442   CONTINUE                                                                
        NMODE2=NMODE2+3                                                         
       ENDIF                                                                    
C  check for Loran-C vars                                                       
       IF (BLKNAM(J).EQ.'LRNC') THEN                                            
        NLORN=LORVAR                                                            
        DO 235 K=1,NLORN                                                        
  235    LORAN(K)=K                                                             
C add LORAN vars to sampled list                                                
        DO 443 KK=1,NLORN                                                       
         MODE2(NMODE2+KK)=NMODE2+KK                                             
         SDINAM(NMODE2+KK)=LORNAM(LORAN(KK))                                    
  443   CONTINUE                                                                
        NMODE2=NMODE2+NLORN                                                     
       ENDIF                                                                    
C get housekeeping vars                                                         
       IF (BLKNAM(J).EQ.'HSKP') THEN                                            
        NHSKP=HSKVAR                                                            
C add HSKP vars to sampled list                                                 
        DO 444 KK=1,NHSKP                                                       
         HSKP(KK)=KK                                                            
         MODE2(NMODE2+KK)=NMODE2+KK                                             
         SDINAM(NMODE2+KK)=HSKNAM(HSKP(KK))                                     
  444   CONTINUE                                                                
        NMODE2=NMODE2+NHSKP                                                     
       ENDIF                                                                    
  234 CONTINUE                                                                  
C  set optional section of project title to default: project name               
      USRTIT=NMPROJ                                                             
C     CALL system("clear")                                                      
      WRITE(LUTO,'('' Processing....'')')                                       
C  Set up calculated and derived parameters whose inputs are present            
      CALL DERIVE                                                               
C  Get all bulletin 9 variables (fill BULL9, VNAME and VUNIT)                   
      CALL GETDIC                                                               
C arrange via bulletin 9 order into VRPOSS array                                
      CALL GETORD                                                               
C get units for VRPOSS names (fill UNITS)                                       
      CALL FNDUN                                                                
C   get attack and sideslip sensors                                             
      CALL PARM                                                                 
C initialize pointer arrays to select standard (bulletin 9) vars, PMS           
C keywords, and all sampled vars                                                
      PSPOUT=0                                                                  
      PL2OUT=0                                                                  
      DO 401 J=1,NMPOSS                                                         
       INDX1=0                                                                  
       INDX2=0                                                                  
       INDX3=0                                                                  
       CALL SERCH2(VRPOSS(J),VNAME,BULL9,NMBUL9,INDX1)                          
       CALL SERCH(VRPOSS(J),PRNAME,NPVARS,INDX2)                                
       CALL SERCH2(VRPOSS(J),SDINAM,MODE2,NMODE2,INDX3)                         
       IF(INDX1.NE.0.OR.INDX2.NE.0.OR.INDX3.NE.0) THEN                          
        PSPOUT=PSPOUT+1                                                         
        PSPPTR(PSPOUT)=J                                                        
        PL2OUT=PL2OUT+1                                                         
        PL2PTR(PL2OUT)=J                                                        
        GOTO 401                                                                
       ENDIF                                                                    
 401  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE OPNSUM                                                         
      INCLUDE "gpio.h"                                                          
      INCLUDE "gpifile.h"                                                       
      do 13, i=1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/compflt.'
      fulpth(lindex+10:lindex+12) = iproj
      open (unit=99,file=fulpth,access='sequential',err=18711,
     $iostat=ierr,status='old',form='formatted')
C     OPEN(99,FILE='compflt',IOSTAT=IERR,STATUS='OLD',ERR=18711)                
18711 IF(IERR.NE.0)  THEN                                                       
       WRITE(6,'('' You must create a compflt file before using this'',/        
     $ '' program...see Log Flight Tapes option in main menu...hit <r>''        
     $ /'' to continue ...'')')                                                 
       READ(LUTI,'(A)',END=18712) I                                             
18712  continue                                                             
       STOP                                                                     
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
