      SUBROUTINE FILE(IOPT)                                                     
C                                                                               
C  Save or restore project specific information to SETSAV <projno>              
C  or save/restore flight specific info to FLTSAV <projno>, in each             
C  case to the specified <workdisk>  (IBM 4381 implementation)                  
C                                                                               
C    SET GLOBAL IERR = 1 if no project setups saved yet                         
C                      2 if EOF read on SETSAV file                             
C                      else leave IERR at 0                                     
C                                                                               
      CHARACTER *8 IDUMMY,temp(600),temp2(600)
      integer  iopt,itemp,itemp2
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
      INCLUDE "gpio.h"                                                          
C     INCLUDE "gpusrvar.h"                                                      
C                                                                               
C                                                                               
C  file access routines                                                         
C    IOPT -- access option as follows:                                          
C         0: restore SETSAV file, old format                                    
C         1: restore SETSAV file, current format                                
C         3: save SETSAV file, current format                                   
C                                                                               
      IERR=0                                                                    
C-------------start to open 13
      do 19, i = 1, 80
         fulpth(i:i) = ' '
 19   continue
      lindex= index(arg2,' ') -1
C---------adjust lindex to the end of the string arg2
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+8) = '/setsav.'
      fulpth(lindex+9:lindex+11) =iproj
C-------------------------------------------------------------
      IF ((IOPT .EQ. 0) .OR. (IOPT .EQ. 1)) THEN    
       open (unit=13,file=fulpth,access='sequential',err=18711,
     $ iostat=ierr,status='old',form='unformatted')
18711  IF(IERR.NE.0)  THEN                                                      
       write(6,'('' iopt='',i2)') iopt
        WRITE(6,'('' No project setups saved yet. '')')                         
        IERR=1                                                                  
        RETURN                                                                  
       ENDIF                                                                    
      ELSE                                                                      
       open (unit=13,file=fulpth,access='sequential',err=18711,
     $ iostat=ierr,form='unformatted')
C      OPEN(13,FILE='SETSAV',IOSTAT=IERR,FORM='UNFORMATTED',                    
C    $ ERR=18711)                                                               
       REWIND(13)                                                               
      ENDIF                                                                     
      IF (IOPT.EQ.0) THEN                                                       
C restore project setup, OLD file format                                        
C IMPORTANT NOTE: update these READ statements with the 'old' format list       
C from option 1 before changing the list under that option in order to          
C maintain this conversion capability                                           
       READ (13,IOSTAT=IERR,END=2000)                                           
     $ IARCFT,NMSCI,NMUSER,NMPROJ,IPROJ,TURBRT,NOTES,                           
C    $ USRVEC,NMUSRV,                                             
     $ NUMKEY,KEYWRD,                                             
     $ BLKNAM,PNAME,NUMPMS,NPROB,IPTAB,NPMS1D,PMS1D,                            
     $ NPVARS,PRNAME,NACC,NCON,PTRACC,PTRCON,                                   
     $ NHSKP,HSKP,NLORN,LORAN,NMODE2,MODE2,NDERIV,DERIV,NMODE3,MODE3            
       READ (13,IOSTAT=IERR,END=2000)                                           
     $ VNAME,VUNIT,VRPOSS,NMPOSS,BULL9,NMBUL9,                                  
     $ PSPPTR,PSPOUT,PL2PTR,PL2OUT,UNITS,                                       
     $ TTX,DPX,QCX,PSX,AVANE,BVANE,QCREF,                                       
     $ DOF,BDIA,TAU1,TAU2,TAU3,                                                 
     $ WIRET,FNUSS,REXP,INILAT,INILON,                                          
     $ OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC,                                      
     $ PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS                         
       CALL TTIME(NOTES(1:14))                                                  
       NMACCT=NMSCI//ACCTNO//IARCFT                                             
      ELSEIF (IOPT.EQ.1) THEN                                                   
C restore project setup, current format                                         
C IMPORTANT NOTE: copy these READ statements to the 'old' format list           
C under option 0 before changing this list in order to                          
C maintain conversion capability                                                
       READ (13,IOSTAT=IERR,END=2000)                                           
     $ IARCFT,NMSCI,NMUSER,NMPROJ,IPROJ,TURBRT,NOTES
C    $ USRVEC,NMUSRV                                             
       READ (13,IOSTAT=IERR,END=2000)                                           
     $ itemp,(temp(j),j=1,itemp),                    
     $ itemp2,(temp2(j),j=1,itemp2),                    
     $ TTX,DPX,QCX,PSX,AVANE,BVANE,QCREF,                                       
     $ DOF,BDIA,TAU1,TAU2,TAU3,                                                 
     $ WIRET,FNUSS,REXP,INILAT,INILON,                                          
     $ OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC,                                      
     $ PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS                         
       CALL TTIME(NOTES(1:14))                                                  
       NMACCT=NMSCI//ACCTNO//IARCFT                                             
C                                                                               
       ISIZE=0                                                                  
       DO 150 K=1,nmposs
          do 160 jj=1,itemp
             if (vrposs(k) .eq. temp(jj)) pspptr(jj)=k
  160     continue
          do 170 jj=1,itemp2
             if (vrposs(k) .eq. temp2(jj)) pl2ptr(jj)=k
  170     continue
  150  CONTINUE                                                                 
       pspout = itemp
       pl2out = itemp2
      ELSEIF (IOPT.EQ.3) THEN                                                   
C save project setup, current format                                            
C IMPORTANT NOTE: copy these WRITE statements to the 'old' format list          
C under option 0 before changing this list in order to                          
C maintain conversion capability                                                
       REWIND(13)                                                               
       WRITE(13)                                                                
     $ IARCFT,NMSCI,NMUSER,NMPROJ,IPROJ,TURBRT,NOTES
C    $ USRVEC,NMUSRV                                             
       WRITE(13)                                           
     $ pspout,(VRPOSS(PSPPTR(j)),j=1,pspout),                    
     $ pl2out,(VRPOSS(Pl2PTR(j)),j=1,pl2out),                    
     $ TTX,DPX,QCX,PSX,AVANE,BVANE,QCREF,                                       
     $ DOF,BDIA,TAU1,TAU2,TAU3,                                                 
     $ WIRET,FNUSS,REXP,INILAT,INILON,                                          
     $ OUTCYC,PRTCYC,PLTCYC,PL2CYC,STACYC,                                      
     $ PR2D1,PR2MS,PR2IO,PL2IO,PL2D1,PL2MS,PL2MP,OUT2MS                         
      ENDIF                                                                     
      CLOSE(13)                                                                 
      RETURN                                                                    
 2000 WRITE(LUTO,'(//'' Read EOF, project setup save file. Contact''            
     $,'' GUINT expert for help''//                                             
     $)')                                                                       
      CALL PAUSER(' For now, hit <r> to continue...')                           
      IERR=2                                                                    
      RETURN                                                                    
      END                                                                       
