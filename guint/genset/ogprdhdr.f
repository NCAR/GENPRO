      SUBROUTINE RDHDR                                                          
C                                                                               
C   read header info prepared by ADSRAW from HEADER <projno> <workdisk>         
C                                                                               
      INCLUDE "gppdata.h"                                                       
      INCLUDE "gpifile.h"                                                       
 1045 FORMAT(3(I2,1X),4X,A3)                                                    
 1080 FORMAT(3X,A4)                                                             
 1082 FORMAT(I6)                                                                
 1131 FORMAT(A4)                                                                
      REWIND(9)                                                                 
      READ (9,1045) IDATEF,IARCFT                                               
      NMACCT(5:12)=ACCTNO//IARCFT                                               
      DO 100 I=1,8                                                              
       READ(9,1080) BLKNAM(I)                                                   
  100 CONTINUE                                                                  
      DO 300 J=1,6                                                              
  300  READ(9,1082) NFULL                                                       
      IF (NFULL.GT.DIMSDI) NFULL=DIMSDI                                         
      READ(9,1082) NUMPMS                                                       
      DO 231 I=1,NUMPMS                                                         
         READ(9,1131) PNAME(I)                                                  
  231 CONTINUE                                                                  
      DO 1200 I=1,NFULL                                                         
       READ(9,1150)IRATE,SDINAM(I),C1(I),C2(I),C3(I),ITYPE(I)                   
       IF(SDINAM(I).EQ.'PHDG') PHDG=IRATE                                       
 1200 CONTINUE                                                                  
 1150 FORMAT(I3,4X,A8,3(2X,A13),4X,A2)                                          
      RETURN                                                                    
      END                                                                       
