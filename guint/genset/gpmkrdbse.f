                   SUBROUTINE RDBASE                                            
                                                                                
C  This subroutine reads the database and inserts the variable titles,          
C  variable units, and variable names into the arrays TITLES, UNITTS,        
C  and VARNAM respectively.                                                     
                                                                                
      include "gparc.h"
      include "gpstd.h"
                                                                                
      INTEGER J
                                                                                
      J = 1                                                                     
      NMST = 0                                                                  
   20   READ (15, 2025, END = 30) TITLE(j), UNITTS(j), 
     $  VARNAM(j),STAD(j)     
 2025   FORMAT (A40,1X,A6,3X,A8,1X,A3)                                          
        IF (TITLE(j) (1:1) .EQ. ' ') GOTO 20                                    
        J = J + 1                                                               
        if (j .gt. 1000) goto 30
      goto 20                                                                   
   30 VAREND = J -1                                                             
      RETURN                                                                    
      END                                                                       
