      SUBROUTINE TTIME(DATE)                                                    
C                                                                               
C  Given a CHARACTER*14 (minimum) string DATE, return the date and time         
C  in 'mm/dd/yy hh:mm' format                                                   
C                                                                               
C                                                                               
      CHARACTER *4  temp
      CHARACTER *(*) DATE                                                       
      integer*4 arry(3),arry2(3)
      call idate(arry)
      call itime(arry2)
      write(temp,'(i4)') arry(3)
        WRITE(DATE,'(I2,''/'',i2,''/'',a2,1x,i2,'':'',i2)') arry(2),
     $  arry(1),temp(3:4),arry2(1),arry2(2)                                     
      END                                                                       
