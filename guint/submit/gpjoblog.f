      CHARACTER *5 FLTNO                                                        
      CHARACTER *2 CRAY                                                         
      CHARACTER *8 SEQNUM,prnam                                                
      CHARACTER *14 DATE                                                        
      CHARACTER *40 COMMNT                                                      
      character*80 fulpth,arg2

      call getarg(1,prnam)
      call getarg(2,arg2)
      call getarg(3,fltno)
      call getarg(4,cray)
      call getarg(5,seqnum)
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
C-------------start to open 16
      do 48, i = 1, 80
         fulpth(i:i) = ' '
 48   continue
C---------adjust lindex to the end of the string arg2
      lindex = index(arg2,' ') - 1
      fulpth(1:lindex) = arg2(1:lindex)
      lldex =index(prnam,' ') 
      fulpth(lindex+1:lindex+1) = '/'
      fulpth(lindex+2:lindex+lldex) = prnam(1:lldex)
      fulpth(lindex+1+lldex:lindex+1+lldex) = '.'
      fulpth(lindex+2+lldex:lindex+7+lldex) = 'joblog'
      open (unit=16,file=fulpth,access='sequential')
      COMMNT=' NO COMMENT'                                                      
      WRITE(6,'('' 40 CHAR. COMMENT : '')')                                     
      READ(5,'(A40)',END=10) COMMNT                                             
   10 continue                                                                  
      CALL EXTEND(16)                                                           
      CALL TTIME(DATE)                                                          
      WRITE(16,'(1x,A5,1X,A2,1X,A8,1X,A14,1X,A40,/,''   -- Result: '')')   
     $FLTNO,CRAY,SEQNUM,DATE,COMMNT                                        
      END                                                                       
