C                                                                               
C  Examine the Scalerrs file created by Scaler and extract the smallest         
C  magnitude value for each variable listed, write back to Sift file            
C                                                                               
C                                                                               
      CHARACTER *8 VAR , CURVAR                                                 
      CHARACTER *9 VARS(100)                                                    
      REAL VALUE ,VALS(100) , BINS(6), TERMS(6)                                 
      CHARACTER *133 RECORD                                                     
      DIMENSION NPTR(100)                                                       
      character*80 arg2,fulpth
      character*9 prname
      character*4 flt

      call getarg(1,flt)
      call getarg(2,arg2)
      call getarg(3,prname)
C--------------------------------------start to open 19
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      lldex=index(prname,' ') - 1
      fulpth(lindex+1:lindex+1) = '/'
      fulpth(lindex+2:lindex+lldex+1) = prname(1:lldex)
      llndex = lindex +lldex + 1
      fulpth(llndex+1:llndex+5) = '.scle'
C-----------------------start to open 19
      open (unit=19,file=fulpth,access='sequential',recl=80,
     $          form='formatted')
C--------------------------------------start to open 20
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      lldex=index(flt,' ') - 1
      fulpth(lindex+1:lindex+10) = '/termvars.'
      fulpth(lindex+11:llndex+11+lldex) = flt(1:lldex)
C-----------------------start to open 20
      open (unit=20,file=fulpth,access='sequential',recl=80,
     $          form='formatted')
C                                                                               
C  define the bins for special terms                                            
C                                                                               
      DATA BINS/3000.,8000.,45000.,90000.,450000.,900000./                      
      DATA TERMS/5000.,10000.,50000.,100000.,500000.,1000000./                  
C     REWIND(8)                                                                 
      READ(19,'(A8,2X,F16.0)',END=100) VAR,VALUE                                
      NVAL=0                                                                    
    8 CURVAR=VAR                                                                
      VALMAX=VALUE                                                              
   10 READ(19,'(A8,2X,F16.0)',END=100) VAR,VALUE                                
      IF (CURVAR.EQ.VAR) THEN                                                   
       IF(VALUE.LT.VALMAX) VALMAX=VALUE                                         
       GOTO 10                                                                  
      ELSE                                                                      
       NVAL=NVAL+1                                                              
       VARS(NVAL)(1:8)=CURVAR                                                   
       VALS(NVAL)=-VALMAX                                                       
C      WRITE (20,'(''TERM= '',F16.0,'',%FOR,'',A8)',END=100)                    
C    $ FLOAT(((INT(-VALMAX))/10 + 1)  * 10),CURVAR                              
       GOTO 8                                                                   
      ENDIF                                                                     
  100 NVAL=NVAL+1                                                               
      VARS(NVAL)(1:8)=CURVAR                                                    
      VALS(NVAL)=-VALMAX                                                        
C 100 WRITE (20,'(''TERM= '',F16.0,'',%FOR,'',A8)',END=100)                     
C    $ FLOAT(((INT(-VALMAX))/10 + 1)  * 10),CURVAR                              
C                                                                               
C   extract those variables that fit into each of the 6 defined bins            
C                                                                               
      XMIN=999.                                                                 
      DO 200 J=1,6                                                              
       NPN=0                                                                    
       DO 150 K=1,NVAL                                                          
        IF(VALS(K).GT.XMIN.AND.VALS(K).LE.BINS(J)) THEN                         
         NPN=NPN+1                                                              
         NPTR(NPN)=K                                                            
        ENDIF                                                                   
  150  CONTINUE                                                                 
       IF (NPN.EQ.0) GOTO 200                                                   
       DO 160 I=1,NPN-1                                                         
  160   VARS(NPTR(I))(9:9)=','                                                  
       VARS(NPTR(NPN))(9:9)=' '                                                 
       WRITE(20,'(''TERM= '',F8.0,'',%FOR,'',(6(A9)))')TERMS(J),                
     $ (VARS(NPTR(N)),N=1,NPN)                                                  
       XMIN=BINS(J)                                                             
  200 CONTINUE                                                                  
      END                                                                       
