C     EDTSUM -- create comprehensive flight entries file from                   
C               SUMMARY file. SUMMARY file must be completed before             
C               using this routine.                                             
C                                                                               
C  INPUT FILE -- Unit 77: SUMMARY <project number> <workdisk>                   
C  OUTPUT FILE -- Unit 88: COMPFLT <project number> <workdisk>                  
C                                                                               
      integer       lindex,start,stopt
      character *3  arg1
      CHARACTER *6  TAPNO,IDATEF,BEGIV,ENDIV,IDATE2,ITIME1,ITIME2               
      CHARACTER *5  FLT1,FLT2                                                   
      CHARACTER *35 DESCRP                               
      CHARACTER *80 PROJ,fulpth,arg2                             
      logical midnit

      midnit=.false.
      call GETARG(1,ARG1)
      call getarg(2,arg2)
      do 13, i = 1, 80
         fulpth(i:i) = ' '
 13   continue
      lindex = index(arg2,' ')
C---------adjust lindex to the end of the string arg2
      lindex = lindex -1
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/summary.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=77,file=fulpth,access='sequential',form='formatted')
      do 14, i = 1, 80
         fulpth(i:i) = ' '
 14   continue
      fulpth(1:lindex) = arg2(1:lindex)
      fulpth(lindex+1:lindex+9) = '/compflt.'
      fulpth(lindex+10:lindex+12) = arg1
      open (unit=88,file=fulpth,access='sequential',form='formatted')
C skip first record of project information                                      
      READ(77,'(A80)') PROJ   
C     WRITE(88,'('' '',A80)') PROJ                            
      WRITE(88,'(A80)') PROJ                            
C read and write out first flight record, save flight number                    
      NUMVOL=1                            
      READ (77,'(bz,A6,2X,A6,2X,A5,2X,I6,1X,I6,2X,A35)')
     $ TAPNO,IDATEF,FLT1,start,stopt,DESCRP           
C   provide for crossing over midnight during flight
      if (stopt .lt. start) then
	 stopt=stopt+240000
	 midnit = .true.
      endif
      write(begiv,'(i6)') start
      write(endiv,'(i6)') stopt
C     WRITE(88,'('' '',A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')        
      WRITE(88,'(A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')        
     $ TAPNO,IDATEF,FLT1,BEGIV,ENDIV,DESCRP               
C loop reading records                                                          
  502 READ (77,'(bz,A6,2X,A6,2X,A5,2X,i6,1X,i6,2X,A35)',END=1000)        
     $ TAPNO,IDATE2,FLT2,start,stopt,DESCRP                                   
      IF (FLT2(1:4).EQ.FLT1(1:4)) THEN                                          
C        same flight -- adjust times if previously crossed midnight
      if (midnit) then
	 stopt = stopt +240000
	 start = start +240000
      elseif (stopt .lt. start) then
	 stopt=stopt+240000
	 midnit=.true.
      endif
      write(itime1,'(i6)') start
      write(itime2,'(i6)') stopt
C more than one volume for this flight; write out the one just read             
       NUMVOL=NUMVOL + 1                                                        
C      WRITE(88,'('' '',A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)') 
       WRITE(88,'(A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)') 
     $ TAPNO,IDATEF,FLT2,ITIME1,ITIME2,DESCRP                                   
       ENDIV=ITIME2                                                             
       GOTO 502                                                                 
      ELSE                                                                      
C new flight number from last one; decide if last one had multiple tapes        
      midnit = .false.
C     IF (NUMVOL.GT.1) THEN                                                    
C            provide for crossing over midnite during flight
      if (stopt .lt. start) then
         stopt = stopt + 240000
         midnit=.true.
      endif
      write(itime1,'(i6)') start
      write(itime2,'(i6)') stopt
C  write out the comprehensive flight information for last flight               
C       WRITE(88,'('' COMP'',I2,2X,A6,2X,A4,3X,A6,1X,A6,2X,       
        WRITE(88,'(''COMP'',I2,2X,A6,2X,A4,3X,A6,1X,A6,2X,       
     $  ''COMPLETE'',1X,A4)')                                                   
     $  NUMVOL,IDATEF,FLT1(1:4),BEGIV,ENDIV,FLT1(1:4)                           
C write out the first tape volume for new flight                                
C       WRITE(88,'('' '',A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')             
        WRITE(88,'(A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')             
     $  TAPNO,IDATE2,FLT2,ITIME1,ITIME2,DESCRP                                  
C      ELSE                                                                     
C write out a 'COMPLETE' descriptor for last flight                             
C       WRITE(88,'('' COMP 1'',2X,A6,2X,A5,2X,A6,1X,A6,2X,''COMPLETE'',
C    $  A4)')                                                                   
C    $  IDATEF,FLT1,BEGIV,ENDIV,FLT1                                            
C write out the 'last' tape volume for this flight                              
C       WRITE(88,'('' '',A6,2X,A6,2X,A5,2X,A6,1X,A6,2X,A35)')            
C    $  TAPNO,IDATE2,FLT2,ITIME1,ITIME2,DESCRP                                  
C      ENDIF                                                                    
C in either case, ensure pertinent vars are reset                               
       NUMVOL=1                                                                 
       FLT1=FLT2                                                                
       IDATEF=IDATE2                                                            
       BEGIV=ITIME1                                                             
       ENDIV=ITIME2                                                             
      ENDIF                                                                     
      GOTO 502                                                                  
 1000 CONTINUE                                                                  
C  write out the comprehensive flight information for last flight in file       
C     WRITE(88,'('' COMP'',I2,2X,A6,2X,A4,3X,A6,1X,A6,2X,''COMPLETE'',         
      WRITE(88,'(''COMP'',I2,2X,A6,2X,A4,3X,A6,1X,A6,2X,''COMPLETE'',         
     $1X,A4)')                                                                  
     $NUMVOL,IDATEF,FLT1(1:4),BEGIV,ENDIV,FLT1(1:4)                             
      END                                                                       
