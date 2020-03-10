      CHARACTER *80 RECORD                                                      
      CHARACTER *6 SIONUM                                                       
C
C open stdin to not use first character as print control.
      open (unit=5,form='formatted')
C
C  Skip first 3 lines                                                           
      DO 1 J=1,3                                                                
    1  READ(5,'(A80)') RECORD                                                   
C Get next SIO number                                                           
    5 READ(5,'(46X,A6)',END=100) SIONUM                                         
C Write the extract command to temporary EXEC file                              
      WRITE(7,'(''GETCHK '',A6)') SIONUM                                        
      GOTO 5                                                                    
C  end of file found                                                            
  100 WRITE(7,'(''QQUIT'')')                                                    
      END                                                                       
