      PROGRAM TEST
      CHARACTER*80 STRING
      CHARACTER*8  DERVAR
      open (unit=34,file='gpgender.dba',access='sequential',
     $  status='old',form='formatted')
       REWIND (34)
       READ (34,'(A80)')
       READ (34,'(A80)')
       Line =2
   10  READ(34,'(A40,1X,I1,1X,A8)',END=99) STRING,NUMSOU,DERVAR
       Line = Line + 1
       WRITE (6, 10100) Line, numsou, dervar
10100 FORMAT ('  Line # ='I4', Var type ='I2', Name = >'A'<')
       GO TO 10
   99  CONTINUE
       STOP
       END
