C-----------COMMON BLOCK VARIABLES                                              
      CHARACTER*6 UNITTS                                                   
      CHARACTER*8 VARNAM,NAME            
      CHARACTER*40 TITLE                                                        
      INTEGER NUMVAR, VAREND              
      CHARACTER*1 KFLAG                                                         

      COMMON/ARAYS/ TITLE(1000)
      common/arrys/ UNITTS(1000)
      common/arrys2/ VARNAM(1000), NAME(600)
      common/kflg1/kflag    
      COMMON/COUNTS/ NUMVAR, VAREND
                                                                                

C     TITLE, UNITS and VARNAM contain info. from gpvar.dba
C     VAREND is the number of valid entries in those three arrays
C     NAME is the list of selected variables
C     NUMVAR is a reference into NAME
C     KFLAG does something I'm sure.
