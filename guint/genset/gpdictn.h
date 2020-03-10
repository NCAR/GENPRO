C  Units dictionary:                                                            
C    MAXNAM --  maximum # of variables possible (size of arrays)                
C    NUMBV  --  number of variables actually available for use                  
C    VNAME  --  names of those variables                                        
C    VUNIT  --  parallel array of units for those variables                     
C    BULL9  --  list of all bulletin 9 variables                                
C    NMBUL9 --  number of bulletin 9 vars                                       
C    VRPOSS --  list of all possible vars (std and non-std) for this flt        
C    NMPOSS --  number of vars in VRPOSS                                        
C    PSPPTR --  pointer array for PLOT/PRINT/STATS names                        
C    PSPOUT --  number of PLOT/PRINT/STATS output selections                    
C    PL2PTR --  pointer array for complete flight PLOT names                    
C    PL2OUT --  number of complete flight PLOT output selections                
C                                                                               
      INTEGER MAXNAM , PSPPTR, PL2PTR                                           
      INTEGER PSPOUT, PL2OUT, NUMBV, BULL9, NMBUL9, NMPOSS                      
      CHARACTER*8 VNAME,VRPOSS                                                  
      CHARACTER*4 VUNIT                                                         
      PARAMETER (MAXNAM = 900)                                                  
      common/cdictn/numbv,pspptr(maxnam),pl2ptr(maxnam),pspout,
     1              pl2out,bull9(maxnam),nmbul9,nmposs
      COMMON/DICTN/ VNAME(MAXNAM),vrposs(maxnam),VUNIT(MAXNAM)
