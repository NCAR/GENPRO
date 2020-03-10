C                                                                               
      CHARACTER*8 DERIV,PMS1D   
      INTEGER  PMSTAB,PTRACC,PTRCON,NCON,NACC                                   
      INTEGER  IPTAB,PMSSIZ                                                     
      INTEGER TABSIZ,NACCUM,NCONC,DIMD13,DIMPMS,DIMSDI                          

      PARAMETER (TABSIZ=20,NACCUM=122,NCONC=122,NMPROB=5)                       
      PARAMETER (DIMSDI=200,DIMD13=200,DIMPMS=NACCUM + NCONC)                   
      COMMON/PARAM/NMODE1,NMODE2,NMODE3,MODE1(DIMD13),MODE2(DIMSDI),            
     $  MODE3(DIMD13),NDERIV,NPMS1D,                
     $  NACC(NMPROB),NCON(NMPROB),PTRACC(NACCUM),PTRCON(NCONC),                 
     $  PMSTAB(TABSIZ),IPTAB(NMPROB),PMSSIZ,NPROB(NMPROB)                       
      common/rparam/deriv(dimd13),pms1d(dimpms)
C                                                                               
C                                                                               
C TABSIZ -- size of lookup table PMSTAB                                         
C NACCUM -- number of possible accumulation variables                           
C NCONC  -- number of possible concentration variables                          
C NMODEx -- number of each mode (1: voltage; 2: sampled; 3: derived)            
C MODEx  -- array of pointers (1: SDINAM; 2: SDINAM; 3: DERIV)                  
C NDERIV -- number of derived variables in use                                  
C DERIV  -- names of derived variables                                          
C NPMS1D -- total number of PMS variables in use                                
C PMS1D  -- individual names of PMS names in use                                
C NACC   -- number of accumulation vars in use for one of NMPROB probes         
C NCON   -- number of concentration vars in use for one of NMPROB probes        
C PTRACC -- pointer array into array of accumulation names                      
C PTRCON -- pointer array into array of concentration names                     
C PMSTAB -- lookup table of indices for start word, length for                  
C           acc'n/conc'n names for each of the PMS probes                       
C IPTAB  -- table of numbers of indices into PNAM, available probes             
C PMSSIZ -- total PMS variables selected, via keyword or individual             
C NPROB  -- flags indicating presence of PMS probe(s)                           
C                                                                               
C                                                                               
C  Variables that make up the ADS Header                                        
C                                                                               
      INTEGER MAXBLK                                                            
      PARAMETER (MAXBLK=8)                                                      
C                                                                               
C   DIMSDI  -- number of words in SDINAM,C1,C2,C3,ISDI,MODE2,ITYPE              
C   DIMD13  -- number of words in DERIV,MODE1,MODE3                             
C   DIMPMS  -- number of words in PMS1D (must equal NACCUM + NCONC)             
C   NMPROB  -- maximum number of PMS probes                                     
C   MAXBLK  -- maximum number of data blocks in ADS header                      
                                                                                
       common/cadshd/numpms,nfull
       COMMON / ADSHD  / BLKNAM(MAXBLK),SDINAM(DIMSDI),            
     $ ITYPE(DIMSDI),PNAM(NMPROB),PNAME(NMPROB)  
       common/adshd2/C1(DIMSDI),C2(DIMSDI),C3(DIMSDI)
C                                                                               
       CHARACTER*13 C1,C2,C3                                                    
       CHARACTER*8 SDINAM                                                       
       CHARACTER*4 BLKNAM,PNAME,PNAM                                            
       CHARACTER*2 ITYPE                                                        
C                                                                               
C  BLKNAM --  names of ADS header data blocks                                   
C  SDINAM --  names of sampled digital parameters                               
C  C1,C2,C3 -- coefficients of sampled parameters                               
C  ITYPE --  type, either digital ('DI') or analog ('AN')                       
C  PNAM  --  names of possible PMS probes, in standard order                    
C  PNAME --  names of  PMS probes in use in current flight                      
C  NUMPMS -- number of PMS probes in use in current flight                      
C  NFULL --  number of sampled parameters in current flight                     
C******************************************************************             
C         Housekeeping Block                                                    
C                                                                               
      INTEGER HSKVAR                                                            
      PARAMETER (HSKVAR=20)                                                     
      INTEGER  HSKP(hskvar),nhskp                                        
      CHARACTER*8 HSKNAM(HSKVAR)                                  
      common/hskp2/HSKNAM
      COMMON/cHSKP/NHSKP,HSKP
C                                                                               
C   HSKP -- pointers to housekeeeping vars selected by user                     
C   NHSKP -- # of hk vars chosen                                                
C   HSKNAM -- all possible hk var names                                         
C   HSKVAR -- # of possible hk var names                                        
C****************************************************************
C       PMS1D processing options and calibration values                         
C       and King Liquid Water calibration values                                
C                                                                               
      COMMON/PMSOP/WIRET, FNUSS, REXP, DOF,BDIA,TAU1,TAU2,TAU3                  
C                                                                               
C       DOF   = Depth of field for FSSP                                         
C       BDIA  = Beam diameter for FSSP                                          
C       TAU1  = Multiplier for STROBES for FSSP                                 
C       TAU2  = Multiplier for RESETS for FSSP                                  
C       TAU3  = Reset # 3                                                       
C       WIRET = Wire Temperature                                                
C       FNUSS = Multiplier for Reynolds number                                  
C       REXP  = Exponent of Reynolds number                                     
C                                                                               
C                                                                               
      INTEGER    NCELLF,NCELLA,NCELLX,NCELLY,NCELL6                             
      PARAMETER (NCELLF=15,NCELLA=15,NCELLX=15,NCELLY=15,NCELL6=62)             
      COMMON/KNOL/PMSFAX(5),PMSNMF(NCELLF),PMSNMA(NCELLA),                      
     $ PMSNMX(NCELLX),PMSNMY(NCELLY),                                           
     1 PMSNM6(NCELL6),PCONC(NCONC),DUMMY(1),PRNAME(10)
      common/cknol/npvars
      CHARACTER*8 PMSFAX,PMSNMF,PMSNMA,PMSNMX,PMSNMY,PMSNM6,PCONC,
     $ DUMMY,PRNAME  
C                                                                               
C    NCELLx -- number of cells in each probe                                    
C    PMSFAX -- special FSSP probe extra parameter names                         
C    PMSNMx -- contains the names of the PMS raw counts                         
C    PCONC  -- contains the names of the PMS concentrations                     
C    NCONC  -- how many concentration cells total                               
C    PRNAME -- array of keywords for counts/corr'd counts for PMS probes        
C    NPVARS -- # of valid words in PRNAME                                       
C                                                                               
C                                                                               
C***********************************************************************        
C                                                                               
C         Loran-C Block                                                         
C                                                                               
      INTEGER LORVAR                                                            
      PARAMETER (LORVAR=4)                                                      
      COMMON/cLORN/LORAN(LORVAR),NLORN                        
      common/lorn/lornam(lorvar)
      CHARACTER*8 LORNAM                                                        
C                                                                               
C   LORAN - pointers to Loran-C vars selected by user                           
C   NLORN -- # of loran-c vars chosen                                           
C   LORNAM -- all possible loran-c var names                                    
C   LORVAR -- # of currently used loran-c var names                             
C                                                                               
C       Keywords                                                                
C                                                                               
C   KEYWORD ARRAY -- KEYWRD                                                     
C   NUMBER OF VALID WORDS USED IN KEYWORD ARRAY -- NUMKEY                       
C   MAXIMUM SIZE OF KEYWORD ARRAY -- KEYSIZ                                     
C                                                                               
C                                                                               
      INTEGER  NUMKEY, KEYSIZ                                                   
      PARAMETER (KEYSIZ = 20)                                                   
      CHARACTER *8 KEYWRD(KEYSIZ)                                               
      COMMON / KEYS / KEYWRD
      common/ckeys/NUMKEY                                             
C                                                                               
C  Additional Probe Parameters ---                                              
C                                                                               
C    XINDX -- index into XNAMES for start of extra names of n'th probe          
C    XNAMES -- additional probe parameters names                                
C    MXXTRA -- maximum # of extra names for any one probe                       
C    EXTRAS -- total number of extra names, all probes                          
C                                                                               
      INTEGER MXXTRA, EXTRAS                                                    
      PARAMETER (MXXTRA = 6, EXTRAS = 28)                                       
      INTEGER  XINDX(MXXTRA)                                                    
      CHARACTER *8 XNAMES(EXTRAS)                                               
      COMMON / PMSX / XNAMES
      common/cpmsx/XINDX                                              
C******************************************************************             
