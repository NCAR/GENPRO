C project info                                                                  
      COMMON/PROJ/NUMUSR,PRNUM,INILAT,EXAMIN,INILON,USERS,NMSCI,NMACCT          
C flag for examine mode                                                         
      LOGICAL EXAMIN                                                            
C initial latitude, longitude                                                   
      CHARACTER*9 INILAT,INILON                                                 
C user names and their account numbers                                          
      PARAMETER (MAXUSR=30)                                                     
      CHARACTER*16 USERS(MAXUSR)                                                
      CHARACTER*4 NMACCT(MAXUSR),NMSCI                                          
C number of users actually listed in GPSCIENC NUMBERS file                      
      INTEGER NUMUSR                                                            
C flight info                                                                   
      PARAMETER (MAXREC=900)                                                    
      CHARACTER *75 PRJREC, FLTREC(MAXREC)                                      
      COMMON/FLT/GOTPRO,NMREC,FLTREC,QCREF,IFLGHT,TAPNO,BEGIV,ENDIV,            
     $ IDATEF,DESCRP,X,PRJREC                                                   
      CHARACTER*1 X                                                             
      CHARACTER*3 QCREF(2),PRNUM                                                
      CHARACTER*5 IFLGHT                                                        
      CHARACTER*6 TAPNO,BEGIV,ENDIV,IDATEF                                      
      CHARACTER*35 DESCRP                                                       
C flag for project setup done yet or not                                        
      LOGICAL GOTPRO                                                            
C number of flight records read in                                              
      INTEGER NMREC                                                             
