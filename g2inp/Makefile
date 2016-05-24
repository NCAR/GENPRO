OBJS = $(OBJ)/g2inp.o $(OBJ)/file.o $(OBJ)/prmpt.o $(OBJ)/setop.o $(OBJ)/rdata.o $(OBJ)/tiinp.o $(OBJ)/captl.o $(OBJ)/newname.o 



$(EXEC)/g2inp: $(OBJS) $(XANLIB)
	ncargf77 $(CFLAGS) $(OBJS) ${LIBORDER} -o $@ 

$(XANLIB): FORCE
		-@cd ../$(LIBRARY_DIR); \
		${MAKE} ; \
		cd ../g2inp ;
FORCE:

$(OBJ)/g2inp.o: g2inp.c g2inp.com slus.prm
	$(CC) $(COFLAGS) -o $(OBJ)/g2inp.o g2inp.c
$(OBJ)/file.o: file.f slus.prm file.com
	$(F77) $(FOFLAGS) -o $(OBJ)/file.o file.f
$(OBJ)/prmpt.o: G2prmpt.f $(XANLIB) g2inp.com tmout.com slus.prm
	$(F77) $(FOFLAGS) -o $(OBJ)/prmpt.o G2prmpt.f
$(OBJ)/setop.o: setop.f $(XANLIB) g2inp.com slus.prm
	$(F77) $(FOFLAGS) -o $(OBJ)/setop.o setop.f
$(OBJ)/rdata.o: G2rdata.f $(XANLIB) g2inp.com slus.prm
	$(F77) $(FOFLAGS) -o $(OBJ)/rdata.o G2rdata.f
$(OBJ)/tiinp.o: tiinp.f $(XANLIB) slus.prm
	$(F77) $(FOFLAGS) -o $(OBJ)/tiinp.o tiinp.f
$(OBJ)/captl.o: captl.f $(XANLIB)
	$(F77) $(FOFLAGS) -o $(OBJ)/captl.o captl.f
$(OBJ)/newname.o: newname.c $(XANLIB)
	$(CC) $(COFLAGS) -o $(OBJ)/newname.o newname.c
