       subroutine reform(icode,ihr,imin,isec,ims,idate,iac,iprj,iflt,
     $   ideltt, outbuf,newvars,newnames)
c
c
c....... This version of "reform.f" uses "coshead.c" to read
c	 GENPRO-I files from MONEX. coshead.c reads a sequence
c	 of files named "f001", "f002", etc, as produced by
c	 cossplit (which must be run on a Sun machine) and after
c	 conversion by "cosconvert -b".
c
c
C*		ihr, imin, isec, ims: 	data time
C*		idate: 			date data recording started
C*		iac:			aircraft ID (4 characters)
C*		iprj:			project number
C*		iflt:			flight number
C*		ideltt:			milliseconds between data records
C*		outbuf:			floating-pt buffer for output data
C*		newvars:		number of output variables
C*		newnames:		names of output variables
C*  Expect these calls to "reform":
C*  INITIALIZATION:  icode < 0.  The names of new variables, in the order
C*	desired in the output records, should be specified in "newnames" 
C*	and the number of such variables in "newvars".  All
C*	variables should be specified using normal DAP conventions, with
C*	character names (optionally terminated by NULL) with max size 11
C*	characters.  ID data can be provided via idate, iac, iprj, iflt,
C*	and ideltt; these can also be specified or overridden via window
C*	interaction.
C*  DATA: icode == 0
C*      For each such call, the time of the data sample should be
C*	returned, and values for the new variables must be returned
C*	in "outbuf" in the order specified in the initialization call.
C*  TERMINATION: icode > 0 ON RETURN.  The user routine should set this 
C*	at the end of the input data, for normal termination of the
C*	program.
C*
C*/
	character*4 iac
 	character*12 newnames(*)
	real outbuf(1000)
	data t/36000./
        data nsecnd/0/
	data alstsc/0/
	data iwrap/-1/
        save nsecnd,alstsc,iwrap

c
c....... initialization call here
c....... provide names to be assigned to each variable, number of
c..      such variables, date, aircraft identification, flight
c..      number, project number, and time between returned records
      if(icode .lt. 0) then
	call coshead(newnames, newvars, idate, iac, iflt, iprj, ideltt)
	write(6,'(" newvars=",i6)') newvars
c
c....... normal data call here.  Provide data values in order, and time:
      else
	nsecnd = nsecnd + 1
	call cosdata(outbuf)
	if(outbuf(1) .le. 0.) then
	    icode = 1
	    return
        endif
	alstsc = outbuf(1)
        time = sect(outbuf(1))
	if (iwrap.eq.-1) then
	    if (time.gt.120000.) then
		iwrap = 1
	    else
		iwrap = 0
	    endif
	endif
	ihr = time/10000.
	if (iwrap.eq.1) then
	    if(ihr.lt.8) then
		ihr = ihr + 24
	    endif
	endif
        imin = (time-ihr*10000.)/100.
        isec = amod(time, 100.)
        ims = 0
c       write(6,
c    $  '(" ",i2,":",i2,":",i2," for seconds=",f15.1," time=",f15.1)')
c    $   ihr,imin,isec,outbuf(1),time
c	write(6,'(" outbuf[2-10]:", 10f8.1)') outbuf(2),outbuf(3),outbuf(4),
c    $outbuf(5),outbuf(6),outbuf(7),outbuf(8),outbuf(9),outbuf(10) 
c....... force termination when done:
      endif
      return
      end
