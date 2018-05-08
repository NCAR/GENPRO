c  @(#)newdap.model	1.1   9/28/92
      subroutine newdap(icode,idate,itime,buffer,outbuf,nvars,vnames,
     $   newvars,newnames)
c....... this is a model for the newdap subroutine.  Expect these calls
c..      from the main program to this subroutine:
c.. INITIALIZATION:  icode < 0.  The user must supply a list of variable
c*	names needed for calculations in "vnames" and the number of such
c*	variables in "nvars".  The variables specified here have no effect
c*	on the eventual output (variables for output are selected through the
c*	interactive window display); these only determine those variables
c*	needed for calculations.  Also, the names of new variables to be added
c*      should be specified in "newnames" and the number of such variables
c*      in "newvars".  (The number of either or both can be 0--1000.)  All
c*	variables should be specified using normal DAP conventions, with
c*	6-character names (optionally terminated by NULL in the 7th or 8th
c*	character) that for input variables must correspond exactly
c*	(including trailing blanks if needed) to DAP variable names in the
c*	input file.  The variables idate, itime, buffer, and outbuf are not 
c*	used for the initialization call, and will be meaningless.
c*  DATA: icode == 0
c*      For each such call, the date and time of the data sample are
c*      provided and the variables requested are supplied in "buffer"
c*      in the order requested in the initialization call.  The user
c*	must provide values for the new variables in "outbuf", in the
c*	order specified in newnames in the initialization call.  Both
c*	buffer and outbuf are floating-point arrays.  The
c*	number of input and output variables and their names are provided
c*	each time as specified in the initialization call, and cannot
c*	be changed during data calls.
c*  TERMINATION: icode > 0.  Normally this call is not needed, and the
c*	user should return without taking any action.  It is provided for
c*	generality, in case summaries of calls or final output listings
c*	and other termination actions are desired.  No action is required
c*	at this time for normal program operation.  The variables idate 
c*	and itime will have the values corresponding to the last valid
c*	data record (in input, not output), and the variable lists and
c*  	numbers of variables still will be valid, but buffer and outbuf
c*	will not necessarily contain valid data and should not be used.
c*      
	character*12 vnames(*), newnames(*)
        real buffer(1000),outbuf(1000)
c       
c....... normal data call.  Calculate desired new variables.  Two are
c..      output here: difference between two temperatures, and a duplicate
c..      of one of the temperature measurements.  The reason duplicates are
c..      often needed is that filtering and time-shifting of the duplicate
c..      then does not affect the original measurement, which can also be
c..      present in the output data file.
c
      if(icode.eq.0) then
	   outbuf(1) = buffer(1) - buffer(2)
	   outbuf(2) = buffer(1)
c
c....... initialization call.  Define variables needed in calculations, and
c..      new variables
      else if(icode.lt.0) then
           vnames(1)="ATRF  "
           vnames(2)="ATB   "
  	   nvars = 2
	   newvars = 2
	   newnames(1)="TDIFF "
	   newnames(2)="ATRFF "
           write(6,'(" THIS IS THE MODEL NEWDAP ROUTINE")')
c
c....... termination call.  Do nothing, usually:
      else
      endif
      return
      end
