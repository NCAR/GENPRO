      subroutine rddata (inunit,t1,t2,nrecrds,nparms,index,array,nhdr,  &
     & iflag,m,n,nst)
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                      c
!        this subroutine is to be used in conjunction with subroutines c
!        rdhdr, and rdrec0.  this routine acts as a driver for the     c
!        others, accepting a start and finish time and a maximum numberc
!        of records to be returned.  these records consist only of     c
!        selected parameters, given by the values of the array index.  c
!                                                                      c
!        entry rddat2 allows more records to be read from the file     c
!        without trying to read a header record.                       c
!                                                                      c
!                                                                      c
!        the call is                                                   c
!             call rddata (inunit,t1,t2,nrecrds,nparms,index,array,    c
!                          nhdr,iflag)                                 c
!        inunit    -  logical unit tape is mounted on                  c
!        t1        -  start time for sampling.                         c
!        t2        -  end time of sampling period.                     c
!        nrecrds   -  maximum number of records to be returned.  if youc
!                     want all the records between t1 and t2, be sure  c
!                     nrecrds >= (t2-t1)/cyctime +1.                   c
!                     on return, nrecrds holds the number of records   c
!                     actually returned.                               c
!        nparms    -  number of parameters to be returned for each     c
!                     record.                                          c
!        index     -  array containing the locations of the parameters c
!                     within a record to be returned.  these need be   c
!                     in no special order.                             c
!        array     -  the array to contain the returned values.  it is c
!                     a two-dimensional array. with the first index    c
!                     referring to record number, and the second to    c
!                     parameter number (not location).                 c
!        nhdr      -  gives the number of physical records in the header
!        iflag     -  as input, controls the printing of the header    c
!                     information.  if iflag = 1, the printing will be c
!                     suppressed.                                      c
!                     as output, it indicates the completion code.     c
!                     iflag=0, normal return, with t2 limiting search. c
!                     iflag=1, normal return, nrecrds limiting search  c
!                     iflag=2, t1 and t2 have invalid times.           c
!                     iflag=3, nrecrds or nparms is < zero.            c
!                     iflag=4, eof was reached on a header read.       c
!                     iflag=5, unrecoverable parity error occurred.    c
!                     iflag=6, eof was reached during data read.       c
!                     iflag=7, catastrophic errors have occurred in    c
!                              execution.                              c
!        m         -  first dimension of input array "array"           c
!        n         -  second dimension of input array "array"          c
!        nst       -  records will be stored in "array" beginning at   c
!                     array(nst,1).                                    c
!                                                                      c
!        written by george lehmann, jr.                                c
!                   october,1976                                       c
!                                                                      c
!        modified to run on the cray-1 by mike daniels                 c
!                   january, 1983                                      c
!                                                                      c
!                                                                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      dimension index(nparms),array(m,n),irate(100),                    &
     &  scale(100),bias(100),inarray(1500),dataray(1500)
      data  iswch /0/
      if(t2.le.t1) go to 900
      if (nrecrds.le.0.or.nparms.le.0) go to 910
      if (iswch.eq.1) go to 100
      iswch = 1
!
!        process header information
!
      call rdhdr (inunit,irate,scale,bias,np,nper,ctime,nr,nw,inarray,  &
     & nhdr,iflag)
      iflag=iflag+1
      go to (100,920,930,90),iflag
   90 write (6,91) inunit
!
!        this entry point allows reading data without first reading
!        the header
!
      entry rddat2
  100 n=nst-1
      t0=t1-float(nr)*ctime
!
!        search for the right records
!
  110 iflag=1
  120 call rdrec (inunit,irate,scale,bias,np,nper,nr,dataray,inarray,   &
     & iflag)
      if (iflag.eq.0) go to 200
      iflag=iflag-89
      go to (940,950,950,930,190), iflag
  190 write (6,91) inunit
  200 if (dataray(1).lt.t0) go to 110
      if (dataray(1).lt.t1) go to 120
      if (dataray(1).gt.t2) go to 800
!
!        move parameters into new array element
!
      n=n+1
      do 300 j=1,nparms
      k=index(j)
  300 array(n,j)=dataray(k)
      if (n.ge.nrecrds) go to 810
      go to 120
!
!        finish up and return
!
  800 nrecrds=n
      iflag=0
      return
  810 nrecrds=n
      iflag=1
      return
!
  900 iflag=2
      write (6,905)
      return
!
  910 iflag=3
      write (6,915)
      return
!
  920 iflag=4
      iswch = 0
      nrecrds=n
      write (6,925)
      return
!
  930  iflag=5
      write (6,935)
      return
!
  940 iflag=6
      iswch = 0
      nrecrds=n
      write (6,945)
      return
!
  950 iflag=7
      write (6,955)
      return
!
   91 format ("0recoverable parity error occurred on unit ",i2)
  905 format ("-  >>>error<<<  invalid values for times t1 and t2")
  915 format ("-  >>>error<<<  number of records or number of parameters&
     & requested is non-positive number")
  925 format ("-  end of volume")
  935 format ("-  >>>error<<<  parity error occurred during tape read")
  945 format ("-  end of file")
  955 format ("-  >>>>>error<<<<< catastrophic error has occurred")
      end
!
      subroutine rdhdr   (inunit,irate,scale,bias,nparms,npercyc,       &
     &  cyctime,nrec,nwords,inarray,nhdr,iflag)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                      c
!                                                                      c
!        this subroutine reads a header block, prints the information  c
!        contained in that block, decodes and returns certain parameters
!        to the calling program.                                       c
!                                                                      c
!        the call is                                                   c
!            call rdhdr   (inunit,irate,scale,bias,nparms,npercyc,     c
!                           cyctime,nrec,nwords,inarray,iflag)         c
!        inunit    -  logical unit number of the tape used             c
!        irate     -  array to contain returned values of sampling ratec
!        scale     -  array to contain returned values for rescaling   c
!                     the data.                                        c
!        bias      -  array to contain the returned values for         c
!                     correcting the data offset.                      c
!        nparms    -  returns the number of parameters per logical rec c
!        npercyc   -  returns the number of parameters taken per cycle c
!        cyctime   -  returns the cycle time used                      c
!        nrec      -  returns the number of logical records per physical
!                     record                                           c
!        nwords    -  returns the number of words of memory occupied   c
!                     by a physical record                             c
!        inarray   -  working array for the subroutine, must be at     c
!                     least 1500 words.                                c
!        nhdr      -  gives number of physical records in the header   c
!        iflag     -  as input, suppresses print if iflag=1            c
!                  -  as output, returns completion code as follows    c
!                     iflag=0 indicates normal return                  c
!                     iflag=1 indicates eof sensed during operation    c
!                     iflag=2 indicates unrecoverable parity error     c
!                             occured during operation.                c
!                     iflag=3 indicates recoverable parity error was   c
!                             detected.                                c
!                                                                      c
!        written by george lehmann, jr.                                c
!                   october,1976                                       c
!                                                                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!dln      dimension irate(1),scale(1),bias(1),inarray(1),label(7)
!dln fix array( 1) with assumed size array
      dimension irate(*),scale(*),bias(*),inarray(*),label(7)
!dln added unit to integer declaration
      integer ascii(64),wkarray(16000),flg, unit
!dln need type real array for call to rdhdr
      dimension array( 1500)
      common /tpcorr/ icorr1
      common /locatn/ locat(500)
      logical icorr1
!dln convert ccB to o'cc'
      data ascii/o'72',o'101',o'102',o'103',o'104',o'105',o'106',o'107',&
     &o'110',o'111',o'112',o'113',o'114',o'115',o'116',o'117',o'120',   &
     &o'121',o'122',o'123',o'124',o'125',o'126',o'127',o'130',o'131',   &
     &o'132',o'60',o'61',o'62',o'63',o'64',o'65',o'66',o'67',o'70',     &
     &o'71',o'53',o'55',o'52',o'57',o'50',o'51',o'44',o'75',o'40',o'54',&
     &o'56',o'43',o'133',o'135',o'45',o'42',o'137',o'41',o'46',o'47',   &
     &o'77',o'74',o'76',o'100',o'134',o'136',o'73'/
      nparity=0
      if (iflag.eq.0) write(6,11)
!
!        fetch the header records from tape
!
      ist = 1
      icorr1 = nhdr.eq.3
      do 100 i=1,nhdr
!dln    7 buffer in (inunit,1)(inarray(ist),inarray(1500))
    7 continue
      read( unit= inunit, iostat= unit) inarray( 1: 1500)
!dln   10 if (unit(inunit)) 20,991,15
   10 continue
      if( unit ) 20, 991, 15
   15 if(nparity.ne.0) go to 995
      write (6,996)
      nparity=1
      backspace inunit
      go to 7
   20 ilen=length(inunit)
  100 ist=ist+ilen
      iter=int((ist-1)*64/6)+1
      call gbytes(inarray,wkarray,0,6,0,iter)
      do 102 j1=1,iter
      if (wkarray(j1).gt.63) then
         wkarray(j1)=ascii(45)
      else
         wkarray(j1)=ascii(wkarray(j1)+1)
      endif
  102 end do
      call sbytes(inarray,wkarray,0,8,0,iter)
      if(iflag.eq.0) write (6,150) (inarray(k),k=1,138)
!
!        read record descriptor information
!
!      decode (121,170,inarray(23)) nparms,npercyc,cyctime
!  170 format (i3,67x,i4,40x,f5.3)
      read(inarray(23), '(i3,67x,i4,40x,f5.3)') nparms,npercyc,cyctime
!dln      decode (107,180,inarray(39)) nrec,ntotal,nwords
      read( inarray( 39), '(1x,i2,26x,i4,70x,i3)') nparms,npercyc,cyctime
!
!        read and print the remainder of the header
!
      if (iflag.eq.0) write (6,195)
      flg=0
      id=138
      loc=1
      do 200 i=1,nparms
      if (flg.eq.0) then
!dln         decode (100,210,inarray(id)) j,irate(i),label,scale(i),bias(i)
         read( inarray( id), '(4x,i3,1x,i4,5x,7a8,10x,f7.1,3x,f7.1)')   &
     &        j,irate(i),label,scale(i),bias(i)
      else
!dln         decode (96,215,inarray(id+1)) j,irate(i),label,scale(i),bias(i)
         read( inarray( id+1), '(i3,1x,i4,5x,7a8,10x,f7.1,3x,f7.1)')    &
     &        j,irate(i),label,scale(i),bias(i)
      endif
      if (i.eq.1) go to 190
      loc=loc+irate(i-1)
  190 if(iflag.eq.0) write (6,220) j,loc,irate(i),label,scale(i),bias(i)
      locat(i)=loc
      if (flg.eq.0) then
         id=id+12
         flg=1
      else
         id=id+13
         flg=0
      endif
  200 end do
!dln  202 call rdrec0 (inunit,irate,scale,bias,nparms,npercyc,nrec,inarray, &
!dln     & inarray,iflag)
!dln call with correct type of work array
  202 call rdrec0 (inunit,irate,scale,bias,nparms,npercyc,nrec,array,   &
     &inarray,iflag)
      iflag=0
      if(nparity.ne.0) iflag=3
      return
!
!        error messages
!
  991 write (6,992)
      iflag=1
      return
!
  995 write (6,996)
      iflag=2
      return
!
   11 format ("1")
  150 format (10(1x,14a8,/))
  180 format (1x,i2,26x,i4,70x,i3)
  195 format (/," item   location   rate",17x,"description",20x,"name", &
     &4x,"units",18x,"scale",12x,"bias",/)
  210 format (4x,i3,1x,i4,5x,7a8,10x,f7.1,3x,f7.1)
  215 format (i3,1x,i4,5x,7a8,10x,f7.1,3x,f7.1)
!dln  220 format ((1x,i3,*)*,i8,i9,5x,7a8,17x,f9.2,5x,f9.2)
  220 format (1x,i3,i8,i9,5x,7a8,17x,f9.2,5x,f9.2)
  992 format ("-  >>>error<<<  eof sensed on header read")
  996 format ("-  >>>error<<<  parity error sensed on header read")
      end
!
      subroutine rdrec0  (inunit,irate,scale,bias,nparms,npercyc,nrec,  &
     &  array,inarray,iflag)
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!                                                                      c
!        this subroutine is used to fetch logical records from tape.   c
!        it has two modes of operation, governed by parameter iflag.   c
!        in scroll mode, the next sequential logical record is         c
!        returned.  in select mode, logical record n is returned.      c
!        if the logical record requested comes before the current      c
!        record pointer, the next physical record is read to obtain    c
!        the correct logical record.                                   c
!                                                                      c
!        a call to rdrec0 initializes the pointer and enables the read c
!        operation.  this call is normally made by subroutine rdhdr.   c
!        entry point rdrec is the call used to read a logical record.  c
!                                                                      c
!        the call is                                                   c
!          call rdrec (inunit,irate,scale,bias,nparms,npercyc,nrec,    c
!                      array,inarray,iflag                             c
!                                                                      c
!        inunit    -  the number of the logical unit the tape is mounted
!                     on                                               c
!        irate     -  the array containing the sample rates for the    c
!                     parameters (returned from rdhdr)                 c
!        scale     -  the array containing the scaling factors for the c
!                     parameters (returned from rdhdr)                 c
!        bias      -  the array containing the offset values for the   c
!                     parameters (returned from rdhdr)                 c
!        nparms    -  number of parameters to be found in a logical    c
!                     record                                           c
!        npercyc   -  total number of data items per logical record    c
!        nrec      -  number of logical records per physical record    c
!        array     -  real array in which the properly biased and      c
!                     scaled data items will be returned               c
!        inarray   -  working array for physical record storage.  must c
!                     be at least 1500 words long.                     c
!        iflag     -  as input, controls the mode of operation and the c
!                     logical record to be returned                    c
!                     iflag=0 selects scroll mode                      c
!                     iflag=1,2,...,nparms selects select mode, with   c
!                     the value of iflag choosing the logical record   c
!                     to be returned.  iflag<0 or iflag>nparms will    c
!                     produce an error message.                        c
!                     as output, iflag indicate the completion code    c
!                     iflag=0  indicates normal completion             c
!                     iflag=90 indicates eof was encountered           c
!                     iflag=91 indicates an invalid value for iflag    c
!                     iflag=92 indicates the subroutine was disabled   c
!                     iflag=93 indicates an unrecoverable parity error c
!                              occurred.                               c
!                     iflag=94 indicates a recoverable parity error    c
!                              occurred.                               c
!                                                                      c
!        written by george lehmann, jr.                                c
!                   october,1976                                       c
!                                                                      c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!dln      dimension irate(1),scale(1),bias(1),array(1),inarray(1)
!dln fix array( 1) with assumed size array
      dimension irate(*),scale(*),bias(*),array(*),inarray(*)
      common /tpcorr/ rhotdc
      logical rhotdc
      data rhotdc /.true./
      integer enable, unit
      data enable /1/
      logrec=nrec+1
      enable=0
      items=npercyc*nrec
      return
!
!        begin normal read sequence
!
      entry rdrec(inunit,irate,scale,bias,nparms,npercyc,nrec,array,    &
     &            inarray,iflag)
!
      nparity=0
      if (iflag.gt.nrec) go to 970
      if (iflag.lt.0) go to 960
      if (enable.ne.0) go to 980

      if (logrec.le.nrec.and.iflag.eq.0.or.iflag.gt.logrec) go to 100
!
!        read in new physical record
!
!dln    5 buffer in (inunit,1)(inarray(1001),inarray(1500))
    5 continue
      read( inunit, iostat= unit) inarray( 1001: 1500)
!dln   10 if (unit(inunit)) 20,950,990
   10 continue
      if (unit) 20,950,990
   20 logrec=1
      nleng=length(inunit)
      if (iflag.ne.0) logrec=iflag
      call gbytes (inarray(1001),inarray(1),0,20,0,items)
  100 istart=(logrec-1)*npercyc
      iset=1
      do 200 j=1,nparms
      j2=irate(j)
      do 200 k=1,j2
      array(iset)=float(inarray(istart+iset))/scale(j)-bias(j)
  200 iset=iset+1
      if (rhotdc) array(7)=array(7)*(array(6)+273.16)/(array(4)+273.16)
      logrec=logrec+1
      iflag=0
      if (nparity.ne.0) iflag=94
      return
!
!        error messages
!
  960 iflag=91
      write (6,961)
      return
!
  950 iflag=90
      enable=1
      return
!
  970 iflag=91
      write (6,971)
      return
!
  980 iflag=92
      write (6,981)
      return
!
  990 write (6,991) inunit
      if (nparity.ne.0) go to 992
      backspace inunit
      nparity=1
      go to 5
  992 iflag=93
      return
!
  961 format ("-  >>>error<<<  value of iflag is negative")
  971 format ("-  >>>error<<<  logical record number requested exceeds a&
     & vailable number of records")
  981 format ("-  >>>error<<<  data record requested before header recor&
     & has been read")
  991 format ("-  >>>error<<<  parity error encountered during read oper&
     & ation on unit ",i2)
      end
!
      subroutine skipblk (inunit,n)
!
!        this routine skips the next n records on the tape mounted
!        on logical unit inunit
!
!        on return, n indicates the completion status
!          n = 1   normal return
!          n = 2   eof detected
!          n = 3   parity error detected
!
!dln add integer declaration for unit
      integer :: unit
      dimension i(1)
      if (n.eq.0) n=1
      do 20 j=1,n
!      buffer in (inunit,1) (i,i)
      read( inunit, iostat= unit) i
!dln   10 if (unit(inunit)) 20,200,300
   10 continue
      if( unit ) 20,200,300
   20 end do
      n=1
      return
  200 n=2
      return
  300 n=3
      return
      end
!
      subroutine skipfile(iun,n)
      do 10 i=1,n
   10 call skfile(iun)
      return
      end
!
      function ievnt(e)
!dln add / to end of data statement
      data n4,n3,n2 /3584,498,56/
      i=e
      i4=i/n4
      i3=(i-i4*n4)/n3
      i2=(i-i4*n4-i3*n3)/n2
      i1=i-i4*n4-i3*n3-i2*n2
      if (i4.gt.0) i4=1000
      if (i3.gt.0) i3=100
      if (i2.gt.0) i2=10
      if (i1.gt.0) i1=1
      ievnt=i4+i3+i2+i1
      return
      end
!
      function itme (t)
      i=t
      ih=i/3600
      im=(i-ih*3600)/60
      is=i-ih*3600-im*60
      itme=is+im*100+ih*10000
      return
      end
