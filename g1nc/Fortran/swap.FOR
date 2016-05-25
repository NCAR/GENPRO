! Adapted from http://rda.ucar.edu/libraries/gbytes/swap.for

    subroutine swap4(in,io,nn)
! swaps bytes in groups of 4 to compensate for byte swapping within
!    words which occurs on DEC (VAX) and PC machines.
!
! in - input array to be swapped
! io - ouput array with bytes swapped
! nn - number of bytes to be swapped
    logical*1   in(1),io(1),ih
!       character*1 in(1),io(1),ih             ! Cray CF90 (Version 3.0.1.3)
!          Use character*1 instead of logical*1 when compling on a Cray
    do 10 i=1,nn,4
    ih=in(i)
    io(i)=in(i+3)
    io(i+3)=ih
    ih=in(i+1)
    io(i+1)=in(i+2)
    io(i+2)=ih
   10   continue
    return
    end
    subroutine swap2(in,io,nn)
! swaps bytes in groups of 2 to compensate for byte swapping within
!    words which occurs on DEC (VAX) and PC machines.
!
! in - input array to be swapped
! io - ouput array with bytes swapped
! nn - number of bytes to be swapped
    logical*1   in(1),io(1),ih
!       character*1 in(1),io(1),ih             ! Cray CF90 (Version 3.0.1.3)
!          Use character*1 instead of logical*1 when compling on a Cray
    do 10 i=1,nn,2
    ih=in(i)
    io(i)=in(i+1)
    io(i+1)=ih
   10   continue
    return
    end
