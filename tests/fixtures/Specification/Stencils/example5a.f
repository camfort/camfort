      program example5
      implicit none

      integer i, imax, x
      parameter (imax = 3, x = (1+1)*imax-2*(1+1))
      real a(0:imax)

      do i = 0, imax
            a(i) = a(i) + a(i+x)
      end do

      end
