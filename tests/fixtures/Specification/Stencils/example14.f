      program example14
      implicit none

      integer i, imax
      parameter (imax = 3)
      real a(0:imax)

      do i = 0, imax
c= stencil readOnce, atLeast, (pointed(dim=1)) :: a
c= stencil readOnce, atLeast, (pointed(dim=1)) :: a
            a(i) = a(i) + a(i+2)
      end do

      end
