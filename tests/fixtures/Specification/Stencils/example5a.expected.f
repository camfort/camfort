      program example5
      implicit none

      integer i, imax, x
      parameter (imax = 3, x = (1+1)*imax-2*(1+1))
      real a(0:imax)

      do i = 0, imax
c= stencil readOnce, atLeast, pointed(dim=1) :: a
c= stencil readOnce, atMost, forward(depth=2, dim=1) :: a
            a(i) = a(i) + a(i+x)
      end do

      end
