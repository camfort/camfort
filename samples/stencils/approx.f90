      program approx
      implicit none

      integer i, imax
      parameter (imax = 3)
      real a(0:imax)
      
      do i = 0, imax
     != stencil atLeast, readOnce, reflexive(dims=1)        :: a
     != stencil atMost, readOnce, (forward(depth=2, dim=1)) :: a
            a(i) = a(i) + a(i+2)
      end do

      end program
