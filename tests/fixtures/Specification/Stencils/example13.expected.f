      program example13
      implicit none

      integer i, imax
      parameter (imax = 3)
      real a(0:imax)

c= region :: r1 = pointed(dim=1)
      do i = 0, imax
c= stencil readOnce, atLeast, r1 :: a
c= stencil readOnce, atMost, forward(depth=2, dim=1) :: a
            a(i) = a(i) + a(i+2)
      end do

      end
