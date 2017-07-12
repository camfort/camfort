      program example15
      implicit none

      integer i
      real a(0:10)

      do i = 0, 10
c= stencil readOnce, forward(depth=1, dim=1) :: a
c= stencil readOnce, forward(depth=1, dim=1) :: a
            a(i) = a(i) + a(i+1)
      end do

      end
