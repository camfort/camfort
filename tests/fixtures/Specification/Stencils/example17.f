      program example16
      implicit none

      integer i
      real a(0:10), x

      do i = 0, 10
c= stencil readOnce, (forward(depth=1, dim=1)) :: a
            x = a(i) + a(i+1)
      end do

      end
