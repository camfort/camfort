      program nonneighbour
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)

      real a(0:imax,0:jmax)

      do 1 i = 0, imax
         do 2 j = 0, jmax
            != stencil readOnce, forward(depth=1, dim=1, nonpointed)*forward(depth=1, dim=2, nonpointed) + pointed(dim=1)*pointed(dim=2) :: a
            a(i,j) = a(i, j) + a(i+1, j+1)
 2       continue
 1    continue

      do i = 0, imax
         do j = 0, jmax
            write (*,*) a(i,j)
         end do
      end do

      end
