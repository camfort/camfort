      program example3
      implicit none

      integer i, x, y, imax, xmax, ymax
      parameter (imax = 3, jmax = 3)

      real a(0:imax,0:jmax), b(0:imax,0:jmax), c(0:imax), d(0:imax), acc

      do i=1,imax
        do x=1,xmax
           do y=1,ymax
             acc = acc + a(x,y,i)
           end do
        end do
 1      b(1,i) = acc
      end do

      do i=1,imax
c     Label 1 should get same spec as this line
         b(1,i) = a(:,:,i)
      end do

      c(:) = d(:)
      c(:) = a(0,:)

      end
