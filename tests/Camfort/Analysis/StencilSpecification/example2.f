      program example2
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)
      real a(0:imax,0:jmax), b(0:imax,0:jmax), c, x

!= region r1 = centered(depth=1, dim=1)

! some kind of setup
      do 1 i = 0, imax
         do 2 j = 0, jmax
            a(i,j) = i+j
 2       continue
 1    continue

! compute mean
      do 3 i = 1, (imax-1)
         do 4 j = 1, (jmax-1)
            if (.true.) then
!= region r2 = centered(depth=1, dim=2)
               x = a(i-1,j) + a(i,j) + a(i+1,j) + abs(0)
!= stencil readOnce, (reflexive(dim=1))*r2 + (reflexive(dim=2))*r1 :: a               
             b(i,j) = (x + a(i,j-1) + a(i,j+1)) / 5.0
! No specification should be inferred here
             b(0,0) = a(i, j)
            end if
 4       continue
      x = a(i,0)
      y = a(i-1,0)
!= stencil readOnce, backward(depth=1, dim=1) :: a      
      b(i,0) = (x + y)/2.0
 3    continue

      b(i,j) = a(i,j)

      end
