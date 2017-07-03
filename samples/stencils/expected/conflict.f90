      program conflict
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)

      real a(0:imax,0:jmax)
      
      do 1 i = 0, imax
         do 2 j = 0, jmax
            a(i,j) = a(j,i-1) + a(i+1,j) + a(i, j) 
 2       continue
 1    continue

      end
