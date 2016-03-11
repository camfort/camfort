      program center
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)
      
      real a(0:imax,0:jmax), b(0:imax,0:jmax)

      do 1 i = 1, (imax-1)
         do 2 j = 1, (jmax-1)
            b(i,j) = a(i,j) + a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1)
 2       continue
 1    continue

      end 
