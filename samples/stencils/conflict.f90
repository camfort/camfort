      program conflict
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)

      real a(0:imax,0:jmax)
      
      do 1 i = 0, imax
         do 2 j = 0, jmax
 != stencil readOnce, reflexive(dims=2), (centered(depth=1, dim=1)) :: a
            a(i,j) = a(j-1,i) + a(i+1,j) + a(i, j) 
 2       continue
 1    continue

      end
