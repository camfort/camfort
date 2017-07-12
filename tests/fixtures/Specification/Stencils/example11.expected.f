      program example11

      integer i, imax
      parameter (imax = 3)

      real a(0:imax), b(0:imax), x

      do 1 i = 1, (imax-1)
         if (.true.) then
c= access readOnce, pointed(dim=1) :: b
        x = b(i)
         end if
 1    continue

      end
