      program example10

      integer i, imax
      parameter (imax = 3)

      real a(0:imax), b(0:imax), x

      do 1 i = 1, (imax-1)
         if (.true.) then
        x = b(i)
         end if
 1    continue

      end
