      program example10

      integer i, imax
      parameter (imax = 3)

      real a(0:imax), b(0:imax)

      do 1 i = 1, (imax-1)
         if (.true.) then
c= stencil readOnce, (pointed(dim=1)) :: b
        a(i) = b(i)
         end if
 1    continue

      end
