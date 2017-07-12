      program example9

      integer i, imax
      parameter (imax = 3)

      real a(0:imax), b(0:imax)

      do 1 i = 1, (imax-1)
c= stencil readOnce, pointed(dim=1) :: b
        a(i) = b(i)
 1    continue

      end
