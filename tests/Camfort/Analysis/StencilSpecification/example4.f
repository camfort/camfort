      program example4
      logical BAR = .true.
      integer X(0:5)
      integer J
      do J=1, 10
         IF (BAR)   X(J) = X(J)+1
       end do
      end
