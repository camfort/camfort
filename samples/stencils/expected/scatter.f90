program range

implicit none

integer i, imax
parameter (imax = 3)

real a(0:imax), b(0:imax)

do i = 0, imax
   != stencil readOnce, (backward(depth=1, dim=1, nonpointed)) :: b
   a(i+1) = b(i)
end do
  
end program
