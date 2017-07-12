program range

implicit none

integer i, imax
parameter (imax = 3)

real a(0:imax), b(0:imax)

!= stencil readOnce, pointed(dim=1) :: b
a(:) = b(:)
  
end program
