! Example of an approximate stencil specification
!
! In situations where the specification language cannot exactly
! describe the pattern an approximate specification can be given as an
! upper and lower bound using the atMost and atLeast modifiers
program approx
implicit none

integer i, n
parameter (n = 3)
real a(0:n)
      
do i = 0, n
 a(i) = a(i) + a(i+4)
end do

end program
