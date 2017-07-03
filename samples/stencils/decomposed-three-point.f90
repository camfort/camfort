! Flows-to example
!
! This is a three point stencil (centred, depth 1) which
! has been decomposed. Camfort will infer a single stencil
! for the loop.
program decomposedthreepoint
implicit none

integer i
parameter (n=10)
real a(0:imax)
real b(0:imax)

do i = 1, n
 x = a(i)
 y = a(i+1)
 b(i) = (a(i-1) + x + y)/3.0
end do

end
