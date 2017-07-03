! Discrete one-dimensional laplacian
!
! Implemented as convolution with [1, -2, 1]. This is a 
! centered, depth 1 stencil
program laplace
implicit none

integer i
parameter (n=10)
real a(0:imax)
real b(0:imax)

do  i = 1, (n-1)
 b(i) = a(i-1) - 2*a(i) + a(i+1)
end do

end
