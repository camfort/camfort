! Specifications can also be given to array computations
! which are not stencils (i.e. do not have a left hand
! side which is an array subscript). This is done using
! the access keyword
program access
implicit none

integer i, n
parameter (n = 3)
real a(0:n)
real r

do i = 1,n
 != access readOnce, (pointed(dim=1)) :: a
 r = max(a(i), r)
end do

end
