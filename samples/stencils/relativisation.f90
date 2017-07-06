! Example of relativisation
!
! Stencil shapes are relative to the index on the left hand
! side. In this case the first index has a non-zero offset
! and so the right hand side is understood relative to this
program relativisation
implicit none

integer i, j, n
parameter (n = 3)
real a(0:n, 0:n)
real b(0:n, 0:n)

do i=1, n
   do j=1, n
      a(i+1,j) = b(i,j) + b(i-1,j)
   end do
end do

end program
