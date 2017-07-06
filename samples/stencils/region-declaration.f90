! Region declarations can be used to create reusable
! names for specifications or parts of specifications
program regiondeclaration
implicit none

integer i
parameter (n=10)
real a(0:n,0:n)
real b(0:n,0:n)

!= region :: r1 = forward(depth=1, dim=1)
!= region :: r2 = forward(depth=1, dim=2)
!= region :: robertsCross = r1 * r2

do i=1, n
 do j=1, n
  != stencil readOnce robertsCross :: a
  b(i,j) = a(i,j) + a(i+1,j) + a(i,j+1) + a(i+1,j+1)
 end do
end do

end
