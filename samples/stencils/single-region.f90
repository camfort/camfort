! Examples of single region stencils
!
! Each of these four stencils is expressed without using any 
! combining operators (e.g. + or *)
program singleregion
implicit none

integer i
parameter (n=10)
real a(0:imax,0:imax)
real b(0:imax,0:imax)
real c(0:imax,0:imax)
real d(0:imax,0:imax,0:imax)
real e(0:imax,0:imax)
real f(0:imax,0:imax)

! A forwards stencil (depth 2)
do i=1, n
 e(i, 0) = a(i, 0) + a(i+1, 0) + a(i+2, 0)
end do

! A backward stencil (depth 2)
do i=1, n
 f(i) = b(i) + b(i-1) + b(i-2)
end do

! A centered stencil (depth 1)
do i=1, n
 do j=1, n
  e(i, j) = (c(j-1) + c(j) + c(j+1))/3.0
 end do
end do

! A pointed stencil
do i=1, n
 do j=1, n
  e(i, j) = d(0, 0, i)
 end do
end do

! A (backwards, depth 2) non-pointed stencil
do i=1, n
 b(i) = a(i-1) + 10*a(i-2)
end do

end
