program center
  implicit none

  integer i, j, imax, jmax
  parameter (imax = 3, jmax = 3)
  logical flag

  real a(0:imax,0:jmax), b(0:imax,0:jmax), x

  do i = 1, (imax-1)
     do j = 1, (jmax-1)
        b(i,j) = a(i,j) + a(i+1,j) + a(i-1,j) + a(i,j+1) + a(i,j-1)
     end do
  end do
  
end program center
