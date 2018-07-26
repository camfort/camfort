program example
  integer, parameter :: x = 1
  integer, parameter :: y = 2
  integer, parameter :: z = 3

  real, dimension(3,3) :: d, e
  real :: sum
  integer :: i
  common /example/ d
  do i=1,3
     sum = d(i,x) + d(i,y) + d(i,z)
  end do
end program
