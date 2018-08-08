! test
program example
  integer, parameter :: x = 1
  integer, parameter :: y = 2
  integer, parameter :: z = 3

  != ddt example_d_common_type(1=>label1, 2=>label1, 3=>label3, 4=>label4) :: d(dim=2)
  real, dimension(3,3) :: d, e
  real :: sum
  integer :: i
  common /example/ d
  do i=1,3
     sum = d(i,x) + d(i,y) + d(i,z)
  end do
end program
