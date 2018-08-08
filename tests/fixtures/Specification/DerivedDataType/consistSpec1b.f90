program consistSpec1
  integer, parameter :: x = 2
  integer, parameter :: y = 3
  integer, parameter :: z = 4

  != ddt consist1(2=>label2, 3=>label3, 4=>label4) :: d(dim=2)
  real, dimension(3,3) :: d, e
  real :: sum
  integer :: i
  common /consist1/ d
  do i=1,3
     sum = d(i,x) + d(i,y) + d(i,z)
  end do
end program
