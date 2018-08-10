program inconsistSpec4
  integer, parameter :: x = 2
  integer, parameter :: y = 3
  integer, parameter :: z = 4

  != ddt* inconsist4(2=>mylabel, 3=>label3, 4=>label4) :: d(dim=2)
  real, dimension(3,3) :: d, e
  real :: sum
  integer :: i
  common /inconsist4/ d
  do i=1,3
     sum = d(i,x) + d(i,y) + d(i,z)
  end do
end program
