program bug
  implicit none
  integer :: i
  real, dimension(2) :: a, b
  real :: x
  do i = 1, 2
    x = abs(a(i))
    b(i) = x
  end do
end program
