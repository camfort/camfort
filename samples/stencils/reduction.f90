program reduction
  implicit none
  real :: r
  integer :: i, j
  real, dimension(10) :: a, b

  do i = 1, 10
     z = b(i+1)
     a(i) = (b(i) + z) / 2
     z = a(i)
     r = max(r, z)
  end do

end program reduction

