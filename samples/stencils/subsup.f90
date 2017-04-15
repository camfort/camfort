program reduction
  implicit none
  real :: r, x, beta_mod, z, eps_E
  integer :: i, j
  real, dimension(10,10) :: b, c
  real, dimension(10) :: a, d

  do i = 1, 10
     do j = 1, 10
        a(i) = b(i+1, j)
        a(i) = b(i-1, j)
        c(i+1, j) = d(i+1)
     end do
  end do

end program reduction

