program reduction
  implicit none
  real :: r, x, beta_mod, z, eps_E
  integer :: i, j
  real, dimension(10,10) :: b, c
  real, dimension(10) :: a, d

  do i = 1, 10
     do j = 1, 10
        != stencil readOnce, (forward(depth=1, dim=1, nonpointed))*(pointed(dim=2)) :: b
        a(i) = b(i+1, j)
        != stencil readOnce, (backward(depth=1, dim=1, nonpointed))*(pointed(dim=2)) :: b
        a(i) = b(i-1, j)
        != stencil readOnce, (pointed(dim=1)) :: d
        c(i+1, j) = d(i+1)
     end do
  end do

end program reduction

