program web
  integer :: i, k
  real, dimension(10, 10) :: a, b
  real :: x, y
  integer :: n, m
  
    do i = 1, n
    do j = 1, m
      x = a(i, j-1) + a(i, j+1) + a(i, j)
      y = a(i+1, j-1) + a(i+1, j+1) + a(i+1, j)
      != stencil readOnce, (backward(depth=1, dim=1))*(centered(depth=1, dim=2)) :: a
      b(i, j) = x + y
    end do
  end do
  end program web
  
