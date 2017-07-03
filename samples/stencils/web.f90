program web
  integer :: i, k
  real, dimension(10, 10) :: a, b
  real :: x, y
  integer :: n, m

    do i = 1, n
    do j = 1, m
      x = a(i, j-1) + a(i, j+1) + a(i, j)
      y = a(i+1, j-1) + a(i+1, j+1) + a(i+1, j)
      b(i, j) = x + y
    end do
  end do
  end program web
