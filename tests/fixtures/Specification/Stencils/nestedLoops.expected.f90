program nestedLoops
  integer :: i, iter, itermax
  real, dimension(10) :: a, b
  real :: x, y

  do iter = 1, itermax
     do i = 1, 10
        x = a(i+1)
        != stencil readOnce, (forward(depth=1, dim=1)) :: a
        b(i) = a(i) + x
    end do

    y = x
  end do

end program nestedLoops
