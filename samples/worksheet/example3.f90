program example3
  integer, dimension(10) :: a
  integer :: i

  do i = 3, 8
    != stencil readOnce, centered(dim=1, depth=2) :: a
    a(i) = a(i-2) + a(i-1) + a(i) + a(i+2)
  end do
end program example3
