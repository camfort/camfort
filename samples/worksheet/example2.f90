program stencil
  integer, dimension(10) :: a
  integer :: i

  data a/10*1/

  do i = 2, 9
    a(i) = a(i-1) + a(i) + a(i+1)
  end do

  print *, a(1:9)
end program stencil
