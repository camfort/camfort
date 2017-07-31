program user
  use provider
  integer :: i
  integer, dimension(10) :: b

  do i = 1, 10
     a(i) = b(i)
  end do
end program user
