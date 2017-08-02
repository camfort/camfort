program crossModuleUser
  use crossModuleProvider
  implicit none
  != unit (m) :: x
  != unit (s) :: y
  integer :: x, y, z
  x = 1
  y = 1
  z = add(x, y)
end program
