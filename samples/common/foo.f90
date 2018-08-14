program foo
  implicit none

  integer :: a = 10
  integer :: i
  integer :: j
  real(kind=8) :: z

  common i, j

  i = 3
  j = 10

  CALL bar(a)

  print '(i2)', i

end program foo
