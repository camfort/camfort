program foo
  implicit none

  integer :: i
  integer :: x = 4
  integer :: y

  real(kind=8) :: z

  common x, y
  
  x = 3
  y = 10 + x

  CALL bar(i)

  print '(i2)', x 

  contains


end program foo
