program foo
  use foobar, x => y
  implicit none

  integer :: i
  integer :: x
  integer :: y

  real(kind=8) :: z

  common x, y
  
  x = 3
  y = 10

  CALL bar(i)

  print '(i2)', x 

  contains


end program foo
