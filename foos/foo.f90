program foo
  implicit none

  integer :: i
  integer :: x
  integer :: y

  common x, y 
  
  x = 3
  y = 10

  CALL bar(i)

  print '(i2)', x 

  contains


end program foo
