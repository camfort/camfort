program foo
  implicit none

  integer :: x
  integer :: y

  common x, y 

  x = 10
  CALL bar(x)
  print '(i2)', x

   

  contains


end program foo
