program iccs
  implicit none

  real, unit(m) :: x ! metres
  real, unit(s) :: t ! seconds
  real, unit(m / s) :: v
  real :: x2
  real :: t2

  x = 50 
  t = 1.3
  v = x / t
  
  x2 = square(x)
  t2 = square(t)

  contains

  real function square(y)
    real y
    square = y * y
  end function

end program
