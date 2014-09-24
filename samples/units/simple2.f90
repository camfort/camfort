program simple
  implicit none

  real, unit(s) :: t
  real, unit(m/s):: v
  real :: x, s, a, b

  x = 20.0
  t = 3.0 
  v = x / t
  s = abs(v)

  a = square (x) 
  b = square (t)
  
  print *, s
  print *, v
  print *, a

  contains 

  real function square(y)
    real y, m
    square = y * y * m
  end function
    
end program
