program simple
  implicit none

  real, unit(m)   :: x
  real, unit(s)   :: t
  real, unit(m/s) :: v
  
  x = 20.0
  t = 3.0 
  v = x / t 
  
  print *, v  
end program
