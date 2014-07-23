program simple
  implicit none

  real, unit(m) :: x
  real, unit(s)  :: t
  real :: v   

  x = 20.0
  t = 3.0 
  v = t / x
  
  print *, v  
end program
