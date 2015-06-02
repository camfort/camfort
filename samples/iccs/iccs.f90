program iccs
  implicit none
  real, unit(m) :: x
  real, unit(s) :: t
  real, unit(m / s) :: v  
  real :: a

  x = 50 
  t = 1.3
  v = (x / t) * a
end program
