program simple
  implicit none

  real :: x
  real, unit(s) :: t
  real, unit(m/s):: v
  real :: s

  x = 20.0
  t = 3.0 
  v = x / t
  s = abs(v)
  
  print *, s
  print *, v
end program
