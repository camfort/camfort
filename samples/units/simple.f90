program simple
  implicit none
  integer, unit(m) :: x
  real, unit(s) :: t
  real :: v
  real :: s
  real :: y

   y = x * x

  x = 20.0  ! initial
  t = 3.0   !  values
  v = x / t
  s = abs(v)
  
  print *, s, v
end program
