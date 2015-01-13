program simple
  implicit none

  unit :: le = m
  integer, unit(le) :: x
  real, unit(s) :: t
  real :: v, s, y, fooz
  logical :: check
  real :: foon

  real :: d
  real, unit(le**2) :: c

   y = x ** 1 !x * x

  x = 20.0  ! initial
  t = 3.0   !  values
  v = x / t
  s = abs(v)
  d = (y / t) / c

  check = s == v
  if (check) then 
     print *, "Velocity is positive"
  endif 
  
  print *, s, v
end program
