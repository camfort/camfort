program main
!=unit(s) :: t
  real :: t
  t = 1
  t = dub(t)
end program main

real function dub(x) 
    real x
    real :: k = 2
! bug: k should be identified as unitless
    dub = k*x
end function
