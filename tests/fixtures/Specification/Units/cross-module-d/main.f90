program main

  use functions

  implicit none

  != unit m :: length_m
  real :: length_m = 5.0

  ! units? 
  real :: length_km

  ! convert from m to km
  length_km = convert(length_m)

end 