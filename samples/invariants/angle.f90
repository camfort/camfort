! Verify with CamFort using:
!  camfort invariants-check angle.f90

program degrees
  implicit none

  real, parameter :: deg = -45
  real rad

  rad = toRad(deg)
  print *, rad

contains

  != static_assert pre("deg >= 0" & "deg <= 360")
  != static_assert post("toRad >= 0" & "toRad <= 6.284")
  real function toRad(deg)
    real deg
    real, parameter :: pi = 3.14159265358979323864
    toRad = 2 * pi * (deg/360)
  end function toRad
end program degrees
