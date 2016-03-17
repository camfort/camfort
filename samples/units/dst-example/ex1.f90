program energy
  != unit :: speed = m/s

 != unit (m / s** (2)) :: gravity
 != unit (m) :: height
 != unit (kg) :: mass
  real, parameter :: mass = 3.00, gravity = 9.81, height = 4.20
 != unit (kg m** (2) / s** (2)) :: potential_energy
  real :: potential_energy

 != unit (1) :: half
 != unit (speed) :: velocity
  real, parameter :: half = 0.5, velocity = 4.00
 != unit (kg m** (2) / s** (2)) :: kinetic_energy
 != unit (kg m** (2) / s** (2)) :: total_energy
  real :: kinetic_energy, total_energy

  potential_energy = mass * gravity * height
  kinetic_energy = half * mass * square(velocity)

  total_energy = potential_energy + kinetic_energy

  contains

  real function square(x)
    real x
    square = x * x
  end function square
end program energy
