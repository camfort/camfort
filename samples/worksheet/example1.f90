program energy
  real, parameter :: mass = 3.00, gravity = 9.81, height = 4.20
  real :: potential_energy

  real, parameter :: velocity = 4.00
  real :: kinetic_energy, total_energy

  potential_energy = mass * gravity * height
  kinetic_energy = 0.5 * mass * square(velocity)

  total_energy = potential_energy + kinetic_energy

  contains

  real function square(x)
    real x
    square = x * x
  end function square
end program energy
