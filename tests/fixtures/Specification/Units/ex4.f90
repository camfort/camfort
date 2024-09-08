program energy

  integer, parameter :: n = 1

  != unit Flibs :: mass
  real mAss(n)

  != unit (m) :: height
  real height(n)
  real gravity(n)

  != unit (Flibs m**2 / s**2) :: potential_energy
  real :: potential_energy

  mass(1)    = 3.00
  gravity(1) = .81
  height(1) = 4.20


  potential_energy = mass(1) * gravity(1) * height(1)
end program energy
