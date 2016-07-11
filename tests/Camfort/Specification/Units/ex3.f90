program energy

 != unit (m) :: height
 != unit (kg) :: mass
  real, parameter :: mass = 3.00, gravity = .81, height = 4.20

  integer :: i = 0
  integer :: j = i
  
 != unit (kg m** (2) / s** (2)) :: potential_energy
  real :: potential_energy
 
  potential_energy = mass * gravity * height
end program energy
