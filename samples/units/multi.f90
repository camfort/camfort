program multi
  implicit none
  != unit m :: height, width
  real :: height = 1.0
  real :: width

  ! Should cause a units error
  width = height * height

  print *, width
end program multi
