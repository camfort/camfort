program transfer
  implicit none
  != unit m :: x
  real :: x
  != unit s :: y
  real :: y
  x = x + transfer(x, y)
end program transfer
