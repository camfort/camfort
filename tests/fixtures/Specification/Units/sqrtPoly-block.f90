program sqrtPoly
  implicit none
  != unit m :: x
  != unit s :: y  
  != unit J :: z
  real :: x
  real :: y
  real :: z
  integer :: a
  integer :: b
  integer :: c
  x = sqrt(a)
  y = sqrt(sqrt(b))
  z = sqrt(square(sqrt(c)))
contains
  real function square(n)
    real :: n
    square = n * n
  end function square
end program sqrtPoly
