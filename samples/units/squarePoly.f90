program squarePoly
  implicit none

  real :: x
  real :: y
  != unit kg**2 :: z
  real :: z

  != unit(m) :: a
  real :: a
  != unit(s) :: b
  real :: b

  real :: c

  x = square(a)
  y = square(b)
  z = square(c)

contains

  real function square(x)
    real :: x
    square = x * x
  end function square

end program
