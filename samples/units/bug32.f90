! Demonstrates parametric polymorphism through functions-calling-functions.
program squarePoly
  implicit none
  real :: x
  real :: y

  != unit(m) :: a
  real :: a
  != unit(s) :: b
  real :: b

  x = squareP(a)
  y = squareP(x)

  contains

  real function square(x)
    real :: x
    square = x * x
  end function

  real function squareP(x)
    real :: x
    squareP = square(x)
  end function

end program
