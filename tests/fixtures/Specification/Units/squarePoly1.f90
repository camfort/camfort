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
  y = squareP(b)
  contains
  real function square(n)
    real :: n
    square = n * n
  end function
  real function squareP(m)
    real :: m
    squareP = square(m)
  end function
end program
