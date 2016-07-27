! Demonstrates parametric polymorphism through subroutines-calling-functions.
program squarePoly2
  implicit none
  real :: x
  real :: y

  != unit(m) :: a
  real :: a
  != unit(s) :: b
  real :: b

  call squareP(x, y, a, b)
  call squareP(y, x, b, a)

  contains

  real function square(x)
    real :: x
    square = x * x
  end function

  subroutine squareP(x,y,a,b)
    real :: x, y, a, b
    x = square(a)
    y = square(b)
  end subroutine squareP

end program squarePoly2
