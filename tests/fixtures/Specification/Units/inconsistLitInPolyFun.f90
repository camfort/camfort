! This should be inconsistent because of the use of the literal "10"
! in the parametric polymorphic function sqr.
program example
  implicit none
  real :: a, b
  != unit(m) :: x
  real :: x = 1
  != unit(s) :: t
  real :: t = 2
  a = sqr(x)
  b = sqr(t)
  contains
  real function sqr(y)
    real :: y
    real :: z = 10
    sqr = y * y + z
  end function
end program example
