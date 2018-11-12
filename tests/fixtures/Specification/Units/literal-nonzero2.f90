program literalnonzero
  != unit m :: a, c
  real :: a, b, c, d
  real, parameter :: n = 1.0
  a = 2.0
  b = f(a)
  c = 3.0
  d = f(c)
contains
  real function f(x)
    real :: x
    x = n
    f = x
  end function f
end program literalnonzero
