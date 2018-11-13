program literalnonzero
  != unit m :: a
  != unit s :: c
  real :: a, b, c, d
  a = 2
  b = f(a)
  c = 3
  d = f(c)
contains
  real function f(x)
    real :: x
    x = 1
    f = x
  end function f
end program literalnonzero
