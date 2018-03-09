program literalnonzero
  != unit m :: a
  real :: a, b
  a = 2
  b = f(a)

contains
  real function f(x)
    real :: x
    x = 1
    f = x
  end function f
end program literalnonzero
