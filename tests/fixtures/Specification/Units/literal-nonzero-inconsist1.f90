program literalnonzero
  real :: a, b
  a = 2
  b = f(a)

contains
  ! unit 'a :: f
  real function f(x)
    != unit 'a :: x
    real :: x
    x = 1
    f = x
  end function f
end program literalnonzero
