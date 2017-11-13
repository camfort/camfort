module complexity
contains
  real function g(x, y)
    real :: x, y, z
    g = (x*x*x*x*x*x + y*y*y*y)
  end function g
end module complexity
