module poly2

contains

  function g(x)
    != unit m :: x
    != unit m :: g
    real :: x, g
    g = x
  end function g

  function f(x)
    != unit 'a :: x
    != unit 'a :: y
    real :: x, y, f
    y = g(x)
    f = y
  end function f
  
end module
