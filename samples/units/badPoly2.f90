! This should fail because f has explicitly polymorphic annotations on
! x and y but it invokes them with function g that has explicitly
! monomorphic annotations on its parameters.

module poly2

contains

  function h(x)
    != unit 'b :: x
    != unit 'b :: h
    real :: x, h
    h = x
  end function h

  function g(x)
    != unit m :: x
    != unit m :: g
    real :: x, g
    g = h(x)
  end function g

  function f(x)
    != unit 'a :: x
    != unit 'a :: y
    real :: x, y, f
    y = g(x)
    f = y
  end function f

end module
