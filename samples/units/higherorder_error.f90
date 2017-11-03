program higherorder
  real :: a,b,c,d,e
  d = f(g,b,c)
  e = f(g,b,c)
contains
    ! need a mechanism to express that all uses of h need to have the same units

  real function f(h,x,y)
    != unit m :: x
    != unit s :: y

    real :: x, y
    real, external :: h
    f = h(x) + h(y)
  end function f

  real function g(y)
    real :: y
    g = y
  end function g
end program higherorder

