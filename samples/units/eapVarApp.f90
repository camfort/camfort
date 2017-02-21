module eapVarApp
contains
  function f(x)
    != unit 'a :: x
    real :: x, j, k, l, f
    j = x
    k = g(j*j)
    l = j * g(j * j * j)
    f = k
  end function f
  function g(x)
    != unit 'b :: x
    real :: x, n, m, g
    m = x
    n = m
    g = n
  end function g
  function h(x)
    != unit m :: x
    real :: x, h, y
    y = f(x)
    h = y
  end function h
end module eapVarApp
