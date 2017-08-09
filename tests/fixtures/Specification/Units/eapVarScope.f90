module eapVarScope
contains
  function f(x)
    != unit 'a :: x
    real :: x, k, f
    k = g(x) * g(x * x)
    f = k
  end function f
  function g(y)
    != unit 'a :: y
    real :: y, j, g
    j = y
    g = j
  end function g
end module eapVarScope
