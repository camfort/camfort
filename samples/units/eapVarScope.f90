module eapVarScope
contains
  function f(x)
    != unit 'a :: x
    real :: x, k, f
    k = g(x) * g(x * x)
    f = k
  end function f
  function g(x)
    != unit 'a :: x
    real :: x, k, g
    k = x
    g = k
  end function g
end module eapVarScope

