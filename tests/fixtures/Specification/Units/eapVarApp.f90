module eapVarApp
contains
  function f(fx)
    != unit 'a :: fx
    real :: fx, fj, fk, fl, f
    fj = fx
    fk = g(fj*fj)
    fl = fj * g(fj * fj * fj)
    f = fk
  end function f
  function g(gx)
    != unit 'b :: gx
    real :: gx, gn, gm, g
    gm = gx
    gn = gm
    g = gn
  end function g
  function h(hx)
    != unit m :: hx
    real :: hx, h, hy
    hy = f(hx)
    h = hy
  end function h
end module eapVarApp
