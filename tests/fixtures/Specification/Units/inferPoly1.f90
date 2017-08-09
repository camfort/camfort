module inferPoly1
contains
  function id(x1)
    real :: x1, id
    id = x1
  end function id
  function sqr(x2)
    real :: x2, sqr
    sqr = x2 * x2
  end function sqr
  function fst(x3,y3)
    real :: x3, y3, fst
    fst = x3
  end function fst
  function snd(x4,y4)
    real :: x4, y4, snd
    snd = y4
  end function snd
end module inferPoly1
