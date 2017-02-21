module mod3
contains
  function outside(x)
    != unit 'a :: x
    real :: x, k, m, outside
    k = x
    outside = inside(k) * 2
    m = outside
  contains
    function inside(y)
      != unit 'a ** 2 :: inside
      real :: y, inside
      inside = y * y
    end function inside
  end function outside
end module mod3

