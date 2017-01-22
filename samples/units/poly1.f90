module poly1
contains
  real function foo(q)
    != unit m :: q
    ! unit for r is determined through constraint-solving
    real :: q, r
    foo = sqr(r) + sqr(q)
  end function foo

  function mul(a,b)
    != unit 'a 'b :: mul
    != unit 'a :: a
    != unit 'b :: b
    real :: a, b, mul
    mul = a * b
  end function mul

  function sqr(x)
    != unit 'a ** 2 :: sqr
    != unit 'a :: x
    real :: x, y, sqr, z

    y = mul(x,x)
    sqr = y
  end function sqr
end module poly1
