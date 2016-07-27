real function mul(a,b)
  real :: a, b
  mul = a * b
end function mul

real function sqr(x)
  != unit m :: x
  real :: x, y
  
  y = mul(x,x)
  sqr = y
end function sqr

program main
  real :: a
  a = sqr(2)
end program main
