program simple
  implicit none

  real, unit(s) :: t
  real, unit(m/s):: v
  real :: x, s, a, b

  x = 20.0
  t = 3.0 
  v = x / t
  s = abss(v)


  a = square (x) 
  b = square (t)
  
  print *, s
  print *, v
  print *, a

  contains 

  real function square(y)
    real y
    real, unit(m) :: k
    square = y * y * k
  end function

  ! fun abs x = if x < zero then zero-x else x
  real function abss(x) 
    real x
    if (x < 0) then 
      abss = 0 - x
    else
      abss = x
    end if
  end function

end program
