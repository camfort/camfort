program simple
  implicit none

  real, unit(m) :: x
  real :: y
  
  real, unit(q) :: a
  real :: b

  y = id(x)
  b = id(a)

  contains 

  real function id(y)
    real y
    id = y
  end function

    
end program
