program foo 
  implicit none
  
  real, unit(m) :: a
  real, unit(s) :: b
  real :: c, d

  a = 10
  b = 10
  c = square(a)
  d = square(b)
  
  contains

   real function square(x)
     real x
     square = x * x
   end function

end program
