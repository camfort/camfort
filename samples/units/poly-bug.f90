! Test via:
!   ./camfort units samples/units/poly-bug.f90 samples/unitso 

program foo 
  implicit none
  
  real, unit(m) :: a = 10
  real, unit(s) :: b = 10
  real :: c, d

  c = square(a)
  d = square(b)
  
  contains

   real function square(x)
     real x
     ! A bad version of square defined as follows fails as expected : 
     !
     ! real :: k = 1
     ! square = (x * x) + k
     ! 
     ! But if we substituted the constant it does not fail. This is bad.

     square = (x * x) + 1

     ! This is regardless of whether you ask literals to be treated Unitless/Poly/ or Mixed mode.

   end function

end program
