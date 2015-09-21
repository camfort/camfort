! Run (in the top-directory): 
! 
!   ./camfort units samples/units/polymorphic_zero.f90 samples/unitso 


program foo 
  implicit none
  
  real, unit(m) :: a1, a2 = 10
  real, unit(s) :: b1, b2 = 10
  real :: c, d, e, f

  c = abs(a1)
  d = abs(b1)

  e = bad_abs(a2)
  f = bad_abs(b2)

  contains


  ! We want this to fine: 0 is polymorphic.
  real function abs(x)
    real x
    if (x < 0) then 
       abs = 0-x
    else
       abs = x
    endif 
 end function

 ! This should not be well-typed. 
 real function bad_abs(x)
    real x
    if (x < 1) then 
       bad_abs = 1-x
    else
       bad_abs = x
    endif 
 end function


end program
