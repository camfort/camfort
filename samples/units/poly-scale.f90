! Demonstrates the units of a scaling function. doubler is polymorphic
! in its units (it takes an 'a and gives an 'a).

! > camfort units-infer poly-scale.f90
! Inferring units for 'poly-scale.f90'
! Finished running unit inference on input 'poly-scale.f90' ...
! Logs:
!
! Result... OK:
! 
! poly-scale.f90:
!   24:11 unit s :: t
!   29:1 unit 'a :: doubler
!   30:10 unit 'a :: x
!   31:13 unit 1 :: k
!
! Note: how doubler takes an x of unit 'a and returns an 'a. 
!
! Note: k is a literal in a polymorphic context and camfort assumes it
! to be unitless

program main
  != unit(s) :: t
  real :: t
  t = 1
  t = doubler(t)
end program main

real function doubler(x) 
    real x
    real :: k = 2
    doubler = k*x
end function
