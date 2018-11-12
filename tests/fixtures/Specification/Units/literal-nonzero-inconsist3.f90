program literalnonzero
  != unit m :: a
  != unit s :: b
  real :: a, b
  a = 2
  a = sqr(a)
  b = sqr(b)

contains
  ! without monomorphism restriction I could write a 'squaring'
  ! function that the units inference system wouldn't recognise: 'a -> 'a
  real function sqr(x)
    real :: x, i
    sqr = 0
    do i=1, x ! disallowed under monomorphism restriction
       sqr = sqr + x
    end do
    ! sqr is now equal to x * x
  end function sqr
end program literalnonzero


! with monomorphism restriction disabled:
!
! tests/fixtures/Specification/Units/literal-nonzero-inconsist3.f90:
!   4:11 unit m :: a
!   4:14 unit s :: b
!   12:3 unit 'a :: sqr
!   13:13 unit 'a :: x
!   13:16 unit 'a :: i
!
! with it enabled (normal):
! tests/fixtures/Specification/Units/literal-nonzero-inconsist3.f90: Inconsistent:
! - at 6:3: 'result of sqr' should have unit 'm'
! - at 6:11: 'parameter 1 to sqr' should have unit 'm'
! - at 7:3: 'result of sqr' should have unit 's'
! - at 7:11: 'parameter 1 to sqr' should have unit 's'
