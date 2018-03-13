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
    real :: x, i = 0, j = 1
    sqr = 0
1   if (i < x) then
       sqr = sqr + x
       i = i + j ! not allowed under monomorphism restriction
       goto 1
    end if
    ! sqr is now equal to x * x
  end function sqr
end program literalnonzero


! with monomorphism restriction disabled:
!
! tests/fixtures/Specification/Units/literal-nonzero-inconsist5.f90:
!   4:11 unit m :: a
!   4:14 unit s :: b
!   12:3 unit 'a :: sqr
!   13:13 unit 'a :: x
!   13:16 unit 'a :: i
!   13:23 unit 'a :: j
!
! with it enabled (normal):
! tests/fixtures/Specification/Units/literal-nonzero-inconsist5.f90: Inconsistent:
!  - at 7:11: 'parameter 1 to sqr' should have unit 's'
!  - at 15:9: 'i' should have the same units as 'parameter 1 to sqr'
!  - at 17:12: 'i' should have unit '1'
