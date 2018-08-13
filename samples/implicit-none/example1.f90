! should fail 'implicit-none' test: 1 case detected
! should fail 'implicit-none-all' test: 2 cases detected
program example1
  implicit none ! ERROR: IMPLICIT NONE must follow all USE statements

  use file1

  != unit(m) :: dist2
  real :: dist2 = 20

  != unit(m) :: dist3
  real :: dist3

  != unit(s) :: time2
  real :: time2

  ! Should be consistent
  dist2 = add(dist1, dist2)

  ! Should be inconsistent
  dist2 = add(time1, dist2)

  ! Should be inconsistent
  dist3 = add(time2, dist3)

contains
  real function add(a,b)
    real :: a, b
    add = a + b
  end function add
end program example1
