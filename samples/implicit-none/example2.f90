! should pass 'implicit-none' test with no cases detected.
program example2
  implicit none
  print *, f(1)
contains
  integer function f(x)
    ! IMPLICIT NONE is not necessary here as it inherits the previous
    ! one, however we do provide 'implicit-none-all' functionality
    ! just in case you want to check all program units regardless of
    ! the inheritance rule.
    integer :: x
    f = 1 + x
  end function f
end program example2
