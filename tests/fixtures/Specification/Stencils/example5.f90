program example5
  implicit none

  integer :: i
  integer, parameter :: imax = 3
  real :: a(0:imax)

  do i = 0, imax
      a(i) = a(i) + a(i+2)
  end do
end program
