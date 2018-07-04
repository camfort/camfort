program example5a
  implicit none

  integer :: i
  integer, parameter :: imax = 3, x = (1+1)*imax-2*(1+1)
  real :: a(0:imax)

  do i = 0, imax
      != stencil readOnce, atLeast, pointed(dim=1) :: a
      != stencil readOnce, atMost, forward(depth=2, dim=1) :: a
      a(i) = a(i) + a(i+x)
  end do
end program
