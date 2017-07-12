program funcExample
  implicit none
  integer :: i
  real, dimension(2) :: a, b
  real :: x
  do i = 1, 2
     x = foo(a(i+1) + a(i))
     != stencil readOnce, forward(depth=1, dim=1) :: a
     b(i) = x
 end do

contains

   real function foo(y)
    real :: y
    foo = y*y
  end function
end program
