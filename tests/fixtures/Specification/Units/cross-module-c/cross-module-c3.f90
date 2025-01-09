module c3
  use c1
  implicit none
  ! some constant
  real :: a3 = 1.0
  ! another constant
  !
  ! another constant
  real :: b3 = 2.0
  ! another global
  real :: x0
    real :: x1
  contains
  subroutine foo()
    x0 = foo5(x1)
    b3 = a / b + foo2()
  end subroutine foo
end module c3
