module c3
  use c1
  implicit none
  ! some constant
  real :: a3 = 1.0
  ! another constant
  !
  ! another constant
  real :: b3 = 2.0
  contains
  subroutine foo()
    b3 = a / b
  end subroutine foo
end module c3
