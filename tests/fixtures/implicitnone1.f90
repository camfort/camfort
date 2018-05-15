program implicitnone
  use foo
  ! test
  implicit none
  real :: x
  real :: y
  y = f(x)
contains
  real function f(x)
    real :: x
    f = x
  end function f
end program

subroutine s(x)
  real :: x
end subroutine s
