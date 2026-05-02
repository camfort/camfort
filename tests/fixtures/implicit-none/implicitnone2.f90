module implicitnone1
  real :: x
  implicit none
  real :: x
  real :: y
  y = f(x)
contains
  real function f(x)
    real :: x
    f = x
  end function f
end module implicitnone1

module implicitnone2
  implicit none
  use foo
  real :: x
  real :: y
  y = f(x)
contains
  real function f(x)
    real :: x
    f = x
  end function f
end module implicitnone2
