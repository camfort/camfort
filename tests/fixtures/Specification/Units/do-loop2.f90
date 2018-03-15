program doLoop2
  != unit m :: x
  real :: x, y = 0,z
  integer :: i
  do i=0,x,2*3
     y = y + x
  end do

contains
  real function f(x)
    real :: x, y = 0
    integer :: i
    do i=0,x
       y = y + x
    end do
    f = y
  end function f

  real function g(x)
    real :: x, y = 0
    integer :: i
    do i=0,x,x/x
       y = y + x
    end do
    g = y
  end function g

  real function h(x)
    real :: x, y = 0
    integer :: i
    do i=0,x,x
       y = y + x
    end do
    h = y
  end function h
end program doLoop2
