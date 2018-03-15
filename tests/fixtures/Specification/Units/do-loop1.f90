program doLoop1
  != unit m :: x
  real :: x, y = 0
  integer :: i
  do i=0,x
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
end program doLoop1
