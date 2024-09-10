program intconv
  implicit none
  real :: x = 1.0
  real :: y = (1.0/3.0) * 3.0
  if (nint(x) == nint(y)) then
    print *, "x and y are equal"
  end if
end program intconv
