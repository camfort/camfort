program nested
  implicit none
  real a(10)
  real :: x
  integer :: i

  do i = 1, 10
     if (.true.) then 
       x = a(i)
      else
       x = a(i+2)
     end if
     y = a(i+1) + x
     a(i) = y
  end do

  contains

  real function example() 
    real a(10)
    real :: x, example
    integer :: i

    do i = 1, 10
       x = a(i)
    end do
    example = x
  end function example
end program nested
