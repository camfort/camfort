program nested
  implicit none
  real a(10)
  real :: x
  integer :: i

  do i = 1, 10
      != access readOnce, (pointed(dim=1)) :: a
      x = a(i)
  end do
  
  contains

  real function example() 
    real a(10)
    real :: x, example
    integer :: i

    do i = 1, 10
       != access readOnce, (pointed(dim=1)) :: a
       x = a(i)
    end do
    example = x
  end function example
end program nested
