program nested
  implicit none
    
contains

  real function example() 
    real a(1:10,1:10), b(1:10, 1:10)
    real :: x, example
    integer :: i, j

    if (.true.) then
       do i = 1, 10
          do j = 1, 10
             x = max(abs(a(i)), x)
       end do
    end do
    x = x
    else
    end if
    example = a
  end function example

end program nested
