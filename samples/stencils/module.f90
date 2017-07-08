module modtest
  implicit none

  contains

  real function example() 
    real a(10)
    real :: x, example
    integer :: i
    do i = 1, 10
       a(i) = a(i) + a(i+1)
    end do
    example = x
  end function example
end module modtest
