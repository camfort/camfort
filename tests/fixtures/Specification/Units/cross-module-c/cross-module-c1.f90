module c1
  implicit none
  ! some constant

    real ::     a = 1.0
  ! another constant
  real :: b = 2.0
  contains
  ! function with another designated variable for the result
  function foo2() result(foo_out)
    real :: foo_out
    real :: c = 4.0
    real :: d = 4.0
    foo_out = c * d
  end function foo2
  ! function whose result is function name
  function foo3()
    real :: foo3
    real :: c = 4.0
    foo3 = c
  end function foo3
  ! subroutine test
  subroutine foo4(x)
      real :: x
    real :: c = 4.0
    x = c
  end subroutine foo4
  ! function whose parameter needs to get constraint somewhere else
  function foo5(x)
    real :: foo5
    real :: x
    foo5 = 1.0
  end function foo5
end module c1
