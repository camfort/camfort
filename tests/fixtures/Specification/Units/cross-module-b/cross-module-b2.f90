module b2
  use b1
contains
  subroutine s()
    != unit c :: foo
    real, parameter :: foo = 273.0
    != unit c :: tc
    != unit k :: t
    real :: tc, t

    tc = k2c*t - foo
  end subroutine s
end module b2
