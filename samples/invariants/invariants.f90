real function example(x)
  != static_assert pre(T)
  != static_assert post(x = r + y * q && r < y)

  implicit none

  real :: x, y
  integer :: q
  real :: r

  r = x
  q = 0
  != static_assert seq(r = x && q = 0)
  do while (y <= r)
     != static_assert invariant(r + y * q)
     r = r - y
     q = q + 1
  end do

  example = r
end function example
