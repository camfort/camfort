!= static_assert pre(T)
!= static_assert post("x" = "r + y * q" & "r" < "y")
subroutine example(x)

  implicit none

  integer :: x, y
  integer :: q
  integer :: r

  r = x
  q = 0
  != static_assert seq("r" = "x" & "q" = "0")
  do while (y <= r)
     != static_assert invariant("x" = "r + y * q")
     r = r - y
     q = q + 1
  end do
end subroutine example
