!= static_assert pre(T)
!= static_assert post("x == r + y * q" & "r < y")
subroutine example(x, y, q, r)

  implicit none

  integer :: x, y
  integer :: q
  integer :: r

  r = x
  q = 0
  != static_assert seq("r == x" & "q == 0")
  do while (y <= r)
     != static_assert invariant("x == r + y * q")
     r = r - y
     q = q + 1
  end do
end subroutine example


!= static_assert post("x <= y" -> "min == x")
!= static_assert post("y <= x" -> "min == y")
integer (kind=8) function min(x, y)
  implicit none

  integer (kind=8) :: x, y

  if (x <= y) then
    min = x
  else
    min = y
  end if

end function min


!= static_assert post("x <= y" -> "minf == x")
!= static_assert post("y <= x" -> "minf == y")
real function minf(x, y)
  implicit none

  real :: x, y

  if (x <= y) then
     minf = x
  else
     minf = y
  end if

end function minf


!= decl_aux("integer" :: x_)
!= decl_aux("integer" :: y_)
!= static_assert pre("x == x_" & "y == y_")
!= static_assert post("multiply == x_ * y_")
integer function multiply(x, y)
  implicit none

  integer :: x, y
  integer :: r, n

  if (x < 0) then
     x = -x
     y = -y
  end if

  r = 0
  n = 0
  != static_assert seq("x * y == x_ * y_" & "n == 0" & "r == 0" & "n <= x")
  do while (n < x)
     != static_assert invariant("x * y == x_ * y_" & "r == n * y" & "n <= x")
     r = r + y
     n = n + 1
  end do

  multiply = r
end function multiply


!= static_assert pre("x >= 0")
!= static_assert post("2 * halve > x - 2")
!= static_assert post("2 * halve <= x")
integer function halve(x)
  implicit none

  integer :: x
  integer :: n, q

  n = 2
  q = 0

  != static_assert seq("n == 2" & "q == 0" & "n <= x + 2")
  do while (n <= x)
     != static_assert invariant("n <= x + 2" & "2 * q == n - 2")
     q = q + 1
     n = n + 2
  end do

  halve = q
end function halve


! Performs (positive) integer division, rounded down.
!= decl_aux("integer" :: x_)
!= decl_aux("integer" :: y_)
!= static_assert pre("x == x_" & "y == y_")
!= static_assert pre("x >= 0")
!= static_assert post("y_ * div > x_ - y_")
!= static_assert post("y_ * div <= x_")
integer function div(x, y)

  integer :: x, y
  integer :: n = y, q = 0

  != static_assert seq("x == x_" & "y == y_" & "n == y" & "q == 0" & "n <= x + y")
  do while (n <= x)
     != static_assert invariant("x == x_" & "y == y_" & "n <= x + y" & "y * q == n - y")
     q = q + 1
     n = n + y
  end do

  div = q

end function div


!= decl_aux("integer" :: x_)
!= decl_aux("real" :: y_)
!= static_assert pre("x == x_" & "y == y_")
!= static_assert post("multiplyf == x_ * y_")
real function multiplyf(x, y)
  implicit none

  integer :: x, n
  real :: y, r

  if (x < 0) then
     x = -x
     y = -y
  end if

  r = 0.0
  n = 0
  != static_assert seq("x * y == x_ * y_" & "n == 0" & "r == 0" & "n <= x")
  do while (n < x)
     != static_assert invariant("x * y == x_ * y_" & "r == n * y" & "n <= x")
     r = r + y
     n = n + 1
  end do

  multiplyf = r
end function multiplyf
