!= static_assert pre(T)
!= static_assert post("x" = "r + y * q" & "r" < "y")
subroutine example(x, y, q, r)

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

!= static_assert post(("x" <= "y" -> "min" = "x") & ("y" <= "x" -> "min" = "y"))
integer function min(x, y)
  implicit none

  integer :: x, y

  if (x <= y) then
    min = x
  else
    min = y
  end if

end function min

!= static_assert post("multiply" = "x * y")
integer function multiply(x, y)
  implicit none

  integer :: x, y
  integer :: r, n, d, l

  if (x >= 0) then
     d = y
     l = x
  else
     ! unary negation isn't supported yet
     d = 0 - y
     l = 0 - x
  end if

  r = 0
  n = 0
  != static_assert seq("d * l" = "x * y" & "n" = "0" & "r" = "0" & "n" <= "l")
  do while (n < l)
     != static_assert invariant("l * d" = "x * y" & "r" = "n * d" & "n" <= "l")
     r = r + d
     n = n + 1
  end do

  multiply = r
end function multiply
