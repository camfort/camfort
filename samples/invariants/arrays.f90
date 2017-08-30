
!= static_assert post("x == y")
subroutine simple_arrays(x)
  implicit none

  integer :: x
  integer :: y

  integer, dimension(8) :: a

  a(0) = x
  a(1) = a(0)

  y = a(1)

end subroutine simple_arrays


!= static_assert post("x == y")
subroutine loop_arrays(x)
  implicit none

  integer :: x
  integer :: y
  integer :: i

  integer, dimension(8) :: a

  i = 1
  a(i) = x

  != static_assert seq("i == 1" & "a(i) == x")
  do while (i < 8)
     != static_assert invariant("i <= 8" & "a(i) == x")
     a(i + 1) = a(i)
     i = i + 1
  end do

  y = a(8)

end subroutine loop_arrays
