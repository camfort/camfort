program Mem

integer :: la, lb, lc, ld, le, lf
integer c(0:5)

call foo(c(la), c(lb), 0)
call foo2(c(ld), c(lf), la, lb)
call foo3(c(lc), c(le))

contains

subroutine foo(a, b)
  integer :: a, b
end subroutine

subroutine foo2(d, f)
  integer :: d, f
end subroutine

subroutine foo3(c, e)
  integer :: c, e
end subroutine


end program Mem
