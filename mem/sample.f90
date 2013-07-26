program Mem

integer :: la, lb, lc, ld, le, lf
real c(0:5)

c(la) = c(lb)
c(lb) = 1
c(ld) = 2
c(lf) = 3
c(lc) = 4
c(le) = 5

call foo(c(la), c(lb))
call foo2(c(ld), c(lf), lb, la)
call foo3(c(lc), c(le))

contains

subroutine foo(a, b)
  integer :: a, b
  a = a + b
end subroutine

subroutine foo2(d, f, b, a)
  integer :: d, f, b, a
end subroutine

subroutine foo3(c, e)
  integer :: c, e
end subroutine


end program Mem
