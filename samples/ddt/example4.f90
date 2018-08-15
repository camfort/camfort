! test
program tmp
  integer, parameter :: x = 1
  integer, parameter :: y = 2
  integer, parameter :: z = 3
  != ddt*  foo(1=>foo1, 2=>foo2, 3=>foo3) :: d(dim=2)
  != ddt*  bar(1=>foo1, 2=>foo2, 3=>foo3) :: d(dim=1)
  real, dimension(3,3) :: d=1, e
  real :: sum
  integer :: i

  sum = d(x,z) + d(y,y) + d(z,x)
  print *, sum
end program
