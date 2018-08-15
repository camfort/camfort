! test
program tmp
  integer, parameter :: x = 1
  integer, parameter :: y = 2
  integer, parameter :: z = 3
  != ddt*  foo(1=>foo1, 2=>foo2, 3=>foo3) :: d(dim=2)
  != ddt*  bar(1=>foo1, 2=>foo2, 3=>foo3) :: e(dim=1)
  real, dimension(3,3) :: d=1, e=2
  real :: sum1, sum2
  integer :: i

  do i=1,3
     sum1 = d(i,x) + d(i,y) + d(i,z)+0
     sum2 = e(x,i) + e(y,i) + e(z,i)+0
  end do
  print *, sum1, sum2
end program
