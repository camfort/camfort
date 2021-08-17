subroutine test(a,b, c)

integer :: c1, c2, c3
COMMON /cmn/  c1, c2, c3

integer :: c4
real :: c5
COMMON /cmnTwo/  c4, c5

real :: a, b, c

print *, a, b, c

end subroutine test
