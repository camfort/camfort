program jacobi
  use ppm
  implicit none
  real :: a(100,100)
  integer :: i, j, n

  do i=1,100
     do j=1,100
        if (i == 1 .or. i == 100) then
           a(i,j) = 1
        else
           a(i,j) = 0
        end if
     end do
  end do

  do n=1,1000
     do i=2,99
        do j=2,99
           ! simple up-down-left-right stencil

           !=region upDownLeftRight = reflexive(dim=1)*centered(depth=1, dim=2, irreflexive) + reflexive(dim=2)*centered(depth=1, dim=1, irreflexive)

           !=stencil readOnce, upDownLeftRight :: a
           a(i,j) = (a(i-1,j+0) + a(i+0,j-1) + a(i+1,j+0) + a(i+0,j+1))/4
        end do
     end do
  end do

  call output_ppm(6, a)

end program jacobi
