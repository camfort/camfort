program one
  implicit none

  integer :: i, j
  integer, parameter :: imax = 3, jmax = 3

  real :: e
  real, dimension(0:imax, 0:jmax) :: a, b

  ! Setup some region specs for the file

  != region :: r1 = centered (depth = 1, dim = 1)
  != region :: r2 = centered (depth = 1, dim = 2)

  ! some kind of setup
  do i = 0, imax
     do j = 0, jmax
        a(i,j) = i+j
     end do
  end do

  write (*,*) "A = "
  do i = 0, imax
     do j = 0, jmax
        write (*,'(" ",f0.2," ")',advance="no") a(i,j)
     end do
     write (*,*) "\n"
  end do

  ! compute mean
  do i = 1, (imax-1)
     do j = 1, (jmax-1)

        != stencil readOnce, pointed(dim=1) * r1 * r2 + r1 * pointed(dim=2) :: a

        != stencil readOnce, pointed(dim=1)*centered(depth=1, dim=2) + centered(depth=1, dim=1)*pointed(dim=2) :: a
        b(i,j) = (a(i-1,j) + a(i,j) + a(i+1,j) + &
                  a(i,j-1) + a(i,j+1)) / 5.0
     end do
     ! top and bottom (inner) edges

     != stencil readOnce, r1 :: a
     b(i, 0) = (a(i, 0) + a(i-1,0) + a(i+1,0) + a(i,1))/4.0

     != stencil readOnce, r1 :: a
     b(i, jmax) = (a(i, jmax) + a(i-1, jmax) + a(i+1, jmax) + a(i,jmax-1))/4.0
  end do

  ! left and right (inner) edges
  do j = 1, (jmax-1)
     b(0, j) = (a(0, 0) + a(0,j-1) + a(0,j+1) + a(1,0))/4.0
     e = a(imax, j) + a(imax, j-1)
     != stencil readOnce, r2 :: a
     b(imax, j) = (e + a(imax, j+1) + a(imax-1,j))/4.0
  end do

  ! corners
  b(0,0) = (a(0,0) + a(0,1) + a(1,0))/3.0
  b(imax,jmax) = (a(imax,jmax) + a(imax-1,jmax) + a(imax,jmax-1))/3.0
  b(imax,0) = (a(imax, 0) + a(imax-1,0) + a(imax,1))/3.0
  b(0, jmax) = (a(0, jmax) + a(1, jmax) + a(0, jmax-1))/3.0

  ! output routine
  write (*,*) "B = "
  do i = 0, imax
     do j = 0, jmax
        write (*,'(" ",f0.2," ")',advance="no") b(i,j)
     end do
     write (*,*) "\n"
  end do

end program
