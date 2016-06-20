program CrankNicolson
  implicit none

  integer :: i, j
  integer, parameter :: imax = 3, jmax = 3
  
  real a(0:imax,0:jmax), b(0:imax,0:jmax)

  ! some kind of setup
  do i = 0, imax
     do j = 0, jmax
        a(i,j) = i+j
     end do
  end do
  !do i = 1, (imax-1)
  !   do j = 1, (jmax-1)
  !      a(i, j) = i*j
  !   end do
  !end do   
  
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
        b(i, j) = a(i, j) 
end program
