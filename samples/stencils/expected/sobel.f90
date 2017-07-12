program sten
  
real (kind=8), dimension(0:imax,1:jmax) :: a
real (kind=8), dimension(0:imax,1:jmax) :: b
real gx, gy

do j=1, jmax-1
   do i=1, imax-1

      gx = -a(i-1,j-1) + a(i+1,j-1)  &
      &  + -2*a(i-1,j) + 2*a(i+1, j) &
      & - a(i-1,j+1) + a(i+1,j+1)

      gy = -a(i-1,j-1) - 2*a(i,j-1) &
        & - a(i+1,j-1)              &
        & + a(i-1,j+1) + 2*a(i,j+1) + a(i+1,j+1)

     != stencil centered(depth=1, dim=1, nonpointed)*centered(depth=1, dim=2) + centered(depth=1, dim=1)*centered(depth=1, dim=2, nonpointed) :: a
     b(i,j) = sqrt(gx*gx+gy*gy)
   enddo
enddo

end program
