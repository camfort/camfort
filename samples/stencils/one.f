      program one
      implicit none

      integer i, j, imax, jmax
      parameter (imax = 3, jmax = 3)

      real a(0:imax,0:jmax), b(0:imax,0:jmax), c

! some kind of setup
      do 1 i = 0, imax
         do 2 j = 0, jmax
            a(i,j) = i+j
 2       continue
 1    continue

!do i = 1, (imax-1)
!   do j = 1, (jmax-1)
!      a(i, j) = i*j
!   end do
!end do

      write (*,*) 'A = '
      do 3 i = 0, imax
         do 4 j = 0, jmax
            write (*,'(" ",f0.2," ")') a(i,j)
 4       continue
         write (*,*) '\n'
 3    continue

! compute mean
      do 5 i = 1, (imax-1)
         do 6 j = 1, (jmax-1)
            c = (a(i-1,j) + a(i,j) + a(i+1,j))
            b(i,j) = (c + a(i,j-1) + a(i,j+1)) / 5.0
 6       continue
! top and bottom (inner) edges
         b(i, 0) = (a(i, 0) + a(i-1,0) + a(i+1,0) + a(i,1))/4.0
         b(i, jmax) = (a(i, jmax) + a(i-1, jmax) + a(i+1, jmax) +
     .        a(i,jmax-1))/4.0
 5    continue

! left and right (inner) edges
      do 7 j = 1, (jmax-1)
         b(0, j) = (a(0, j) + a(0,j-1) + a(0,j+1) + a(1,j))/4.0
         b(imax, j) = (a(imax, j) + a(imax, j-1) + a(imax, j+1) +
     .        a(imax-1,j))/4.0
 7    continue

! corners
      b(0,0) = (a(0,0) + a(0,1) + a(1,0))/3.0
      b(imax,jmax) = (a(imax,jmax) + a(imax-1,jmax) +
     .     a(imax,jmax-1))/3.0
      b(imax,0) = (a(imax, 0) + a(imax-1,0) + a(imax,1))/3.0
      b(0, jmax) = (a(0, jmax) + a(1, jmax) + a(0, jmax-1))/3.0

! output routine
      write (*,*) 'B = '
      do 8 i = 0, imax
         do 9 j = 0, jmax
            write (*,'(" ",f0.2," ")') b(i,j)
 9       continue
         write (*,*) '\n'
 8       continue

      end
