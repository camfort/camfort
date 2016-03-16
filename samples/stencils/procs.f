!     Stencil spec inference expectation         
!     a : centered depth=1 dim=0,1
!     b : backward depth=1{a} dim=t
!      
! [or possibly  a : backward depth=1{b} dim=t]

      program procs
      implicit none

      integer i, j, t, imax, jmax, tmax
      parameter (imax = 3, jmax = 3, tmax = 3)
      
      real a(0:imax,0:jmax), b(0:imax,0:jmax)
      
! some kind of setup
      do 1 i = 0, imax
         do 2 j = 0, jmax
            a(i,j) = i*j
 2       continue
 1    continue
      
! main part
      do 3 t = 0, tmax
         call stencl(a, b, imax, jmax)
         a = b

!     output routine           
         write (*,*) 'A = '
         do 4 i = 0, imax
            do 5 j = 0, jmax
               write (*,'(" ",f0.2," ",$)') a(i,j)
 5          continue
            write (*,*) '\n'
 4       continue
 3    continue
         end

! subroutine to do the actual stencil part (flow depenency b <- a)
      subroutine stencl(a, b, imax, jmax)
       integer i, j
       real a(0:imax,0:jmax), b(0:imax,0:jmax)
       do 1 i = 1, (imax-1)
          do 2 j = 1, (jmax-1)
             b(i,j) = (a(i,j) + a(i+1,j) 
     &                 + a(i-1,j) + a(i,j+1) + a(i,j-1))/4.0
 2        continue
 1     continue
      end
