c     Stencil spec inference expectation
c     a : centered depth=1 dim=0,1
c     b : backward depth=1{a} dim=t
c
c     [or possibly  a : backward depth=1{b} dim=t]

      program procs
      implicit none

      integer i, j, t, imax, jmax, tmax
      parameter (imax = 3, jmax = 3, tmax = 3)

      real a(0:imax,0:jmax), b(0:imax,0:jmax)

c     some kind of setup
      do 1 i = 0, imax
         do 2 j = 0, jmax
            a(i,j) = i*j
            b(i,j) = 0
 2       continue
 1    continue

c     main part
      do 3 t = 0, tmax

c     output routine
         write (*,*) 'A = '
         do 4 i = 0, imax
            do 5 j = 0, jmax
               write (*,'(" ",f0.2," ",$)') a(i,j)
 5          continue
            write (*,'(/)')
 4       continue
         call stencl(a, b, imax, jmax)
         a = b

 3    continue
      end

c     subroutine to do the actual stencil part (flow depenency b <- a)
      subroutine stencl(a, b, imax, jmax)
      integer i, j
      real a(0:imax,0:jmax), b(0:imax,0:jmax)
      do 1 i = 1, (imax-1)
         do 2 j = 1, (jmax-1)
            b(i,j) = (a(i,j) + a(i+1,j)
     &           + a(i-1,j) + a(i,j+1) + a(i,j-1))/4.0
 2       continue
 1    continue
      end
