program example1 
  implicit none

  real :: d
  real :: x1
  real :: x2
  real :: t1
  real :: t2
  real :: v1, v2, s1, s2, x
  equivalence (v1, s1)
  equivalence (v2, s2)

  common x1, x2


  d = 1.0
  x1 = 0.0
  x2 = 0.4
  t1 = 20.0
  t2 = 10.0

  
  v1 = (d - x1) / t1
  v2 = (d - x2) / t2

  call intersect (s1, s2, x)
      
  !write (*,'(F8.3, A, F8.3, A, F8.3)'), v1, ' ', v2, ' ', x

  contains

   subroutine intersect(s1, s2, x)
      real :: s1, s2
      real :: y1
      real :: y2
      real :: t_i, x



      common y1, y2

      t_i = (y1 - y2)/(s2 - s1)
      
      x = s1 * t_i
   end subroutine

end program
