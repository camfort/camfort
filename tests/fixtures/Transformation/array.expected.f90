program arrays

  integer, parameter :: n = 5
  integer, parameter :: m = 7
  integer, dimension(m,n) :: a
  integer, dimension(n,m) :: b
  integer :: i, j
  
  do i = 1, n
     do j = 1, m
        a(j,i) = i*j
     end do
  end do

  b = addAndZero(n, m, a, b)

  do i = 1, n
     do j = 1, m
        write (*, "(I3) ", advance="no") a(j, i)
     end do
     write (*,*) ""
  end do

  write (*,*) ""
  
  do i = 1, n
     do j = 1, m
        write (*, "(I3) ", advance="no") b(i, j)
     end do
     write (*,*) ""
  end do
  
  contains

    function addAndZero(n, m, c, d)
      integer :: n, m
      integer, dimension(m, n) :: c
      integer, dimension(n, m) :: d
      integer, dimension(n, m) :: addAndZero
      integer :: i, j
      do i = 1, n
         do j = 1, m
            addAndZero(i, j) = c(j, i) + d(i, j)
            c(j, i) = 0
         end do
      end do
    end function addAndZero
    
end program arrays
