program foo
  implicit none

  CALL bar(10)

  contains

  subroutine bar(x)
    integer :: x
    integer :: i
    integer ::  j
    real a(0:x), b(0:x), c

    c = 0.1

    do i = 1, x
      c = c + 0.1
      j = j + 1
      a(i) = j 
      b(i) = a(i+1)
      print '(f0.7)', a(i+2)
      
    end do

    j = j + 1
    print '(f0.7)', c

  end subroutine

end program foo
