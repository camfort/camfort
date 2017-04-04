program foo
  type xtype
    real :: val
  end type xtype
  type(xtype), dimension(10) :: a, b
  
  integer :: i

  do i=1,10
    a(i)%val = a(i)%val
  end do
  do i=1,10
    b(i) = a(i)
  end do
end program foo
