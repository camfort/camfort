program foo
  type xtype
    real :: val
  end type xtype
  type(xtype), dimension(10) :: a, b
  
  integer :: i

  do i=1,10
    != stencil readOnce, pointed(dim=1) :: a
    a(i)%val = a(i)%val
  end do
  do i=1,10
    != stencil readOnce, pointed(dim=1) :: a
    b(i) = a(i)
  end do
end program foo
