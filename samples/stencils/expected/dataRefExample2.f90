program foo
  type xtype
    real :: xval
  end type xtype
 
  ! arrays of data types
  type(xtype), dimension(10) :: a, b

  type ytype
    real, dimension(10) :: yval
  end type ytype

  ! data types containing arrays
  type(ytype) :: c, d

  ! arrays of data types containing arrays
  type(ytype), dimension(10) :: e, f
  
  integer :: i, j

  ! Normal array subscripts
  do i=1,10
      != stencil readOnce, (pointed(dim=1)) :: a
      b(i) = a(i)
  end do

  ! data ref on subscript 
  do i=1,10
    != stencil readOnce, (pointed(dim=1)) :: a
    a(i)%xval = a(i)%xval
  end do
 
  ! subscript on data ref
  do i=1,10
     != stencil readOnce, (pointed(dim=1)) :: yval
     c%yval(i) = d%yval(i)
  end do

  ! subscript on data ref on subscript
  do j=1,10
    do i=1,10
       != stencil readOnce, (pointed(dim=1)) :: f, yval
       e(i)%yval(i) = f(i)%yval(i)
    end do
  end do
 
end program foo
