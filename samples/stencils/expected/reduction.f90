program reduction
  implicit none
  real :: r, x, beta_mod, z, eps_E
  integer :: i, j
  real, dimension(10) :: a, b

  do i = 1, 10
     eps_E = toMask(iand(b(i+1), .true.))
     if (.true.) then
        if (.true.) then
        else if (.true.) then
               beta_mod = -6/((eps_E+1)*5+2*3)
               != stencil (pointed(dim=1)) :: b
               a(i) = (b(i) + beta_mod + eps_E*b(i)) / 2
               != access readOnce, (pointed(dim=1)) :: a
               z = a(i)
               r = max(r, z)
           end if
       end if
     end if
  end do

end program reduction

