program main
  != unit(m) :: y
  integer :: x = 5, y = 2, z
  z = recur(x,y)
  print *, y
contains
  real recursive function recur(n, b) result(r)
    integer :: n, b
    if (n .EQ. 0) then
       r = b
    else
       r = b + recur(n - 1, b)
    end if
  end function recur
end program main
