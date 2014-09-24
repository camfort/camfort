program test
  implicit none
     
  integer :: n
  read *, n
  if (n < 3) then 
     print *, "n is small"
  end if

  if (mod(n, 2) == 0) then
     print *, "n is even"
  else
     print *, "n is odd"
  end if

end program test
