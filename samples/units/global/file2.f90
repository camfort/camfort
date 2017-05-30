program file2
  implicit none

  use file1

  != unit(m) :: dist2
  real :: dist2 = 20

  != unit(m) :: dist3
  real :: dist3

  != unit(s) :: time2
  real :: time2

  ! Should be consistent
  dist2 = dist1 + dist2
  
  ! Should be inconsistent
  dist2 = time1 + dist2

  ! Should be inconsistent
  dist3 = time2 + dist3
  
end program file2
