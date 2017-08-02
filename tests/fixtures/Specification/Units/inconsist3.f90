program example
  != unit(a) :: x
  real :: x, j, k

  j = 1 + 1
  k = j * j
  x = x + k
  x = x * j ! inconsistent
end program example
