subroutine s()
  real x(2:10), y
  common /shared/ x, y

  x = 1
  y = 2
end subroutine s
