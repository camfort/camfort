program ex2a
  real x(2:10), y
  common /shared/ x, y

  call s()

  print *, x, y
end program ex2a
