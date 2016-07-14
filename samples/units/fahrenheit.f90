program fahrenheit
  != unit(F) :: f
  != unit(C) :: c
  real :: f, s, c, a
  s = 1.8
  a = 32.0
  read (*,*) c
  f = s * c + a
  write (*,*) f
end program
