program fahrenheit
  != unit(Fah) :: f
  != unit(Cel) :: c
  
  != unit(fah / cel) :: s
  != unit(fah) :: a
  real :: f, s, c, a
  s = 1.8
  a = 32.0
  
  read (*,*) c
  f = s * c + a + c
  write (*,*) f

end program
