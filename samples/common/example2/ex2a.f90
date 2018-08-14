! camfort common ex2a.f90 out2a.f90
! camfort common ex2b.f90 out2b.f90

! gfortran -c shared.f90
! gfortran -o ex1 out1a.f90 out1b.f90 shared.f90

program ex2a
  real x(2:10), y
  common /shared/ x, y

  call s()

  print *, x, y
end program ex2a
