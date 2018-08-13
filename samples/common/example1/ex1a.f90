! camfort common ex1a.f90 out1a.f90
! camfort common ex1b.f90 out1b.f90

! gfortran -c Common.f90
! gfortran -o ex1 out1a.f90 out1b.f90 Common.f90

program ex1a
  real x, y
  common x, y

  call s()

  print *, x, y
end program ex1a
