      program param
        implicit none
        != unit(m) :: x, y
        real, parameter :: x = 4, y = 5
        != unit(s) :: t
        real, parameter :: t = 2
        real :: v1, v2
        v1 = x / t
        v2 = y / t
      end program
