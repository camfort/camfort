program example
    integer(kind=4) :: a, b
    integer(kind=8) :: c
    real(kind=8) :: r1
    real(kind=16) :: r2
    character :: x(4), y(4)

    equivalence (a, b)        ! safe
    equivalence (c, r1)       ! safe
    equivalence (x, y)        ! safe
    equivalence (a, c)        ! not safe
    equivalence (a, x(1))     ! not safe
    equivalence (a, x)        ! not safe
    equivalence (r1, r2)      ! not safe
end program example
