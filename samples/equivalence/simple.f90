program example
    integer(kind=4) :: a, b
    character :: x(4)

    equivalence (a, b)      ! endian-portable
    equivalence (a, x)      ! not endian-portable
end program example