program byte_sign_test
    integer*2 bitmap, expected
    character*2 bytes
    equivalence(bitmap, bytes)

    ! Set up integer*2 as bitmap
    bytes(1:1) = char(200)
    bytes(2:2) = char(0)

    ! Problem: Using < to check sign bit
    if (bitmap .lt. 0) then
        print *, 'Negative (big-endian)'
    else
        print *, 'Positive (little-endian)'
    endif

    ! Solution: Compare to physical bit
    expected = 0_2
    call set_physical_bit_c(expected, 7)
    if (iand(bitmap, expected) .ne. 0) then
        print *, 'First byte high bit set (endian independent)'
    else
        print *, 'ERROR: first bit not set'
    endif

end program byte_sign_test