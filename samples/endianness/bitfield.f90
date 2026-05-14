! An integer is being used as a bitfield, then the integer is being compared to numbers to check which bits are set.
program bit_tests
    integer*2 bitmap, expected
    character*2 bytes
    equivalence(bitmap, bytes)

    ! Set up integer*2 as a bit map (that is endian-independent)
    bytes(1:1) = char(20)
    bytes(2:2) = char(0)

    ! Problem: code comparing with integer values to check if bits are set
    if (bitmap .eq. 20) then
        print *, 'Bits 2,4 set (little-endian)'
    else
        print *, 'Bits 2,4 NOT set (big-endian)'
    endif

    ! Solution: create an integer, set the physical bytes, compare with this
    expected = 0_2
    call set_physical_bit_c(expected, 2)    ! This is defined in bifield.c
    call set_physical_bit_c(expected, 4)
    if (bitmap .eq. expected) then
        print *, 'Bits 2,4 set (endian-independent)'
    else
        print *, 'ERROR: Values do not match'
    endif
end program bit_tests