program litmus_equivalence_endian
    implicit none(type, external)

    ! Declares a single 32-bit integer and a 4-byte character array that are aliased
    integer(kind=4) :: integer_bytes
    character(len=1)  :: char_bytes(4)
    equivalence (integer_bytes, char_bytes)

    ! 0x075BCD15 = bytes 15, 205, 91, 7 in hex
    integer_bytes = 123456789

    write(*,*) 'Integer value set to:', integer_bytes
    write(*,*) 'Bytes (decimal) in EQUIVALENCE (chunked view):'
    write(*,'(4(1X,Z2))') ichar(char_bytes(1)), ichar(char_bytes(2)), &
                          ichar(char_bytes(3)), ichar(char_bytes(4))

    ! Test whether the character bytes match expected little-endian ordering
    if (char_bytes(1) == char(int(z'15')) .and. &
        char_bytes(2) == char(int(z'CD')) .and. &
        char_bytes(3) == char(int(z'5B')) .and. &
        char_bytes(4) == char(int(z'07'))) then
      write(*,*) 'Detected ordering consistent with little-endian LSB-first.'
    else
      write(*,*) 'Detected ordering consistent with big-endian or different layout.'
    end if
end program