! Test showing the effects of endianness on structures
program test
    structure / s /
        union
        map
            character*12 char_array
        end map
        map
            integer*4 i4_array(3)
        end map
        end union
    end structure 
    integer*4 i

    record / s / r
    r.i4_array(1) = 1684234849
    r.i4_array(2) = 1751606885
    r.i4_array(3) = 1818978921
    ! "abcdefghij" on big-endian
    do i = 1,9, i+2
        write(*,*) r.char_array(i:i+1)
    end do
end program test