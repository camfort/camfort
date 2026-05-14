! Test showing the effects of endianness on strings equivalenced to integers
program test
    character*12 char_array
    integer*4 i
    integer*4 i4_array(3)/1684234849,1751606885,1818978921/ ! "abcdefghij" on big-endian
    equivalence(char_array, i4_array)
    do i = 1,9, i+2
        write(*,*) char_array(i:i+1)
    end do
end program test