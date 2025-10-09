program litmus_common_endian
  implicit none

  ! Define a COMMON block that holds 4 raw bytes (chunked view).
  integer(kind = 1) :: raw_bytes(4)
  common /overlay_raw/ raw_bytes

  integer(kind = 4) :: iv

  ! We'll transfer between iv and raw_bytes to demonstrate byte layout.
  iv = 123456789    ! 0x075BCD15

  ! Move iv's bit-pattern into the common raw_bytes array
  raw_bytes = transfer(iv, raw_bytes)

  write(*,*) 'Integer value set to:', iv
  write(*,*) 'Bytes in COMMON /overlay_raw/ (chunked view):'
  write(*,'(4(1X,Z2))') raw_bytes(1), raw_bytes(2), raw_bytes(3), raw_bytes(4)

  write(*,*) 'Bytes (hex):'
  call print_hex(raw_bytes)

  ! Now show the reverse: set the bytes and transfer back to an integer
  raw_bytes = (/ int(21, kind=1), int(-51, kind=1), int(91, kind=1), int(7, kind=1) /)
  iv = transfer(raw_bytes, iv)
  write(*,*) 'After setting raw bytes explicitly, integer interpreted as:', iv

  if (raw_bytes(1) == 21) then
    write(*,*) 'Detected ordering consistent with little-endian LSB-first.'
  else
    write(*,*) 'Detected ordering consistent with big-endian or different layout.'
  end if

contains

  subroutine print_hex(arr)
    integer(kind=1), intent(in) :: arr(:)
    integer :: k
    do k = 1, size(arr)
      write(*,'(1X,Z2)', advance='no') arr(k)
    end do
    write(*,*)
  end subroutine print_hex

end program litmus_common_endian