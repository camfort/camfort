program litmus_common_endian
  implicit none

  ! Define a COMMON block that holds 4 raw bytes (chunked view).
  integer(kind = 4) :: integer_bytes
  common /overlay_raw/ integer_bytes

  ! 0x075BCD15 = bytes 15, 205, 91, 7 in hex
  integer_bytes = 123456789
  write(*,*) 'Integer value set to:', integer_bytes

  call litmus_common_receiver()

end program litmus_common_endian