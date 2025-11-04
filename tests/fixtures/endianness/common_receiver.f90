subroutine litmus_common_receiver()
  implicit none

  character(len=1)  :: char_bytes(4)
  common /overlay_raw/ char_bytes

  write(*,*) 'Bytes (decimal) in COMMON (chunked view):'
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

end subroutine