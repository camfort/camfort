program endian_demo
  use iso_c_binding, only: c_int32_t, c_double, c_int8_t
  implicit none(type, external)

  integer(c_int32_t) :: i32
  real(c_double)     :: r64
  integer :: ios, n
  character(len=16) :: fname = 'data_native.bin'
  integer(c_int8_t), allocatable :: buf(:)
  integer :: pos

  ! Prepare values with well-known bit patterns
  i32 = 123456789    ! 0x075BCD15
  r64 = 1.23456789012345d0

  ! Write in native (unformatted stream) binary. This will be written
  ! using the compiler/runtime's native endianness and record layout.
  open(unit=10, file=fname, access='stream', form='unformatted', status='replace', iostat=ios)
  if (ios /= 0) then
     write(*,*) 'Failed to open file for writing, IOSTAT=', ios
  else
    write(10) i32
    write(10) r64
    close(10)

    write(*,*) 'Wrote native-endian binary to "', trim(fname), '".'

    ! Now read back raw bytes and print them so output depends on endianness.
    open(unit=20, file=fname, access='stream', form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        write(*,*) 'Failed to open file for reading, IOSTAT=', ios
        return
    end if

    ! Determine file size
    inquire(unit=20, size=n)
    allocate(buf(n))
    read(20) buf

    write(*,*) 'Raw bytes (decimal) in file:'
    do pos = 1, n
      write(*,'(I3,1X)', advance='no') buf(pos)
      if (mod(pos,16) == 0) write(*,*)
    end do
    if (mod(n,16) /= 0) write(*,*)

    call interpret_and_print(buf)

    close(20)
    deallocate(buf)
  end if

contains

  subroutine interpret_and_print(buf)
    ! Interpret first 4 bytes as 32-bit int and next 8 bytes as 64-bit real
    integer(c_int8_t), intent(in) :: buf(:)
    integer(c_int32_t) :: i32_local
    real(c_double) :: r64_local
    integer(c_int8_t), dimension(4) :: i32_bytes
    integer(c_int8_t), dimension(8) :: r64_bytes
    integer :: k

    if (size(buf) < 12) then
      write(*,*) 'File too small to contain expected records.'
      return
    end if

    i32_bytes = buf(1:4)
    r64_bytes = buf(5:12)

    ! Move raw bytes into variables via TRANSFER - this preserves bit-patterns
    i32_local = transfer(i32_bytes, i32_local)
    r64_local = transfer(r64_bytes, r64_local)

    write(*,*) 'Interpreted as native 32-bit integer:', i32_local
    write(*,*) 'Interpreted as native 64-bit real   :', r64_local

    write(*,*) 'Byte order shown (hex) for clarity:'
    write(*,*) '  int32 bytes:'
    do k = 1, 4
      write(*,'(1X,Z2)', advance='no') i32_bytes(k)
    end do
    write(*,*)
    write(*,*) '  real64 bytes:'
    do k = 1, 8
      write(*,'(1X,Z2)', advance='no') r64_bytes(k)
    end do
    write(*,*)

    if (i32_local == 123456789) then
      write(*,*) 'Detected native endianness: matches expected little-endian ordering.'
    else
      write(*,*) 'Detected native endianness: ordering differs (likely big-endian).'
    end if

  end subroutine interpret_and_print

end program endian_demo
