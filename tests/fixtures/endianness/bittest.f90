program bittest
  implicit none
  integer(kind=4) :: val
  integer :: i
  
  ! Create a value where each byte is different
  ! On little-endian: bytes stored as [78, 56, 34, 12] in memory
  ! On big-endian:    bytes stored as [12, 34, 56, 78] in memory
  val = int(Z'12345678', kind=4)
  
  write(*,'(A,Z8.8)') 'Original value (hex): ', val
  write(*,*)
  
  ! BTEST - Test specific bits
  ! Bit 0 is LSB of the integer, but which BYTE that's in
  ! depends on endianness when viewing memory
  write(*,'(A)') '=== BTEST: Testing bits ==='
  write(*,'(A,L1)') 'Bit 0 (LSB of byte 0): ', btest(val, 0)   ! 0 (0x78 = 0111 1000)
  write(*,'(A,L1)') 'Bit 3 (in byte 0):     ', btest(val, 3)   ! 1
  write(*,'(A,L1)') 'Bit 8 (LSB of byte 1): ', btest(val, 8)   ! 0 (0x56 = 0101 0110)
  write(*,'(A,L1)') 'Bit 16 (LSB of byte 2):', btest(val, 16)  ! 0 (0x34 = 0011 0100)
  write(*,'(A,L1)') 'Bit 24 (LSB of byte 3):', btest(val, 24)  ! 0 (0x12 = 0001 0010)
  write(*,*)
  
  ! IBSET - Set a bit in a specific byte
  ! This modifies different physical memory locations on different endianness
  write(*,'(A)') '=== IBSET: Setting bits ==='
  write(*,'(A,Z8.8)') 'Set bit 0 (byte 0):  ', ibset(val, 0)   ! Changes 0x78 -> 0x79
  write(*,'(A,Z8.8)') 'Set bit 8 (byte 1):  ', ibset(val, 8)   ! Changes 0x56 -> 0x57
  write(*,'(A,Z8.8)') 'Set bit 16 (byte 2): ', ibset(val, 16)  ! Changes 0x34 -> 0x35
  write(*,'(A,Z8.8)') 'Set bit 24 (byte 3): ', ibset(val, 24)  ! Changes 0x12 -> 0x13
  write(*,*)
  
  ! IBCLR - Clear a bit in a specific byte
  write(*,'(A)') '=== IBCLR: Clearing bits ==='
  write(*,'(A,Z8.8)') 'Clear bit 3 (byte 0):  ', ibclr(val, 3)   ! Changes 0x78 -> 0x70
  write(*,'(A,Z8.8)') 'Clear bit 9 (byte 1):  ', ibclr(val, 9)   ! Changes 0x56 -> 0x54
  write(*,'(A,Z8.8)') 'Clear bit 20 (byte 2): ', ibclr(val, 20)  ! Changes 0x34 -> 0x24
  write(*,'(A,Z8.8)') 'Clear bit 28 (byte 3): ', ibclr(val, 28)  ! Changes 0x12 -> 0x02
  write(*,*)
  
  ! Demonstrate non-portable behavior: extract "bytes" via bit ops
  ! This gives same NUMERIC result but maps to different MEMORY locations
  write(*,'(A)') '=== Extracting bytes via bit operations ==='
  write(*,'(A)') '(Same numeric result, different memory locations by endianness)'
  do i = 0, 3
    write(*,'(A,I1,A,Z2.2)') 'Byte ', i, ' (bits ', i*8, '-', i*8+7, '): ', &
      iand(ishft(val, -i*8), int(Z'FF', kind=4))
  end do

end program bittest
