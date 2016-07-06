module ppm
  implicit none
contains
  subroutine output_ppm(u, img)
    integer, intent(in) :: u
    real, intent(in) :: img
    dimension img(100,100)
    integer :: i, j, v

    write(u, '(A2)') 'P6'
    write(u, '(I0,'' '',I0)') 100, 100
    write(u, '(A)') '255'

    do j=1, 100
       do i=1, 100
          v = int(255*(img(i,j)))

          write(u, '(3A1)', advance='no') v, v, v
       end do
    end do

  end subroutine output_ppm
end module ppm
