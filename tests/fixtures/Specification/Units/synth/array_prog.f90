program array_prog

    use array_spec

    !=unit(m**3) :: array1
    real :: array1(array_length) = (/1,2,3,4/)

    !=unit(K) :: array2
    real :: array2(array_length) = (/1,2,3,4/)

    real :: array3(array_length) = array1 * array2

end program
